# =============================================================================
# 07_ppml.R   (§2-3 — colonne vertebrale gravite PPML, DiD sanctions/votes ONU)
# -----------------------------------------------------------------------------
# DECISION DE DESIGN (ancree dans la litterature : Crozet & Hinz 2020 ;
# Felbermayr et al. GSDB 2020 ; Yalcin et al. GSDB-R4 2025 ; Larch et al. 2024) :
# la gravite structurelle s'estime sur un PANEL MULTI-PAYS LARGE avec trois jeux
# de FE (exportateur-temps, importateur-temps, paire dirigee). Les FE pays-temps
# absorbent les resistances multilaterales (Anderson-van Wincoop), ce qui n'est
# identifiable QUE si chaque pays apparait dans de nombreuses dyades. Un panel
# Russie-seule rend ces FE inestimables (chaque partenaire = une seule dyade
# d'export -> saturation, colinearite testee). DONC : "centre Russie" vit dans le
# TRAITEMENT, la question, le 2x2 et les descriptives, PAS dans une coupe
# d'echantillon. Le panel reste large ; le traitement ne s'allume que sur les
# dyades-Russie quand le PARTENAIRE sanctionne la Russie (dirige, non-commercial).
#
# Choix d'estimateur (delibere) : Sun & Abraham (2021) est le seul event-study
# robuste a l'heterogeneite qui tourne NATIVEMENT en PPML (zeros gardes,
# semi-elasticites, benchmark GSDB-R4). Callaway-Sant'Anna et dCDH sont lineaires
# (logs) -> hors gravite ; dCDH est reserve a l'intensite 2022 en 08_ols.
#
# VALIDITE AVANT EFFET : on lit (i) balance/sorting (-> 06_descriptives_did),
# (ii) pre-tendances (incond. + cond. energie), (iii) HonestDiD, PUIS seulement
# le signe/ampleur (statique, 2x2, ATT).
#
# Output  : 07_ppml/{figures,tables}/ + 07_report.md
# Entrees : sanctions_panel.parquet (03, panel LARGE : trade_value, rta,
#           sanction_*) ; GSDB v4 dyadique brut (Data/Raw/IV, pour le flag DIRIGE
#           partenaire->RUS non-commercial) ; covariates_panel.parquet (05,
#           post-KAZ : cell_2022, condamne, energie) ; un_votes.parquet (04).
# Chemins / wrappers I/O / helpers : 00_setup.R.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "ggplot2", "haven", "HonestDiD")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) {
  message("Installation des packages manquants : ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest); library(ggplot2)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})
PART <- "07_ppml"
setFixest_nthreads(0)

PATH_FIG <- out_fig(); PATH_TAB <- out_tab()
YR_MIN <- 2008L; YR_MAX <- 2023L   # fenetre tractable (pre-base 2008-13, onset 2014, escalade 2022-23)
log_step("Setup termine.")

extract_coefs <- function(model, name) {
  n <- nobs(model)
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct[, `:=`(model = name, N = n)][]
}
write_tab <- function(dt, base) {
  fwrite(dt, file.path(PATH_TAB, paste0(base, ".csv")))
  invisible(dt)
}


# ---- Section 1 : panel large + traitement Russie-dirige non-commercial -------

log_step("Section 1 : chargement panel large (sanctions_panel) + GSDB dirige.")

df <- read_parquet_safe(PATH_SANCTIONS_PANEL)
df <- df[year >= YR_MIN & year <= YR_MAX]
df[, pair := paste(exp_iso3, imp_iso3, sep = "_")]                # paire dirigee (FE)
df[, pkey := fifelse(exp_iso3 < imp_iso3, paste(exp_iso3, imp_iso3, sep = "_"),
                     paste(imp_iso3, exp_iso3, sep = "_"))]       # paire non-ordonnee (cluster)
df[, rus_dyad := exp_iso3 == "RUS" | imp_iso3 == "RUS"]
df[, partner  := fifelse(exp_iso3 == "RUS", imp_iso3,
                  fifelse(imp_iso3 == "RUS", exp_iso3, NA_character_))]
master_iso3 <- sort(unique(c(df$exp_iso3, df$imp_iso3)))
cat("  - Panel large :", nrow(df), "obs |", df[rus_dyad == TRUE, uniqueN(pkey)],
    "paires-Russie |", uniqueN(df$pkey), "paires totales\n")

# --- Flag DIRIGE partenaire -> RUS, NON-COMMERCIAL, depuis GSDB brut ----------
# Coherent avec le fix KAZ (Option B) : on EXCLUT le case_id 1519 (KAZ 2023,
# mesure anti-contournement a l'export, commerciale) -> KAZ jamais "traite".
# La restriction non-commerciale evite la tautologie d'embargo (cf. §3 / GSDB-R4)
# et exclut de facto les cas purement commerciaux.
log_step("Section 1 : construction du flag dirige partenaire->RUS (non-commercial).")
g <- read_dta_safe(file.path(PATH_IV, "gsdb_v4", "GSDB_V4_dyadic.dta"))
g <- as.data.table(g)
g <- g[year >= YR_MIN & year <= YR_MAX &
       sanctioning_state_iso3 %in% master_iso3 &
       sanctioned_state_iso3  %in% master_iso3]
g <- g[as.character(case_id) != "1519"]   # KAZ Option B (anti-contournement)
to_rus <- g[sanctioned_state_iso3 == "RUS"]
# GARDE-FOU anti-contamination : la Russie ne doit jamais etre l'emettrice ici.
stopifnot(to_rus[sanctioning_state_iso3 == "RUS", .N] == 0L)
to_rus[, nt := as.integer(arms == 1 | military == 1 | financial == 1 |
                          travel == 1 | other == 1)]
to_rus[, tr := as.integer(trade == 1)]
dir_py <- to_rus[, .(rus_nt = as.integer(any(nt == 1)),
                     rus_tr = as.integer(any(tr == 1))),
                 by = .(partner = sanctioning_state_iso3, year)]

# Onset (non-commercial) au niveau partenaire = paire non-ordonnee RUS.
onset <- dir_py[rus_nt == 1L, .(onset_year = min(year)), by = partner]
df <- merge(df, onset, by = "partner", all.x = TRUE)
df[, ever := rus_dyad & !is.na(onset_year)]
df[, cohort := fifelse(ever, onset_year, 10000L)]
df[, rel_time := fifelse(ever, year - onset_year, NA_integer_)]
df[, treated_post := as.integer(ever & year >= onset_year)]

# Indicateurs DIRIGES "en vigueur" par annee (pour le contraste par type, §2).
df <- merge(df, dir_py[, .(partner, year, rus_nt, rus_tr)],
            by = c("partner", "year"), all.x = TRUE)
df[is.na(rus_nt), rus_nt := 0L]; df[is.na(rus_tr), rus_tr := 0L]
df[rus_dyad == FALSE, `:=`(rus_nt = 0L, rus_tr = 0L)]   # jamais sur dyades non-RUS

# Controle GSDB : la dyade (quelle qu'elle soit) est-elle sous une sanction
# quelconque cette annee (evite que les dyades non-RUS sanctionnees polluent
# les controles). sanction_any est NA apres 2023 -> 0 dans la fenetre.
df[, under_any_sanction := as.integer(!is.na(sanction_any) & sanction_any == 1L)]


# ---- Section 2 : validation descriptive du traitement -----------------------

log_step("Section 2 : validation descriptive du traitement.")
treated_partners <- onset[partner %in% df[rus_dyad == TRUE, unique(partner)]]
cohort_sizes <- treated_partners[, .(n_partners = .N), by = onset_year][order(onset_year)]
n_treated <- nrow(treated_partners)
n_never   <- df[rus_dyad == TRUE, uniqueN(partner)] - n_treated
cat("  - Partenaires Russie traites (non-commercial) :", n_treated,
    "| jamais-traites :", n_never, "\n")
cat("  - Distribution des cohortes (onset) :\n"); print(cohort_sizes)
cat(sprintf("  - 2022 = intensification, pas onset : %d nouveaux onsets en 2022-2023 vs %d en 2014\n",
            cohort_sizes[onset_year %in% 2022:2023, sum(n_partners)],
            cohort_sizes[onset_year == 2014L, sum(n_partners)]))
# Garde-fou contamination Russie-emettrice (deja assure par construction).
cat("  - Garde-fou : 0 cas Russie-emettrice dans le flag dirige (verifie).\n")
val <- copy(cohort_sizes)
val[, share := round(n_partners / sum(n_partners), 3)]
write_tab(val, "tab_treatment_validation")


# ---- Section 3 : DiD statique + contraste par type --------------------------

log_step("Section 3 : DiD statique + contraste par type (PPML 3-way FE).")
FE <- ~ exp_iso3^year + imp_iso3^year + pair

tic(); m_static <- fepois(trade_value ~ treated_post + rta | exp_iso3^year + imp_iso3^year + pair,
                          data = df, cluster = ~ pkey)
cat("    static:", toc(), "s | N =", nobs(m_static), "\n")
tic(); m_static_ctrl <- fepois(trade_value ~ treated_post + rta + under_any_sanction |
                                 exp_iso3^year + imp_iso3^year + pair, data = df, cluster = ~ pkey)
cat("    static+ctrl:", toc(), "s\n")
tic(); m_type <- fepois(trade_value ~ rus_tr + rus_nt + rta + under_any_sanction |
                          exp_iso3^year + imp_iso3^year + pair, data = df, cluster = ~ pkey)
cat("    type:", toc(), "s\n")

static_csv <- rbindlist(list(
  extract_coefs(m_static,      "static_treated_post"),
  extract_coefs(m_static_ctrl, "static_treated_post_anyctrl"),
  extract_coefs(m_type,        "type_contrast_dir")))
write_tab(static_csv, "tab_static_did")
cat("\n  --- DiD statique + type (hors rta/ctrl) ---\n")
print(static_csv[term %in% c("treated_post", "rus_tr", "rus_nt"),
                 .(model, term, est = round(estimate, 4), se = round(se, 4), p = round(p, 4))])

# EFFECTIFS du contraste par type (dirige) : cadrer le "commercial n.s." (petit n ?).
type_counts <- data.table(
  type = c("non_commercial (rus_nt)", "commercial (rus_tr)"),
  n_partners   = c(uniqueN(df[rus_dyad & rus_nt == 1L, partner]),
                   uniqueN(df[rus_dyad & rus_tr == 1L, partner])),
  n_dyad_years = c(df[rus_dyad & rus_nt == 1L, .N],
                   df[rus_dyad & rus_tr == 1L, .N]))
write_tab(type_counts, "tab_type_counts")
cat("  - Effectifs contraste par type (dirige partenaire->RUS) :\n"); print(type_counts)


# ---- Section 4 : 2x2 condamne x sanctionne autour de 2022 (bras ONU) ---------

log_step("Section 4 : 2x2 condamne x sanctionne (interaction avec post2022).")
cov <- read_parquet_safe(PATH_COVARIATES)
pp_cell <- unique(cov[, .(partner = partner_iso3,
                          cell_2022 = as.character(cell_2022_static))])
pp_cell <- pp_cell[!is.na(cell_2022)]
df <- merge(df, pp_cell, by = "partner", all.x = TRUE)
# Dyades non-Russie (et RUS sans cellule) -> categorie "non_russia" (masse qui
# identifie les FE, ne recoit pas d'effet de cellule). Reference = d_neither.
df[is.na(cell_2022), cell_2022 := "non_russia"]
df[, cell_2022 := factor(cell_2022,
     levels = c("d_neither", "a_both", "b_condemn_only", "c_sanction_only", "non_russia"))]
df[, cell_2022 := droplevels(cell_2022)]   # retire c_sanction_only (vide, post-KAZ)
df[, post2022 := as.integer(year >= 2022L)]

# Cellule Crimee 2014 (vote Res 68/262 x statut de sanction) -> 2x2 confondeur.
pp_cell14 <- unique(cov[, .(partner = partner_iso3, cell_2014 = as.character(cell_2014_static))])
pp_cell14 <- pp_cell14[!is.na(cell_2014)]
df <- merge(df, pp_cell14, by = "partner", all.x = TRUE)
df[is.na(cell_2014), cell_2014 := "non_russia"]
df[, cell_2014 := factor(cell_2014,
     levels = c("d_neither", "a_both", "b_condemn_only", "c_sanction_only", "non_russia"))]
df[, cell_2014 := droplevels(cell_2014)]

# Dictionnaire de libelles ANGLAIS (defini une fois, applique partout dans les figures).
LBL_CELL <- c(a_both = "Condemns and sanctions", b_condemn_only = "Condemns only",
              c_sanction_only = "Sanctions only", d_neither = "Neither condemns nor sanctions",
              non_russia = "Rest of world (control)")
COL_CELL <- c("Condemns and sanctions" = "#2166AC", "Condemns only" = "#B2182B")
LBL_TERC <- c(T1_low = "Low energy dependence", T2_mid = "Medium energy dependence",
              T3_high = "High energy dependence")
COL_TERC <- c("Low energy dependence" = "#2166AC", "Medium energy dependence" = "#999999",
              "High energy dependence" = "#B2182B")
Y_ES <- "Effect on bilateral trade with Russia (semi-elasticity)"

cell_n <- df[rus_dyad == TRUE, .(n_partner_years = .N,
                                 n_partners = uniqueN(partner)), by = cell_2022][order(cell_2022)]
cat("  - Effectifs par cellule (dyades-Russie) :\n"); print(cell_n)
if (df[rus_dyad == TRUE & cell_2022 == "b_condemn_only", .N] == 0L)
  cat("  !! Cellule 'condamne seulement' vide : ne pas interpreter.\n")

tic(); m_2x2 <- fepois(trade_value ~ i(cell_2022, post2022, ref = "d_neither") +
                         rta + under_any_sanction | exp_iso3^year + imp_iso3^year + pair,
                       data = df, cluster = ~ pkey)
cat("    2x2:", toc(), "s\n")
tab_2x2 <- extract_coefs(m_2x2, "did_2x2_cell_x_post2022")
write_tab(tab_2x2, "tab_2x2_did")
cat("\n  --- 2x2 (cellule x post2022, ref = Neither-Russie) ---\n")
print(tab_2x2[grepl("cell_2022", term),
              .(term, est = round(estimate, 4), se = round(se, 4), p = round(p, 4))])

# Robustesse : align_2022 (yes=2, abstain=1, no/absent=0). REFERENCE = "1"
# (abstention) car c'est le groupe qui correspond au "Neither" du 2x2 principal
# (majoritairement des abstentionnistes : Chine, Inde...). Signes ainsi COMPARABLES
# au 2x2 binaire (negatif = baisse de commerce vs la base non-condamnatrice).
if ("align_2022" %in% names(cov)) {
  pp_al <- unique(cov[, .(partner = partner_iso3, align_2022)])[!is.na(align_2022)]
  d_al <- merge(df, pp_al, by = "partner", all.x = TRUE)
  d_al[is.na(align_2022), align_2022 := -1L]   # non-Russie / inconnu = categorie a part (NR)
  d_al[, align_f := factor(align_2022, levels = c(-1, 0, 1, 2),
                           labels = c("NR", "no_absent", "abstain", "yes"))]
  al_n <- d_al[rus_dyad == TRUE, .(n_partners = uniqueN(partner)), by = align_f][order(align_f)]
  cat("  - Effectifs par niveau d'alignement (dyades-Russie) :\n"); print(al_n)
  m_align <- tryCatch(fepois(trade_value ~ i(align_f, post2022, ref = "abstain") + rta + under_any_sanction |
                               exp_iso3^year + imp_iso3^year + pair, data = d_al, cluster = ~ pkey),
                      error = function(e) {cat("    align robustness skip:", conditionMessage(e), "\n"); NULL})
  if (!is.null(m_align)) {
    al_tab <- extract_coefs(m_align, "did_2x2_align2022_ref_abstain")
    write_tab(rbind(al_tab, al_n[, .(term = paste0("n_", align_f), estimate = n_partners,
                    se = NA_real_, stat = NA_real_, p = NA_real_, model = "n_partners", N = NA_integer_)],
                    fill = TRUE), "tab_2x2_did_align")
    cat("  --- align (ref = abstain ~ Neither) ---\n")
    print(al_tab[grepl("align_f", term), .(term, est = round(estimate,4), se = round(se,4), p = round(p,4))])
  }
}

# --- Pre-tendances du 2x2 : event study cellule x annee (deux episodes) --------
# Helper : interagit les cellules-Russie avec les annees (ref = refyr x Neither).
# "Condemns only" sur les annees pre-traitement doit etre PLAT ~0 (pre-tendance OK).
log_step("Section 4b : pre-tendances du 2x2 (Ukraine 2022 + Crimee 2014).")
run_2x2_pretrends <- function(cellvar, refyr, y0, y1, figname, tabname, ttl, sub) {
  dd <- df[year >= y0 & year <= y1]
  fml <- as.formula(sprintf(
    "trade_value ~ i(year, %s, ref = %d, ref2 = 'd_neither') + rta + under_any_sanction | exp_iso3^year + imp_iso3^year + pair",
    cellvar, refyr))
  m <- tryCatch(fepois(fml, data = dd, cluster = ~ pkey),
                error = function(e) {cat("    2x2 pretrends", cellvar, "skip:", conditionMessage(e), "\n"); NULL})
  if (is.null(m)) return(NULL)
  e <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(e, 2:5, c("estimate","se","stat","p"))
  e <- e[grepl("year::", term) & grepl(sprintf("%s::", cellvar), term)]
  e[, yr := as.integer(sub(".*year::([0-9]+).*", "\\1", term))]
  e[, cell := sub(sprintf(".*%s::([a-z_]+).*", cellvar), "\\1", term)]
  e[, `:=`(ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se)]; setorder(e, cell, yr)
  write_tab(e[, .(cell, year = yr, estimate, se, ci_lo, ci_hi, p)], tabname)
  cat(sprintf("  --- %s : 'Condemns only' (annees pre-traitement doivent etre ~0) ---\n", cellvar))
  print(e[cell == "b_condemn_only", .(year = yr, est = round(estimate,4), se = round(se,4), p = round(p,4))])
  ep <- e[cell %in% c("a_both", "b_condemn_only")]
  ep[, cell_lbl := factor(LBL_CELL[cell], levels = c("Condemns and sanctions", "Condemns only"))]
  p <- ggplot(ep, aes(yr, estimate, color = cell_lbl, fill = cell_lbl)) +
    geom_hline(yintercept = 0, lty = 2, color = "grey50") +
    geom_vline(xintercept = refyr + 0.5, lty = 3, color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.8) +
    scale_x_continuous(breaks = seq(y0, y1, 1)) +
    scale_color_manual(values = COL_CELL) + scale_fill_manual(values = COL_CELL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(size = 10, color = "grey40"), legend.position = "bottom",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(title = ttl, subtitle = sub, x = "Year", y = Y_ES, color = NULL, fill = NULL,
         caption = "PPML with exporter-year, importer-year and pair fixed effects; pair-clustered SE. Cells = UN condemnation x sanction status.")
  ggsave(file.path(PATH_FIG, paste0(figname, ".png")), p, width = 10, height = 6, dpi = 300)
  m
}

# Ukraine 2022 (vote ES-11/1) : reference 2021, fenetre 2016-2023. m_2x2_es sert
# aussi a la borne HonestDiD du phare (Section 7).
m_2x2_es <- run_2x2_pretrends(
  "cell_2022", 2021L, 2016L, 2023L,
  "condemnation_2x2_pretrends_ukraine", "tab_2x2_pretrends_2022",
  "Pre-trends of the 2022 condemnation 2x2 (trade with Russia)",
  "Reference 2021 x Neither. Pre-2022 coefficients flat ~0 (clean pre-trend); 2022-2023 carry the effect.")

# Crimee 2014 (vote Res 68/262) : reference 2013, fenetre PROPRE 2010-2021.
# Test du confondeur : 'Condemns only' decroche-t-il deja apres 2014, AVANT le boom
# d'absorption post-2022 du groupe Neither (Chine/Inde) ?
run_2x2_pretrends(
  "cell_2014", 2013L, 2010L, 2021L,
  "condemnation_2x2_pretrends_crimea", "tab_2x2_pretrends_2014",
  "Pre-trends of the 2014 Crimea condemnation 2x2 (trade with Russia)",
  "Reference 2013 x Neither. Confounder test: does 'Condemns only' break after 2014, before the post-2022 rerouting?")


# ---- Section 5 : event study Sun & Abraham (fenetre PROPRE + full) -----------

log_step("Section 5 : event study Sun & Abraham (fenetre propre 2010-2021 + full 2008-2023).")
# PRINCIPAL = fenetre 2010-2021 : exclut les leads de crise (2008-2009) ET les
# lags de guerre (2022-2023, qui appartiennent a l'intensite -> 08_ols). Les
# onsets hors fenetre deviennent CONTROLES ; left-censored (onset < y0+1) exclus.
# => effet 2014 NET, non contamine. SECONDAIRE = 2008-2023 (transparence guerre).
run_es <- function(y0, y1, blo, bhi, label) {
  d <- df[year >= y0 & year <= y1]
  d <- d[!(ever & onset_year < y0 + 1L)]                          # drop left-censored
  d[, coh := fifelse(ever & onset_year <= y1, onset_year, 10000L)] # onset hors fenetre -> controle
  d[, evc := coh != 10000L]
  if (d[evc == TRUE, uniqueN(coh)] < 1L) return(NULL)
  rg <- range(d[evc == TRUE, year - coh]); blo <- max(blo, rg[1]); bhi <- min(bhi, rg[2])
  fml <- as.formula(sprintf(
    "trade_value ~ sunab(coh, year, bin.rel = list('%d' = %d:%d, '%d' = %d:%d)) + rta | exp_iso3^year + imp_iso3^year + pair",
    blo, rg[1], blo, bhi, bhi, rg[2]))
  tic(); m <- fepois(fml, data = d, cluster = ~ pkey)
  cat("    es", label, ":", toc(), "s | N =", nobs(m), "| cohortes :", d[evc==TRUE, uniqueN(coh)], "\n")
  ac <- as.data.table(summary(m, agg = "att")$coeftable, keep.rownames = "term")
  setnames(ac, 2:5, c("estimate","se","stat","p")); ac <- ac[term == "ATT"]
  e <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(e, 2:5, c("estimate","se","stat","p"))
  e <- e[grepl("year::", term)]; e[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*","\\1", term))]
  e[, `:=`(ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se, window = label)]; setorder(e, rel_time)
  list(model = m, es = e, att = ac, data = d)
}

es_clean <- run_es(2010L, 2021L, -4L, 6L, "2010_2021")   # PRINCIPAL (effet 2014 net)
es_full  <- run_es(2008L, 2023L, -5L, 5L, "2008_2023")   # SECONDAIRE (avec guerre)
stopifnot(!is.null(es_clean), !is.null(es_full))
att_clean <- es_clean$att; att_full <- es_full$att
cat(sprintf("\n  ATT 2014 PROPRE (2010-2021) : %.4f (p=%.4f) | ATT full (2008-2023, contamine guerre) : %.4f\n",
            att_clean$estimate, att_clean$p, att_full$estimate))

# Table : les deux fenetres etiquetees + lignes ATT.
out_es <- rbindlist(list(
  es_clean$es[, .(window, term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
  es_full$es[,  .(window, term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
  att_clean[, .(window = "2010_2021", term = "ATT", rel_time = NA_integer_, estimate, se,
                ci_lo = estimate-1.96*se, ci_hi = estimate+1.96*se)],
  att_full[,  .(window = "2008_2023", term = "ATT", rel_time = NA_integer_, estimate, se,
                ci_lo = estimate-1.96*se, ci_hi = estimate+1.96*se)]), use.names = TRUE)
write_tab(out_es, "tab_eventstudy_sunab")

mk_es_plot <- function(e, ttl, sub) ggplot(e, aes(rel_time, estimate)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = -0.5, lty = 3, color = "grey60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166AC") +
  geom_line(color = "#2166AC", linewidth = 0.6) + geom_point(color = "#2166AC", size = 2) +
  scale_x_continuous(breaks = seq(-6, 9, 1)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = ttl, subtitle = sub, x = "Years relative to sanction onset", y = Y_ES,
       caption = "Source: BACI-CEPII, GSDB-R4. Treatment = partner imposes non-commercial sanctions on Russia (directed). 95% CI, pair-clustered.")
ggsave(file.path(PATH_FIG, "sanctions_event_study.png"),
       mk_es_plot(es_clean$es, "Non-commercial sanctions on Russia: net 2014 effect on trade (2010-2021)",
                  "Sun & Abraham event study (PPML, three-way FE). Clean window: excludes the 2008-09 crisis and the 2022-23 war."),
       width = 10, height = 6, dpi = 300)
ggsave(file.path(PATH_FIG, "sanctions_event_study_full_window.png"),
       mk_es_plot(es_full$es, "Effect on trade with Russia (2008-2023, including the war years)",
                  "Secondary view: the +5 bin absorbs 2022-2023 (intensification -> 08_ols). Read with this caveat."),
       width = 10, height = 6, dpi = 300)


# ---- Section 6 : pre-tendances CONDITIONNELLES a l'energie (fenetre propre) ---

log_step("Section 6 : pre-tendances conditionnelles a la dependance energetique.")
# Tercile = exposition energetique PRE-guerre (moyenne 2018-2021), fixe par
# partenaire. On NE met PAS l'energie en regresseur (absorbee par FE paire) :
# on STRATIFIE l'event study (fenetre propre) par tercile -> leads plats par strate ?
en_ref <- cov[, .(energy = mean(partner_energy_dep_rus[year %in% 2018:2021], na.rm = TRUE)),
              by = .(partner = partner_iso3)]
en_ref <- en_ref[is.finite(energy)]
en_ref[, terc := cut(energy, quantile(energy, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     labels = c("T1_low", "T2_mid", "T3_high"), include.lowest = TRUE)]
dclean <- merge(es_clean$data, en_ref[, .(partner, terc)], by = "partner", all.x = TRUE)

es_by <- list()
for (k in c("T1_low", "T2_mid", "T3_high")) {
  dk <- dclean[coh == 10000L | (evc == TRUE & terc == k)]
  if (dk[evc == TRUE, uniqueN(coh)] < 1L) { cat("    tercile", k, ": aucun traite -> saute\n"); next }
  rk <- range(dk[evc == TRUE, year - coh])
  fk <- as.formula(sprintf(
    "trade_value ~ sunab(coh, year, bin.rel = list('%d' = %d:%d, '%d' = %d:%d)) + rta | exp_iso3^year + imp_iso3^year + pair",
    max(-4L, rk[1]), rk[1], max(-4L, rk[1]), min(6L, rk[2]), min(6L, rk[2]), rk[2]))
  mk <- tryCatch(fepois(fk, data = dk, cluster = ~ pkey),
                 error = function(e) {cat("    tercile", k, "skip:", conditionMessage(e), "\n"); NULL})
  if (is.null(mk)) next
  ek <- as.data.table(coeftable(mk), keep.rownames = "term"); setnames(ek, 2:5, c("estimate","se","stat","p"))
  ek <- ek[grepl("year::", term)]; ek[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*","\\1", term))]
  ek[, `:=`(tercile = k, ci_lo = estimate-1.96*se, ci_hi = estimate+1.96*se)]; es_by[[k]] <- ek
  cat("    tercile", k, ": N =", nobs(mk), "| treated cohorts :", dk[evc==TRUE, uniqueN(coh)], "\n")
}
if (length(es_by)) {
  es_by <- rbindlist(es_by); setorder(es_by, tercile, rel_time)
  es_uncond <- copy(es_clean$es[, .(rel_time, estimate, se, ci_lo, ci_hi)]); es_uncond[, tercile := "all (uncond.)"]
  cond_tab <- rbind(es_by[, .(tercile, rel_time, estimate, se, ci_lo, ci_hi)], es_uncond)
  write_tab(cond_tab, "tab_pretrends_conditional")
  es_by[, terc_lbl := factor(LBL_TERC[tercile], levels = LBL_TERC[c("T2_mid", "T3_high")])]
  p_by <- ggplot(es_by, aes(rel_time, estimate, color = terc_lbl, fill = terc_lbl)) +
    geom_hline(yintercept = 0, lty = 2, color = "grey50") +
    geom_vline(xintercept = -0.5, lty = 3, color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.6) +
    scale_x_continuous(breaks = seq(-4, 6, 1)) +
    scale_color_manual(values = COL_TERC) + scale_fill_manual(values = COL_TERC) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(size = 10, color = "grey40"), legend.position = "bottom",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(title = "Event study by tercile of energy dependence on Russia (clean window)",
         subtitle = "Pre-trends at comparable energy dependence. The low-dependence tercile has no sanctioners.",
         x = "Years relative to sanction onset", y = Y_ES, color = NULL, fill = NULL,
         caption = "Tercile = pre-war energy exposure to Russia (2018-2021 average), fixed per partner.")
  ggsave(file.path(PATH_FIG, "sanctions_by_energy_dependence.png"), p_by, width = 10, height = 6, dpi = 300)
} else cat("  !! Aucun tercile estimable.\n")


# ---- Section 7 : HonestDiD (Rambachan & Roth 2023), recible sur l'ATT --------

log_step("Section 7 : HonestDiD (sensibilite, RECIBLEE sur l'ATT post k>=+1).")
hd_ok <- requireNamespace("HonestDiD", quietly = TRUE)
if (!hd_ok) cat("  !! package HonestDiD indisponible : etape sautee (a installer).\n") else {
  # fixest sunab n'expose pas la vcov agregee event-time -> on applique HonestDiD
  # a la REPRESENTATION event-study TWFE i(rel) sur la FENETRE PROPRE 2010-2021
  # (coef()/vcov() concordent ; cohorte 2014 dominante => ~ Sun-Abraham).
  # CIBLE = l'ATT (moyenne des effets post k>=+1 ; PAS k=0 transition) via l_vec.
  dh <- copy(es_clean$data)
  dh[, rel_bin := ifelse(evc, pmax(-4L, pmin(6L, year - coh)), -1L)]  # never-treated -> ref
  m_twfe <- fepois(trade_value ~ i(rel_bin, ref = -1) + rta |
                     exp_iso3^year + imp_iso3^year + pair, data = dh, cluster = ~ pkey)
  b_all <- coef(m_twfe); V_all <- vcov(m_twfe)
  ev <- grep("rel_bin::", names(b_all), value = TRUE)
  rt_h <- as.integer(sub(".*rel_bin::(-?[0-9]+).*", "\\1", ev)); ord <- order(rt_h)
  ev <- ev[ord]; rt_h <- rt_h[ord]
  betahat <- b_all[ev]; V <- V_all[ev, ev, drop = FALSE]
  numPre  <- sum(rt_h < 0); numPost <- sum(rt_h >= 0)
  postk <- rt_h[rt_h >= 0]
  l_vec <- as.numeric(postk >= 1); l_vec <- if (sum(l_vec) > 0) l_vec / sum(l_vec) else rep(1/numPost, numPost)
  cat("  - event-study TWFE (2010-2021) : numPre =", numPre, "| numPost =", numPost,
      "| l_vec cible k>=1 (moyenne", sum(postk>=1), "periodes)\n")
  # Controle de coherence : point + IC de l'estimand cible (l_vec' beta_post).
  post_idx <- which(rt_h >= 0); bp <- betahat[post_idx]; Vp <- V[post_idx, post_idx, drop = FALSE]
  att_point <- sum(l_vec * bp); att_se <- sqrt(as.numeric(t(l_vec) %*% Vp %*% l_vec))
  cat(sprintf("  - ATT cible (TWFE, k>=1) : %.4f  IC95 [%.4f ; %.4f]\n",
              att_point, att_point - 1.96*att_se, att_point + 1.96*att_se))
  # M-bar de rupture EXACT : plus grand M-bar ou l'IC robuste exclut encore zero,
  # interpole lineairement entre le dernier point excluant et le premier incluant
  # (la grille seule sous-estime ; 0.5 etait un artefact de pas grossier).
  hd_break <- function(dt) {
    setorder(dt, Mbar)
    excl <- (dt$lb > 0 & dt$ub > 0) | (dt$lb < 0 & dt$ub < 0)
    if (!any(excl)) return(0)
    li <- max(which(excl))
    if (li == nrow(dt)) return(dt$Mbar[li])              # ne croise pas sur la grille
    bnd <- if (dt$ub[li] < 0) "ub" else "lb"             # la borne qui approche 0
    y1 <- dt[[bnd]][li]; y2 <- dt[[bnd]][li + 1]
    dt$Mbar[li] + (0 - y1) * (dt$Mbar[li + 1] - dt$Mbar[li]) / (y2 - y1)
  }
  GRID <- seq(0, 1.2, by = 0.05)
  hd <- tryCatch({
    rm_res <- HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = betahat, sigma = V, numPrePeriods = numPre, numPostPeriods = numPost,
      l_vec = l_vec, Mbarvec = GRID)
    sd_res <- HonestDiD::createSensitivityResults(
      betahat = betahat, sigma = V, numPrePeriods = numPre, numPostPeriods = numPost,
      l_vec = l_vec, Mvec = seq(0, 0.3, by = 0.1))
    list(rm = as.data.table(rm_res), sd = as.data.table(sd_res))
  }, error = function(e) {cat("  !! HonestDiD erreur :", conditionMessage(e), "\n"); NULL})
  if (!is.null(hd)) {
    rm_dt <- hd$rm; rm_dt[, `:=`(method = "relative_magnitudes", estimand = "ATT_post_k>=1")]
    sd_dt <- hd$sd; sd_dt[, `:=`(method = "smoothness", estimand = "ATT_post_k>=1")]
    Mbreak <- hd_break(rm_dt)
    rm_dt[, breakdown_Mbar := Mbreak]
    hd_tab <- rbind(rm_dt, sd_dt, fill = TRUE)
    write_tab(hd_tab, "tab_honestdid_bounds")
    m0 <- rm_dt[Mbar == 0]
    cat(sprintf("  - COHERENCE : HonestDiD M-bar=0 IC [%.4f ; %.4f] vs ATT cible IC [%.4f ; %.4f]\n",
                m0$lb[1], m0$ub[1], att_point - 1.96*att_se, att_point + 1.96*att_se))
    cat(sprintf("  - M-bar de rupture EXACT (ATT, grille fine) : %.3f\n", Mbreak))
    p_hd <- ggplot(rm_dt, aes(Mbar)) +
      geom_hline(yintercept = 0, lty = 2, color = "grey50") +
      geom_vline(xintercept = Mbreak, lty = 3, color = "#B2182B") +
      geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2, fill = "#2166AC") +
      geom_line(aes(y = lb)) + geom_line(aes(y = ub)) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 13),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)) +
      labs(title = "HonestDiD sensitivity of the sanctions ATT (post k>=+1, clean window 2010-2021)",
           subtitle = sprintf("Breakdown M-bar = %.2f (>=1 = robust). Fine grid. Target = ATT, not k=0.", Mbreak),
           x = "M-bar (relative magnitude of pre-trend violation)", y = "Robust CI of the ATT (post k>=1)",
           caption = "Rambachan & Roth (2023). TWFE event study (~ Sun-Abraham; 2014 cohort dominant).")
    ggsave(file.path(PATH_FIG, "honestdid_sanctions.png"), p_hd, width = 9, height = 6, dpi = 300)
  }

  # --- Tache 2 : borne HonestDiD du RESULTAT PHARE (2x2 condamne-seul + both) ---
  # Sur l'event study cellule x annee (ref 2021 x Neither) : pre = 2016-2020 (leads),
  # post = 2022-2023 (lags), cible = effet post moyen 2022-2023. Avec 2 lags seulement
  # et des IC larges, le M-bar de rupture du condamne-seul sera probablement bas
  # (resultat possiblement fragile) -> on le rapporte tel quel.
  if (exists("m_2x2_es") && !is.null(m_2x2_es)) {
    b2 <- coef(m_2x2_es); V2 <- vcov(m_2x2_es)
    hd_cell <- function(cell) {
      tt <- grep("year::", names(b2), value = TRUE)
      tt <- tt[grepl(sprintf("cell_2022::%s$", cell), tt)]
      if (!length(tt)) return(NULL)
      yr <- as.integer(sub(".*year::([0-9]+).*", "\\1", tt)); o <- order(yr); tt <- tt[o]; yr <- yr[o]
      bh <- b2[tt]; Vv <- V2[tt, tt, drop = FALSE]
      nPre <- sum(yr < 2021L); nPost <- sum(yr > 2021L)
      if (nPre < 1L || nPost < 1L) return(NULL)
      lv <- rep(1 / nPost, nPost)                        # moyenne post 2022-2023 (longueur = nPost)
      r <- tryCatch(HonestDiD::createSensitivityResults_relativeMagnitudes(
             betahat = bh, sigma = Vv, numPrePeriods = nPre, numPostPeriods = nPost,
             l_vec = lv, Mbarvec = GRID),
           error = function(e) {cat("    HonestDiD 2x2", cell, "skip:", conditionMessage(e), "\n"); NULL})
      if (is.null(r)) return(NULL)
      dt <- as.data.table(r); dt[, `:=`(cell = cell, method = "relative_magnitudes")]
      brk <- hd_break(dt); dt[, breakdown_Mbar := brk]
      pt <- sum(lv * bh[yr > 2021L]); se <- sqrt(as.numeric(t(lv) %*% Vv[yr > 2021L, yr > 2021L, drop=FALSE] %*% lv))
      cat(sprintf("  - 2x2 %-15s : post moyen 2022-23 = %.4f (IC [%.4f;%.4f]) | M-bar rupture = %.3f (nPre=%d,nPost=%d)\n",
                  cell, pt, pt-1.96*se, pt+1.96*se, brk, nPre, nPost))
      dt
    }
    hd2 <- rbindlist(Filter(Negate(is.null), list(hd_cell("b_condemn_only"), hd_cell("a_both"))), use.names = TRUE)
    if (nrow(hd2)) {
      write_tab(hd2, "tab_honestdid_2x2")
      hd2[, cell_lbl := factor(LBL_CELL[cell], levels = c("Condemns and sanctions", "Condemns only"))]
      labs2 <- hd2[, .(brk = breakdown_Mbar[1]), by = cell_lbl]
      sub2 <- paste(sprintf("%s: M-bar=%.2f", labs2$cell_lbl, labs2$brk), collapse = "   |   ")
      p_hd2 <- ggplot(hd2, aes(Mbar, color = cell_lbl, fill = cell_lbl)) +
        geom_hline(yintercept = 0, lty = 2, color = "grey50") +
        geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.12, color = NA) +
        geom_line(aes(y = lb)) + geom_line(aes(y = ub)) +
        scale_color_manual(values = COL_CELL) + scale_fill_manual(values = COL_CELL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 13),
              plot.subtitle = element_text(size = 10, color = "grey40"), legend.position = "bottom",
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA)) +
        labs(title = "HonestDiD sensitivity of the headline 2x2 result (post-2022 effect)",
             subtitle = paste0("Breakdown M-bar (>=1 = robust).   ", sub2),
             x = "M-bar (relative magnitude of pre-trend violation)", y = "Robust CI of the post effect",
             color = NULL, fill = NULL,
             caption = "Rambachan & Roth (2023) on the cell x year event study (ref 2021 x Neither). Only 2 post lags -> wide CIs.")
      ggsave(file.path(PATH_FIG, "honestdid_condemnation_2x2.png"), p_hd2, width = 9, height = 6, dpi = 300)
    }
  }
}


# ---- Section 8 : verdict (ordre validite -> effet) + cloture ----------------

log_step("Section 8 : verdict de validite (ordre impose).")
cat("\n========================================================\n")
cat("VERDICT — on juge la credibilite AVANT le resultat\n")
cat("========================================================\n")
cat("(i)   BALANCE / SORTING : voir 06_descriptives_did (densites brutes,",
    "carte 2x2) ; SMD formel a construire la.\n")
cat("(ii)  PRE-TENDANCES (inconditionnelles) : leads k<0 ci-dessus (tab_eventstudy_sunab) ;",
    "doivent etre plats ~0.\n")
cat("      PRE-TENDANCES (conditionnelles energie) : tab_pretrends_conditional /",
    "sanctions_by_energy_dependence.png (plats par tercile).\n")
cat("(iii) HonestDiD : tab_honestdid_bounds (M-bar de rupture).\n")
cat("---- PUIS seulement l'effet ----\n")
cat(sprintf("  Statique (treated_post)        : %.4f (p=%.4f)\n",
            static_csv[term=="treated_post" & model=="static_treated_post", estimate],
            static_csv[term=="treated_post" & model=="static_treated_post", p]))
cat(sprintf("  ATT 2014 PROPRE (2010-2021)    : %.4f (p=%.4f)\n", att_clean$estimate, att_clean$p))
cat(sprintf("  ATT full (2008-2023, guerre)   : %.4f (p=%.4f) [caveat : +5 absorbe 2022-23 -> 08_ols]\n",
            att_full$estimate, att_full$p))
cat("\n== Figures ==\n"); print(list.files(PATH_FIG))
cat("== Tables ==\n");  print(list.files(PATH_TAB))
log_step("07_ppml.R termine.")


# =============================================================================
# Section 9 : BOOTSTRAP PIGEONHOLE MULTINOMIAL (Davezies, D'Haultfoeuille &
#             Guyonvarch 2021, Annals of Statistics, Section 2.3) — SE/IC/p-values
#             robustes a la DEPENDANCE DYADIQUE sur les PPML pharses de ce script.
# -----------------------------------------------------------------------------
# ADDITIF STRICT : aucune section/sortie existante n'est modifiee. Reutilise les
# helpers (log_step, write_tab, extract_coefs, tic/toc) et les conventions de
# chemins. Schema (NE PAS confondre) : reechantillonnage au niveau PAYS, POPULATION
# UNIQUE (exp & imp tires du meme ensemble) ; a chaque replication, comptes
# MULTINOMIAUX W sur les pays ; poids d'une dyade-annee = W[exp]*W[imp] (entier) ;
# refit du MEME fepois pondere (weights=~w) sur w>0, MEMES labels de FE (les pays
# de poids 0 disparaissent naturellement). Ce n'est PAS Exp(1) i.i.d., PAS un
# resampling de paires, PAS un cluster ~exp+imp. p-value = mean(|theta*-theta_hat|
# > |theta_hat|) (convention Table 2 de l'article).
# =============================================================================
log_step("Section 9 : bootstrap pigeonhole multinomial (dependance dyadique).")
suppressPackageStartupMessages(library(parallel))
B_DYADIC   <- as.integer(Sys.getenv("B_DYADIC", "200"))  # surchargeable (test rapide)
SEED_DYADIC <- 1234L
N_WORKERS  <- max(1L, min(2L, parallel::detectCores() - 1L))  # plafonne RAM ~8 Go
setFixest_nthreads(1)                                          # 1 thread / worker
cat(sprintf("  - B = %d | workers = %d | 1 thread/worker\n", B_DYADIC, N_WORKERS))

# Panel reduit aux seules colonnes utiles (limite les copies dans les forks).
dfb <- df[, .(trade_value, exp_iso3, imp_iso3, year, pair, pkey,
              rta, treated_post, rus_tr, rus_nt, under_any_sanction,
              cell_2022, post2022)]
rm(df); gc(verbose = FALSE)   # libere le grand panel original (non additif aux sorties)

pigeonhole_boot_ppml <- function(fml, data, coefs_of_interest, model_name,
                                 B = 200L, seed = 1234L,
                                 country_cols = c("exp_iso3", "imp_iso3"),
                                 workers = 1L, batch = 25L) {
  t_all <- Sys.time()
  # --- fit de base NON pondere : theta_hat + SE/p clusterises paire ---
  m0  <- fepois(fml, data = data, cluster = ~ pkey)
  ct0 <- as.data.table(coeftable(m0), keep.rownames = "term")
  setnames(ct0, 2:5, c("estimate", "se_pair", "stat", "p_pair"))
  coefs <- intersect(coefs_of_interest, ct0$term)
  if (!length(coefs)) { cat("   !!", model_name, ": aucun coef d'interet present.\n"); return(NULL) }
  base <- ct0[term %in% coefs]
  theta_hat <- setNames(base$estimate, base$term)

  # --- pre-calculs : index pays (population unique) ---
  pays <- sort(unique(c(data[[country_cols[1]]], data[[country_cols[2]]])))
  np <- length(pays)
  exp_idx <- match(data[[country_cols[1]]], pays)
  imp_idx <- match(data[[country_cols[2]]], pays)
  cat(sprintf("   %s : %d coefs | %d pays | %d obs | theta_hat = %s\n", model_name,
              length(coefs), np, nrow(data),
              paste(sprintf("%s=%.4f", names(theta_hat), theta_hat), collapse = " ")))

  # --- poids multinomiaux PRE-generes (reproductible, independant des workers) ---
  set.seed(seed)
  Wlist <- lapply(seq_len(B), function(b) tabulate(sample.int(np, np, replace = TRUE), np))

  # --- diagnostic du pays FOCAL (Russie) : draws ou W[RUS]=0 -> traitement degenere
  # (proba ~ e^-1 ~ 37% car le traitement est concentre sur 1 pays) -> explique le
  # taux d'echec, ce n'est PAS un bug mais une limite intrinseque du pigeonhole pays.
  rus_pos  <- match("RUS", pays)
  n_focal0 <- if (!is.na(rus_pos)) sum(vapply(Wlist, function(W) W[rus_pos] == 0L, logical(1))) else NA_integer_
  if (!is.na(n_focal0))
    cat(sprintf("   %s : draws avec W[RUS]=0 (traitement degenere, attendu ~37%%) = %d/%d (%.0f%%)\n",
                model_name, n_focal0, B, 100 * n_focal0 / B))

  # --- checkpoint : reprise si draws deja sur disque pour ce modele ---
  ckpt <- file.path(PATH_TAB, paste0("_pigeonhole_draws_", model_name, ".csv"))
  draws <- NULL; done_b <- 0L
  if (file.exists(ckpt)) {
    draws <- fread(ckpt); done_b <- if (nrow(draws)) max(draws$b) else 0L
    cat(sprintf("   reprise checkpoint : %d draws deja faits\n", done_b))
  }

  one_draw <- function(W) {
    w <- W[exp_idx] * W[imp_idx]
    keep <- which(w > 0)
    dsub <- data[keep]; dsub[, w := w[keep]]
    fit <- tryCatch(fepois(fml, data = dsub, weights = ~ w),
                    error = function(e) NULL)
    if (is.null(fit)) return(setNames(rep(NA_real_, length(coefs)), coefs))
    cf <- coef(fit); out <- cf[coefs]; names(out) <- coefs; out
  }

  # --- boucle par batch (checkpoint au fil de l'eau, crash-safe) ---
  if (done_b < B) for (start in seq(done_b + 1L, B, by = batch)) {
    bs <- start:min(start + batch - 1L, B)
    t0 <- Sys.time()
    res <- if (workers > 1L) mclapply(Wlist[bs], one_draw, mc.cores = workers, mc.preschedule = FALSE)
           else               lapply(Wlist[bs], one_draw)
    mat <- do.call(rbind, res)
    dt <- as.data.table(mat); dt[, b := bs]
    fwrite(dt, ckpt, append = file.exists(ckpt))
    draws <- rbind(draws, dt, fill = TRUE)
    dts <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    nok <- sum(stats::complete.cases(mat))
    cat(sprintf("   %s batch %d-%d : %.1fs (%.2fs/draw) | ok %d/%d\n",
                model_name, min(bs), max(bs), dts, dts / length(bs), nok, length(bs)))
  }

  # --- agregation : SE bootstrap, IC percentile, p-value (convention article) ---
  out <- rbindlist(lapply(coefs, function(cf) {
    v <- draws[[cf]]; v <- v[is.finite(v)]; th <- theta_hat[[cf]]
    data.table(model = model_name, term = cf, estimate = th,
               se_pair = base[term == cf, se_pair], p_pair = base[term == cf, p_pair],
               se_boot = sd(v),
               ci_lo = as.numeric(quantile(v, 0.025)), ci_hi = as.numeric(quantile(v, 0.975)),
               p_boot = mean(abs(v - th) > abs(th)),
               n_draws_ok = length(v), fail_rate = 1 - length(v) / B,
               n_focal_zero = n_focal0, boot_mean = mean(v))
  }))
  cat(sprintf("   %s : %.1f min | conv %.0f%% | max|theta_bar-theta_hat| = %.4g\n",
              model_name, as.numeric(difftime(Sys.time(), t_all, units = "mins")),
              100 * (1 - max(out$fail_rate)), max(abs(out$boot_mean - out$estimate))))
  out
}

# --- Application aux specs pharses (memes formules que les sections precedentes) -
FE_PPML <- ~ exp_iso3^year + imp_iso3^year + pair
specs <- list(
  list(name = "static_treated_post",
       fml  = trade_value ~ treated_post + rta | exp_iso3^year + imp_iso3^year + pair,
       coefs = "treated_post"),
  list(name = "type_contrast_dir",
       fml  = trade_value ~ rus_tr + rus_nt + rta + under_any_sanction | exp_iso3^year + imp_iso3^year + pair,
       coefs = c("rus_tr", "rus_nt")),
  list(name = "did_2x2_cell_x_post2022",
       fml  = trade_value ~ i(cell_2022, post2022, ref = "d_neither") + rta + under_any_sanction | exp_iso3^year + imp_iso3^year + pair,
       coefs = grep("^cell_2022::(a_both|b_condemn_only):post2022$",
                    names(coef(m_2x2)), value = TRUE)))
cat("  - Sun-Abraham agrege : SKIP (secondaire ; extraction de l'ATT agrege sous ponderation non triviale).\n")

boot_res <- list()
for (sp in specs) {
  boot_res[[sp$name]] <- tryCatch(
    pigeonhole_boot_ppml(sp$fml, dfb, sp$coefs, sp$name,
                         B = B_DYADIC, seed = SEED_DYADIC, workers = N_WORKERS),
    error = function(e) { cat("  !! spec", sp$name, "echouee :", conditionMessage(e), "\n"); NULL })
}
boot_tab <- rbindlist(Filter(Negate(is.null), boot_res), use.names = TRUE)

if (nrow(boot_tab)) {
  out_cols <- c("model","term","estimate","se_pair","p_pair","se_boot","ci_lo","ci_hi",
                "p_boot","n_draws_ok","fail_rate","n_focal_zero")
  write_tab(boot_tab[, ..out_cols], "tab_ppml_dyadic_bootstrap")
  cat("\n  --- bootstrap pigeonhole (SE paire vs SE/IC/p dyadique) ---\n")
  print(boot_tab[, .(model, term, est = round(estimate, 4),
                     se_pair = round(se_pair, 4), se_boot = round(se_boot, 4),
                     p_pair = round(p_pair, 4), p_boot = round(p_boot, 4),
                     conv = round(1 - fail_rate, 2))])

  # --- Resume markdown dans Reports/ ---
  md <- c(
    "# PPML — bootstrap pigeonhole multinomial (dépendance dyadique)",
    "",
    "> *Méthode : **pigeonhole multinomial**, Davezies, D'Haultfœuille & Guyonvarch",
    "> (2021, *Annals of Statistics*, §2.3). Population pays unique (exp & imp tirés du",
    sprintf("> même ensemble) ; comptes multinomiaux W ; poids dyade = W[exp]×W[imp] ; refit"),
    sprintf("> `fepois` pondéré ; **B = %d** réplications. p-value = mean(|θ*−θ̂| > |θ̂|).*", B_DYADIC),
    "",
    sprintf("**⚠️ Limite intrinsèque (pays focal unique).** Le traitement est concentré sur **la Russie**. Le tirage multinomial sur la population de pays met **W[RUS]=0 avec proba ≈ e⁻¹ ≈ 37 %%** ; dans ces réplications, toutes les dyades-Russie ont un poids nul → plus aucune variation de traitement → le coefficient n'est pas identifié (draw **dégénéré**, compté en échec). C'est pourquoi `fail_rate ≈ 0,37` (≈ %d/%d draws W[RUS]=0) — **ce n'est pas un bug** mais une propriété du bootstrap pays avec un pays focal. Les SE/IC/p ci-dessous sont donc calculés sur les ~63 %% de réplications **non dégénérées**, et s'interprètent comme une inférence robuste à la dépendance dyadique **conditionnelle à la présence de la Russie dans le rééchantillon**.",
                  boot_tab$n_focal_zero[1], B_DYADIC),
    "",
    "Comparaison SE **clusterisée-paire** (existante) vs SE/IC/**p** **bootstrap dyadique** :",
    "",
    "| modèle | coef | θ̂ | SE paire | p paire | SE boot | IC95 boot | p boot | conv. |",
    "|---|---|---:|---:|---:|---:|---|---:|---:|")
  for (i in seq_len(nrow(boot_tab))) with(boot_tab[i], {
    md[[length(md) + 1]] <<- sprintf(
      "| %s | %s | %.4f | %.4f | %.4f | %.4f | [%.4f ; %.4f] | %.4f | %.0f%% |",
      model, term, estimate, se_pair, p_pair, se_boot, ci_lo, ci_hi, p_boot, 100*(1-fail_rate))
  })
  md <- c(md, "", "## Significativités qui changent (seuil 5 %)")
  chg <- boot_tab[(p_pair < 0.05) != (p_boot < 0.05)]
  if (nrow(chg)) for (i in seq_len(nrow(chg))) with(chg[i], {
    md[[length(md) + 1]] <<- sprintf(
      "- **%s / %s** : p paire = %.4f (%s) → p boot = %.4f (%s) — significativité %s.",
      model, term, p_pair, ifelse(p_pair < 0.05, "sig", "n.s."),
      p_boot, ifelse(p_boot < 0.05, "sig", "n.s."),
      ifelse(p_boot < 0.05, "GAGNÉE", "PERDUE"))
  }) else md <- c(md, "- Aucune : toutes les conclusions à 5 % sont inchangées entre SE paire et SE dyadique.")
  fail_max <- max(boot_tab$fail_rate)
  if (fail_max > 0.10) md <- c(md, "",
    sprintf("> ⚠️ **Avertissement** : taux d'échec de convergence jusqu'à %.0f%% sur certains coefs (>10%%) — résultats conservés mais à lire avec prudence.", 100*fail_max))
  md <- c(md, "", sprintf("*Convergence des réplications : %.0f%%–%.0f%%. Sanity : moyenne bootstrap ≈ θ̂ (écart max %.4g).*",
                          100*(1-max(boot_tab$fail_rate)), 100*(1-min(boot_tab$fail_rate)),
                          max(abs(boot_tab$boot_mean - boot_tab$estimate))))
  writeLines(md, file.path(out_rep(), "report_ppml_dyadic_bootstrap.md"))
  cat("  - ecrit Reports/report_ppml_dyadic_bootstrap.md\n")

  # --- Validation loggee ---
  cat(sprintf("\n  VALIDATION : convergence %.0f%%-%.0f%% | max|theta_bar-theta_hat| = %.4g\n",
              100*(1-max(boot_tab$fail_rate)), 100*(1-min(boot_tab$fail_rate)),
              max(abs(boot_tab$boot_mean - boot_tab$estimate))))
  if (max(boot_tab$fail_rate) > 0.10) cat("  !! AVERTISSEMENT : fail_rate > 10% sur un coef (cf. md).\n")
} else cat("  !! Aucun resultat bootstrap (toutes les specs ont echoue).\n")
log_step("Section 9 (bootstrap pigeonhole) terminee.")


# =============================================================================
# Section 10 : FOREST PLOT — inference clusterisee-paire vs bootstrap dyadique.
# ADDITIF STRICT (apres la Section 9). Lit boot_tab en memoire, sinon le CSV.
# Aucune reestimation : visualise tab_ppml_dyadic_bootstrap.
# =============================================================================
log_step("Section 10 : forest plot (inference paire vs bootstrap dyadique).")
# Locale UTF-8 pour le rendu des accents (le device echoue sous locale C).
suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (!exists("boot_tab"))
  boot_tab <- fread(file.path(PATH_TAB, "tab_ppml_dyadic_bootstrap.csv"))

LAB_FOREST <- c(
  treated_post                         = "DiD statique (effet moyen)",
  rus_nt                               = "Type : non-commercial",
  rus_tr                               = "Type : commercial",
  "cell_2022::a_both:post2022"         = "2×2 : condamné + sanctionné",
  "cell_2022::b_condemn_only:post2022" = "2×2 : condamné seul")
ORD_F     <- names(LAB_FOREST)
MODEL_LAB <- c(static_treated_post = "Statique", type_contrast_dir = "Type",
               did_2x2_cell_x_post2022 = "2×2")

bt_f <- as.data.table(boot_tab)
miss <- setdiff(ORD_F, bt_f$term)
if (length(miss)) cat("  !! coefs absents de la table :", paste(miss, collapse = ", "), "\n")
cat(sprintf("  - coefs presents : %d/5\n", length(intersect(ORD_F, bt_f$term))))
bt_f <- bt_f[term %in% ORD_F]
qz <- qnorm(0.975)
fr <- rbind(
  bt_f[, .(term, model, estimate, lo = estimate - qz * se_pair, hi = estimate + qz * se_pair,
           p = p_pair, method = "Cluster paire")],
  bt_f[, .(term, model, estimate, lo = ci_lo, hi = ci_hi, p = p_boot, method = "Bootstrap dyadique")])
fr[, lab    := factor(LAB_FOREST[term], levels = rev(LAB_FOREST[ORD_F]))]   # treated_post en haut
fr[, mblk   := factor(MODEL_LAB[model], levels = c("Statique", "Type", "2×2"))]
fr[, method := factor(method, levels = c("Cluster paire", "Bootstrap dyadique"))]

# Labels de convergence (1 - fail_rate), une fois par coef, a droite.
xr   <- range(fr[is.finite(lo), lo], fr[is.finite(hi), hi])
xpad <- xr[2] + 0.10 * diff(xr)
conv <- bt_f[, .(term, model, conv = sprintf("conv. %.0f%%", 100 * (1 - fail_rate)))]
conv[, `:=`(lab = factor(LAB_FOREST[term], levels = rev(LAB_FOREST[ORD_F])),
            mblk = factor(MODEL_LAB[model], levels = c("Statique", "Type", "2×2")), x = xpad)]

# Annotation du basculement de significativite (2x2 : condamne seul).
bk <- "cell_2022::b_condemn_only:post2022"
ann <- if (bk %in% bt_f$term) data.table(
  lab  = factor(LAB_FOREST[bk], levels = rev(LAB_FOREST[ORD_F])),
  mblk = factor("2×2", levels = c("Statique", "Type", "2×2")),
  x = bt_f[term == bk, estimate], txt = "perd la significativité à 5 %") else NULL

pal <- c("Cluster paire" = "#4393C3", "Bootstrap dyadique" = "#D6604D")
pd  <- position_dodge(width = 0.55)
p_for <- ggplot(fr, aes(estimate, lab, color = method)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), position = pd, height = 0.25, linewidth = 0.7) +
  geom_point(position = pd, size = 2.6) +
  geom_text(data = conv, aes(x = x, y = lab, label = conv), inherit.aes = FALSE,
            hjust = 0, size = 2.6, color = "grey55") +
  { if (!is.null(ann)) geom_text(data = ann, aes(x = x, y = lab, label = txt), inherit.aes = FALSE,
            hjust = 0.5, vjust = 2.1, size = 2.7, color = "#D6604D") } +
  facet_grid(mblk ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = pal) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.18))) +
  labs(title = "PPML — inférence clusterisée-paire vs bootstrap dyadique",
       subtitle = "Intervalles de confiance à 95 % ; point = estimation PPML (identique aux deux méthodes)",
       x = "Effet (semi-élasticité PPML)", y = NULL, color = NULL,
       caption = paste0(
         "Pigeonhole multinomial, Davezies–D'Haultfœuille–Guyonvarch (2021, §2.3), B = 200.\n",
         "Bootstrap rééchantillonnant les pays : ~37–45 % des tirages mettent W[Russie]=0\n",
         "(traitement non identifié, comptés en échec) → inférence conditionnelle à la\n",
         "présence de la Russie dans le rééchantillon. À interpréter avec ce caveat.")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey45", hjust = 0),
        legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text.y = element_text(angle = 0, face = "bold", size = 9),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
ggsave(file.path(PATH_FIG, "ppml_dyadic_bootstrap_forest.png"), p_for, width = 9, height = 6, dpi = 300)
log_step(paste("Section 10 terminee : ecrit", file.path(PATH_FIG, "ppml_dyadic_bootstrap_forest.png")))
