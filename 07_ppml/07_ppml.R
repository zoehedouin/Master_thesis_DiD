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
# (logs) -> hors gravite ; dCDH est reserve a l'intensite 2022 en 08_dcdh.
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

# Robustesse : align_2022 (abstention = condamnation partielle), si disponible.
if ("align_2022" %in% names(cov)) {
  pp_al <- unique(cov[, .(partner = partner_iso3, align_2022)])[!is.na(align_2022)]
  d_al <- merge(df, pp_al, by = "partner", all.x = TRUE)
  d_al[is.na(align_2022), align_2022 := -1L]   # non-Russie / inconnu = categorie a part
  d_al[, align_f := factor(align_2022)]
  m_align <- tryCatch(fepois(trade_value ~ i(align_f, post2022, ref = "0") + rta + under_any_sanction |
                               exp_iso3^year + imp_iso3^year + pair, data = d_al, cluster = ~ pkey),
                      error = function(e) {cat("    align robustness skip:", conditionMessage(e), "\n"); NULL})
  if (!is.null(m_align)) write_tab(extract_coefs(m_align, "did_2x2_align2022"), "tab_2x2_did_align")
}


# ---- Section 5 : event study Sun & Abraham (pre-tendances) -------------------

log_step("Section 5 : event study Sun & Abraham.")
# Garder jamais-traites (cohort=10000) ; exclure left-censored in-window (onset
# < YR_MIN+1, pas de rel_time=-1). Reduit le nombre de cohortes -> tractable.
df_es <- df[cohort == 10000L | onset_year >= YR_MIN + 1L]
rng <- range(df_es[ever == TRUE, rel_time])
cat("  - obs event study :", nrow(df_es), "| cohortes traitees :",
    uniqueN(df_es[ever == TRUE, cohort]), "| rel_time range :", paste(rng, collapse = " "), "\n")
fml_es <- as.formula(sprintf(
  "trade_value ~ sunab(cohort, year, bin.rel = list('-5' = %d:-5, '5' = 5:%d)) + rta | exp_iso3^year + imp_iso3^year + pair",
  rng[1], rng[2]))
tic(); m_es <- fepois(fml_es, data = df_es, cluster = ~ pkey)
cat("    sunab:", toc(), "s | N =", nobs(m_es), "\n")

att <- summary(m_es, agg = "att")$coeftable
att_ct <- as.data.table(att, keep.rownames = "term"); setnames(att_ct, 2:5, c("estimate","se","stat","p"))
att_ct <- att_ct[term == "ATT"]   # ne garder que l'ATT (pas la ligne rta)
es <- as.data.table(coeftable(m_es), keep.rownames = "term"); setnames(es, 2:5, c("estimate","se","stat","p"))
es <- es[grepl("year::", term)]
es[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
es[, `:=`(ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se)]; setorder(es, rel_time)
cat("\n  --- ATT agrege ---\n"); print(att_ct[, .(estimate = round(estimate,4), se = round(se,4), p = round(p,4))])
out_es <- rbindlist(list(
  es[, .(term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
  att_ct[, .(term = "ATT", rel_time = NA_integer_, estimate, se,
             ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se)]), use.names = TRUE)
write_tab(out_es, "tab_eventstudy_sunab")

p_es <- ggplot(es, aes(rel_time, estimate)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = -0.5, lty = 3, color = "grey60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166AC") +
  geom_line(color = "#2166AC", linewidth = 0.6) + geom_point(color = "#2166AC", size = 2) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Sanctions non-commerciales contre la Russie : effet sur le commerce",
       subtitle = "Event study Sun & Abraham (PPML 3-way FE, panel large). IC 95% cluster paire. k=0 = transition.",
       x = "Temps relatif a l'onset (annees)", y = "Semi-elasticite (effet sur log trade)",
       caption = "Source : BACI-CEPII, GSDB-R4. Traitement = partenaire sanctionne la Russie (dirige, non-commercial).")
ggsave(file.path(PATH_FIG, "es_sunab_russia.png"), p_es, width = 10, height = 6, dpi = 300)


# ---- Section 6 : pre-tendances CONDITIONNELLES a l'energie -------------------

log_step("Section 6 : pre-tendances conditionnelles a la dependance energetique.")
# Tercile d'energie = exposition PRE-guerre (moyenne 2018-2021), fixe par
# partenaire. On NE met PAS l'energie en regresseur (absorbee par FE paire) :
# on STRATIFIE l'event study par tercile et on verifie que les leads restent
# plats dans chaque strate (pas pilote par le choc gazier differentiel de 2022).
en_ref <- cov[, .(energy = mean(partner_energy_dep_rus[year %in% 2018:2021], na.rm = TRUE)),
              by = .(partner = partner_iso3)]
en_ref <- en_ref[is.finite(energy)]
en_ref[, terc := cut(energy, quantile(energy, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     labels = c("T1_low", "T2_mid", "T3_high"), include.lowest = TRUE)]
df_es <- merge(df_es, en_ref[, .(partner, terc)], by = "partner", all.x = TRUE)

es_by <- list()
for (k in c("T1_low", "T2_mid", "T3_high")) {
  # treated du tercile k + tous les jamais-traites comme controles
  dk <- df_es[cohort == 10000L | (ever == TRUE & terc == k)]
  if (dk[ever == TRUE, uniqueN(cohort)] < 1L) next
  rk <- range(dk[ever == TRUE, rel_time])
  fk <- as.formula(sprintf(
    "trade_value ~ sunab(cohort, year, bin.rel = list('-5' = %d:-5, '5' = 5:%d)) + rta | exp_iso3^year + imp_iso3^year + pair",
    rk[1], rk[2]))
  mk <- tryCatch(fepois(fk, data = dk, cluster = ~ pkey),
                 error = function(e) {cat("    tercile", k, "skip:", conditionMessage(e), "\n"); NULL})
  if (is.null(mk)) next
  ek <- as.data.table(coeftable(mk), keep.rownames = "term"); setnames(ek, 2:5, c("estimate","se","stat","p"))
  ek <- ek[grepl("year::", term)]
  ek[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
  ek[, `:=`(tercile = k, ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se)]
  es_by[[k]] <- ek
  cat("    tercile", k, ": N =", nobs(mk), "| treated cohorts :", dk[ever==TRUE, uniqueN(cohort)], "\n")
}
if (length(es_by)) {
  es_by <- rbindlist(es_by); setorder(es_by, tercile, rel_time)
  # version inconditionnelle (rappel) pour comparaison
  es_uncond <- copy(es[, .(rel_time, estimate, se, ci_lo, ci_hi)]); es_uncond[, tercile := "all (uncond.)"]
  cond_tab <- rbind(es_by[, .(tercile, rel_time, estimate, se, ci_lo, ci_hi)], es_uncond)
  write_tab(cond_tab, "tab_pretrends_conditional")
  p_by <- ggplot(es_by, aes(rel_time, estimate, color = tercile, fill = tercile)) +
    geom_hline(yintercept = 0, lty = 2, color = "grey50") +
    geom_vline(xintercept = -0.5, lty = 3, color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.6) +
    scale_x_continuous(breaks = seq(-5, 5, 1)) +
    scale_color_manual(values = c(T1_low = "#2166AC", T2_mid = "#999999", T3_high = "#B2182B")) +
    scale_fill_manual(values = c(T1_low = "#2166AC", T2_mid = "#999999", T3_high = "#B2182B")) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(size = 10, color = "grey40"), legend.position = "bottom",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(title = "Event study par tercile de dependance energetique russe",
         subtitle = "Pre-tendances a dependance energetique comparable (leads plats par strate ?)",
         x = "Temps relatif a l'onset (annees)", y = "Semi-elasticite", color = NULL, fill = NULL,
         caption = "Tercile = exposition energetique pre-guerre (moyenne 2018-2021), fixe par partenaire.")
  ggsave(file.path(PATH_FIG, "es_sunab_by_energy.png"), p_by, width = 10, height = 6, dpi = 300)
} else cat("  !! Aucun tercile estimable.\n")


# ---- Section 7 : HonestDiD (Rambachan & Roth 2023) --------------------------

log_step("Section 7 : HonestDiD (sensibilite sur les pre-tendances).")
hd_ok <- requireNamespace("HonestDiD", quietly = TRUE)
if (!hd_ok) cat("  !! package HonestDiD indisponible : etape sautee (a installer).\n") else {
  # NOTE TECHNIQUE : fixest sunab n'expose PAS la vcov agregee event-time
  # (coeftable est agrege "year::k" mais vcov reste brut "year::k:cohort::c").
  # HonestDiD exige betahat ET vcov coherents -> on l'applique a la REPRESENTATION
  # event-study TWFE i(rel_time) (memes specs/echantillon que l'Etape 5), dont
  # coef() et vcov() concordent. Vu la cohorte 2014 DOMINANTE (39/44 traites),
  # cet event-study TWFE est quasi identique a Sun-Abraham -> sensibilite fidele.
  # jamais-traites -> categorie de reference (-1) pour rester CONTROLES (sinon
  # i() les droppe sur NA). Traites -> temps relatif binne a [-5, 5].
  df_es[, rel_bin := ifelse(ever, pmax(-5L, pmin(5L, rel_time)), -1L)]
  m_twfe <- fepois(trade_value ~ i(rel_bin, ref = -1) + rta |
                     exp_iso3^year + imp_iso3^year + pair, data = df_es, cluster = ~ pkey)
  b_all <- coef(m_twfe); V_all <- vcov(m_twfe)
  ev <- grep("rel_bin::", names(b_all), value = TRUE)
  rt_h <- as.integer(sub(".*rel_bin::(-?[0-9]+).*", "\\1", ev)); ord <- order(rt_h)
  ev <- ev[ord]; rt_h <- rt_h[ord]
  betahat <- b_all[ev]; V <- V_all[ev, ev, drop = FALSE]
  numPre  <- sum(rt_h < 0)
  numPost <- sum(rt_h >= 0)
  cat("  - event-study TWFE pour HonestDiD : numPre =", numPre, "| numPost =", numPost, "\n")
  hd <- tryCatch({
    rm_res <- HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = betahat, sigma = V, numPrePeriods = numPre, numPostPeriods = numPost,
      Mbarvec = seq(0, 2, by = 0.5))
    sd_res <- HonestDiD::createSensitivityResults(
      betahat = betahat, sigma = V, numPrePeriods = numPre, numPostPeriods = numPost,
      Mvec = seq(0, 0.3, by = 0.1))
    list(rm = as.data.table(rm_res), sd = as.data.table(sd_res))
  }, error = function(e) {cat("  !! HonestDiD erreur :", conditionMessage(e), "\n"); NULL})
  if (!is.null(hd)) {
    rm_dt <- hd$rm; rm_dt[, method := "relative_magnitudes"]
    sd_dt <- hd$sd; sd_dt[, method := "smoothness"]
    hd_tab <- rbind(rm_dt, sd_dt, fill = TRUE)
    write_tab(hd_tab, "tab_honestdid_bounds")
    # M-bar de rupture : plus grand Mbar ou la borne basse reste > 0 (ou haute < 0)
    sig <- rm_dt[ (lb > 0 & ub > 0) | (lb < 0 & ub < 0) ]
    Mbreak <- if (nrow(sig)) max(sig$Mbar, na.rm = TRUE) else NA_real_
    cat("  - M-bar de rupture (relative magnitudes) :", Mbreak, "\n")
    p_hd <- ggplot(rm_dt, aes(Mbar)) +
      geom_hline(yintercept = 0, lty = 2, color = "grey50") +
      geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2, fill = "#2166AC") +
      geom_line(aes(y = lb)) + geom_line(aes(y = ub)) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 13),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)) +
      labs(title = "HonestDiD - sensibilite (relative magnitudes)",
           subtitle = sprintf("Borne de rupture M-bar = %s (>=1 = robuste)", format(Mbreak)),
           x = "M-bar (violation relative des pre-tendances)", y = "IC robuste de l'ATT",
           caption = "Rambachan & Roth (2023). Sur l'event study Sun & Abraham de l'Etape 5.")
    ggsave(file.path(PATH_FIG, "honestdid_sensitivity.png"), p_hd, width = 9, height = 6, dpi = 300)
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
    "es_sunab_by_energy.png (plats par tercile ?).\n")
cat("(iii) HonestDiD : tab_honestdid_bounds (M-bar de rupture).\n")
cat("---- PUIS seulement l'effet ----\n")
cat(sprintf("  Statique (treated_post)        : %.4f (p=%.4f)\n",
            static_csv[term=="treated_post" & model=="static_treated_post", estimate],
            static_csv[term=="treated_post" & model=="static_treated_post", p]))
cat(sprintf("  ATT agrege (Sun-Abraham)       : %.4f (p=%.4f)\n", att_ct$estimate, att_ct$p))
cat("\n== Figures ==\n"); print(list.files(PATH_FIG))
cat("== Tables ==\n");  print(list.files(PATH_TAB))
log_step("07_ppml.R termine.")
