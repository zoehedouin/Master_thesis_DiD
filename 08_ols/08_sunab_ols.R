# =============================================================================
# 08_sunab_ols.R — Event study Sun-Abraham en OLS sur logs, panel pkey (monde).
# -----------------------------------------------------------------------------
# Section 08 = monde OLS/log de l'intensite des sanctions (par opposition au PPML
# DIRIGE du 07). Estimateur le plus simple : Sun & Abraham (2021) en OLS sur
# log(trade+1), panel paire NON ORDONNEE (pkey), FE pkey + year. feols gere les
# FE creux -> panel pkey COMPLET (pas de sous-echantillonnage : c'est une
# contrainte propre au dCDH, pas a feols).
#
# Construction du panel pkey reprise VERBATIM du §4.1 (08_ols.R).
# Estimation = VRAIE (feols), imprime des chiffres reels. Aucun chiffre fabrique.
#
# Sorties : 08_ols/tables/tab_sunab_ols.csv,
#           08_ols/figures/es_fig_sunab_ppml_vs_ols.png,
#           08_ols/08_sunab_ols_report.md.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(ggplot2)
})
# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))
})
PART <- "08_ols"
PATH_TAB <- out_tab("EventStudy")
PATH_FIG <- out_fig("EventStudy")

YR_MIN <- 2008L; YR_MAX <- 2023L           # fenetre VERBATIM du §4.1
PRE  <- 2018:2021; POST <- 2022:2023        # base pre-guerre / escalade (diag. zeros)

# ---- AUDITABLE : paires a exclure pour "trou de reporting" post-2022 ---------
# Sans imputation. Ancre = Bielorussie : "BLR_RUS" est la plus grosse extinction
# pre>0 -> zero post du diagnostic zeros (~33.5 Md USD -> 0), invraisemblable
# comme vrai effondrement (le commerce BLR-RUS a AUGMENTE) -> lacune de
# declaration BACI (la Bielorussie, sanctionnee, cesse de declarer ; les deux
# cotes eteints -> le miroir BACI ne reconstruit rien). Regle : toute paire pkey
# contenant a la fois "BLR" et "RUS". EXTENSIBLE a d'autres non-declarants
# documentes post-2022 (ajouter ci-dessous). NB : exclusion au niveau PAIRE,
# JAMAIS pays -> la Russie reste (c'est le traitement) ; les paires
# Russie-Occident restent (le partenaire occidental declare -> flux reel).
pairs_reporting_gap <- c("BLR_RUS")   # extensible : c("BLR_RUS", "XXX_RUS", ...)

# ---- 1. Panel pkey (VERBATIM §4.1) ------------------------------------------
log_step("1. Panel pkey (verbatim §4.1), sample COMPLET.")
d <- read_parquet_safe(PATH_SANCTIONS_PANEL)
d[, pkey := ifelse(exp_iso3 < imp_iso3,
                   paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]
pk <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot     = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core)),
  by = .(pkey, year)]
pk[, log_trade := log(trade_tot + 1)]
rm(d); gc(verbose = FALSE)
cat(sprintf("  - pkey-year obs : %d | paires : %d\n", nrow(pk), uniqueN(pk$pkey)))
pk_full <- copy(pk)   # panel complet (avant toute exclusion) pour le tri PARTIE 2

# ---- 2. Liste des extinctions sous traitement + exclusion reporting-gap ------
log_step("2. Extinctions pre>0 -> zero post (ever-traitees) + exclusion reporting-gap.")
ever_treated <- pk[n_active_core > 0L, unique(pkey)]
pp <- pk[, .(
  pre_mean   = mean(trade_tot[year %in% PRE]),
  post_zero_all = ifelse(any(year %in% POST), as.integer(all(trade_tot[year %in% POST] == 0)), NA_integer_),
  tier_post  = ifelse(any(year %in% POST),
                      max(fcase(n_active_core[year %in% POST] == 0L, 0L,
                                n_active_core[year %in% POST] == 1L, 1L,
                                n_active_core[year %in% POST] <= 5L, 2L, default = 3L)), NA_integer_),
  rus = grepl("(^|_)RUS($|_)", pkey[1])), by = pkey]
induced <- pp[pkey %in% ever_treated & is.finite(pre_mean) & pre_mean > 0 &
              !is.na(post_zero_all) & post_zero_all == 1L][order(-pre_mean)]
cat(sprintf("  - paires ever-traitees passant positif(pre 2018-21) -> zero(post 2022-23) : %d (Russie : %d)\n",
            nrow(induced), induced[rus == TRUE, .N]))
cat("  - sous-ensemble RUSSIE (pkey, moy. pre k USD, palier post) :\n")
print(induced[rus == TRUE, .(pkey, pre_mean = round(pre_mean, 1), tier_post)])

# Exclusion (uniforme, servira aussi a C). Verifie l'ancre BLR&RUS.
gap_rule <- unique(pk$pkey)[grepl("BLR", unique(pk$pkey)) & grepl("RUS", unique(pk$pkey))]
stopifnot(all(pairs_reporting_gap %in% gap_rule) || length(gap_rule) == 0)
n_pair0 <- uniqueN(pk$pkey); n_obs0 <- nrow(pk)
pk <- pk[!(pkey %in% pairs_reporting_gap)]
cat(sprintf("  - exclusion reporting-gap %s : -%d paire(s), -%d obs (reste %d paires, %d obs)\n",
            paste(pairs_reporting_gap, collapse = ","),
            n_pair0 - uniqueN(pk$pkey), n_obs0 - nrow(pk), uniqueN(pk$pkey), nrow(pk)))

# ---- 2b. TRI des extinctions Russie (data step, alimente l'exclusion de C) ---
# C (dose) vit sur l'escalade 2022 -> les extinctions Russie doivent etre triees.
# Critere REPRODUCTIBLE (sans imputation) : le partenaire non-russe declare-t-il
# encore du commerce (present dans BACI = miroir reconstruit) sur 2022-2023 ?
#   * partenaire present (commerce mondial BACI substantiel) -> zero avec la Russie
#     = collapse REEL bilateral -> on GARDE.
#   * partenaire disparu de BACI (les deux cotes eteints) -> trou de reporting
#     -> on FLAGUE (niveau paire, jamais pays).
log_step("2b. Tri des extinctions Russie (commerce mondial BACI pre/post du partenaire).")
cw <- rbind(pk_full[, .(iso = sub("_.*", "", pkey), trade_tot, year)],
            pk_full[, .(iso = sub(".*_", "", pkey), trade_tot, year)])
world <- cw[, .(world = sum(trade_tot)), by = .(iso, year)]
wpp <- merge(world[year %in% PRE,  .(wpre  = mean(world)), by = iso],
             world[year %in% POST, .(wpost = mean(world)), by = iso], by = "iso", all = TRUE)
tri <- copy(induced[rus == TRUE])
tri[, partner := fifelse(sub("_.*", "", pkey) == "RUS", sub(".*_", "", pkey), sub("_.*", "", pkey))]
tri <- merge(tri, wpp, by.x = "partner", by.y = "iso", all.x = TRUE)
tri[, retention := wpost / wpre]
tri[, verdict := fcase(
  pkey == "BLR_RUS", "FLAG (anchor: RUS+BLR both COMTRADE-dark; 33.5bn->0 implausible)",
  is.na(wpost) | wpost <= 0 | retention < 0.02, "FLAG (partner vanished from BACI)",
  default = "KEEP (partner reports world trade -> real bilateral collapse)")]
setorder(tri, -pre_mean)
cat("  - tri des extinctions Russie :\n")
print(tri[, .(pkey, partner, pre_mean = round(pre_mean, 0), tier_post,
              wpost = round(wpost, 0), retention = round(retention, 2), verdict)])
fwrite(tri[, .(pkey, partner, pre_mean, tier_post, wpre, wpost, retention, verdict)],
       file.path(PATH_TAB, "tab_reporting_gap_triage.csv"))
flagged <- tri[grepl("^FLAG", verdict), pkey]
pairs_reporting_gap <- unique(c(pairs_reporting_gap, flagged))   # extension data-driven
cat(sprintf("  - paires FLAGUEES (exclues uniformement, dont C) : %s\n",
            paste(pairs_reporting_gap, collapse = ", ")))
cat(sprintf("  - extension vs BLR_RUS seul : %d paire(s) supplementaire(s) (les autres = collapses reels gardes)\n",
            length(setdiff(pairs_reporting_gap, "BLR_RUS"))))

# ---- 3. Onset / cohorte (Sun-Abraham, traitement binaire absorbant) ---------
log_step("3. Onset / cohorte.")
onset <- pk[n_active_core > 0L, .(onset_year = min(year)), by = pkey]
pk <- merge(pk, onset, by = "pkey", all.x = TRUE)
pk[, cohort := fifelse(is.na(onset_year), 10000L, onset_year)]  # never-treated = sentinelle
# Exclure les left-censored (onset == YR_MIN : pas de pre-periode), comme la
# variante full-window 2008-2023 du sunab du 07 (comparabilite).
lc <- pk[cohort != 10000L & onset_year == YR_MIN, uniqueN(pkey)]
pk <- pk[!(cohort != 10000L & onset_year == YR_MIN)]
pk[, rel_time := fifelse(cohort == 10000L, NA_integer_, year - cohort)]
cat(sprintf("  - left-censored (onset %d) exclus : %d paires | cohortes traitees : %d | never-treated : %d\n",
            YR_MIN, lc, uniqueN(pk[cohort != 10000L, cohort]), uniqueN(pk[cohort == 10000L, pkey])))

# ---- 4. ESTIMATION B : Sun-Abraham OLS --------------------------------------
log_step("4. feols Sun-Abraham OLS (log(trade+1) | pkey + year, cluster pkey).")
rng <- range(pk[cohort != 10000L, rel_time])
# Memes conventions que le 07 : bornes binnees +/-5, reference pre-onset (-1).
m_B <- feols(log_trade ~ sunab(cohort, year,
               bin.rel = list("-5" = rng[1]:-5, "5" = 5:rng[2])) | pkey + year,
             data = pk, cluster = ~ pkey)

att <- as.data.table(summary(m_B, agg = "att")$coeftable, keep.rownames = "term")
setnames(att, 2:5, c("estimate", "se", "stat", "p")); att <- att[term == "ATT"]
es <- as.data.table(coeftable(m_B), keep.rownames = "term")
setnames(es, 2:5, c("estimate", "se", "stat", "p"))
es <- es[grepl("year::", term)]
es[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
es[, `:=`(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]; setorder(es, rel_time)
# Moyenne post k>=+1 (lecture demandee), a cote de l'ATT fixest (post k>=0).
att_post1 <- es[rel_time >= 1, mean(estimate)]
cat(sprintf("  - ATT (fixest agg, post k>=0) = %.4f (se %.4f, p %.4g) | moyenne post k>=+1 = %.4f\n",
            att$estimate, att$se, att$p, att_post1))
cat("  - profil event-study OLS :\n")
print(es[, .(rel_time, estimate = round(estimate, 4), se = round(se, 4),
             ci_lo = round(ci_lo, 4), ci_hi = round(ci_hi, 4))])

out_B <- rbindlist(list(
  es[, .(model = "B_ols_pkey", term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
  att[, .(model = "B_ols_pkey", term = "ATT", rel_time = NA_integer_, estimate, se,
          ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)],
  data.table(model = "B_ols_pkey", term = "ATT_post_k>=1", rel_time = NA_integer_,
             estimate = att_post1, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_)),
  use.names = TRUE)
fwrite(out_B, file.path(PATH_TAB, "tab_sunab_ols.csv"))
cat("  - ecrit tab_sunab_ols.csv\n")

# ---- 4'. ADDENDUM A' : PPML-sunab sur le MEME panel pkey que B ---------------
# Seule l'ECHELLE change vs B (fepois/zeros natifs au lieu de feols/log+1) :
# meme panel pk, memes cohortes, memes bornes +-5, meme exclusion BLR_RUS, memes
# left-censored exclus. Decompose l'ecart A<->B :
#   A->A' = effet du BUNDLE geometrie+traitement (dirige+MRT -> pkey monde) ;
#   A'->B = effet de l'ECHELLE PURE (PPML -> OLS log+1, MEME panel).
log_step("4'. fepois Sun-Abraham (A') sur le panel pkey (outcome = trade_tot niveau).")
aprime_ok <- TRUE
m_Ap <- tryCatch(
  fepois(trade_tot ~ sunab(cohort, year,
           bin.rel = list("-5" = rng[1]:-5, "5" = 5:rng[2])) | pkey + year,
         data = pk, cluster = ~ pkey),
  error = function(e) { cat("  !! A' (fepois) a echoue :", conditionMessage(e), "\n"); NULL })
if (is.null(m_Ap)) {
  aprime_ok <- FALSE
  cat("  !! A' non estimable (OOM/non-convergence) -> reporte tel quel, AUCUN chiffre fabrique.\n")
} else {
  attA <- as.data.table(summary(m_Ap, agg = "att")$coeftable, keep.rownames = "term")
  setnames(attA, 2:5, c("estimate", "se", "stat", "p")); attA <- attA[term == "ATT"]
  esA <- as.data.table(coeftable(m_Ap), keep.rownames = "term")
  setnames(esA, 2:5, c("estimate", "se", "stat", "p"))
  esA <- esA[grepl("year::", term)]
  esA[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
  esA[, `:=`(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]; setorder(esA, rel_time)
  attA_post1 <- esA[rel_time >= 1, mean(estimate)]
  cat(sprintf("  - A' ATT (PPML, post k>=0) = %.4f (se %.4f) | moyenne post k>=+1 = %.4f\n",
              attA$estimate, attA$se, attA_post1))
  print(esA[, .(rel_time, estimate = round(estimate, 4), se = round(se, 4))])
  out_A <- rbindlist(list(
    esA[, .(model = "Aprime_ppml_pkey", term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
    attA[, .(model = "Aprime_ppml_pkey", term = "ATT", rel_time = NA_integer_, estimate, se,
             ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]), use.names = TRUE)
  fwrite(out_A, file.path(PATH_TAB, "tab_sunab_pkey_ppml.csv"))
  cat("  - ecrit tab_sunab_pkey_ppml.csv\n")
}

# ---- 5. DECOMPOSITION A -> A' -> B (overlay 3 courbes) -----------------------
log_step("5. Decomposition A -> A' -> B (overlay 3 courbes).")
pathA <- file.path(ANALYSIS_ROOT, "07_ppml", "tables", "tab_eventstudy_sunab.csv")
have_A <- file.exists(pathA)
LAB_A <- "A : PPML directed (07, MRT)"; LAB_AP <- "A' : PPML pkey (scale only)"; LAB_B <- "B : OLS pkey log(trade+1)"
parts <- list()
if (have_A) {
  A <- fread(pathA)[window == "2008_2023" & term == "event_time", .(rel_time, estimate, ci_lo, ci_hi)]
  A[, profil := LAB_A]; parts[["A"]] <- A
} else cat("  !! 07 event-study CSV introuvable :", pathA, "\n")
if (aprime_ok) { Ap <- esA[, .(rel_time, estimate, ci_lo, ci_hi)]; Ap[, profil := LAB_AP]; parts[["Ap"]] <- Ap }
B <- es[, .(rel_time, estimate, ci_lo, ci_hi)]; B[, profil := LAB_B]; parts[["B"]] <- B
ov <- rbindlist(parts, use.names = TRUE)
ov[, profil := factor(profil, levels = c(LAB_A, LAB_AP, LAB_B))]
pal <- setNames(c("#2166AC", "#1B7837", "#B2182B"), c(LAB_A, LAB_AP, LAB_B))
p <- ggplot(ov, aes(rel_time, estimate, color = profil, fill = profil)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = 0, lty = 3, color = "grey55") +
  geom_vline(xintercept = 8, lty = 3, color = "grey75") +
  annotate("text", x = 0, y = Inf, label = "onset (~2014)", vjust = 1.4, hjust = -0.05, size = 2.7, color = "grey45") +
  annotate("text", x = 8, y = Inf, label = "~2022 (2014 cohort)", vjust = 1.4, hjust = -0.02, size = 2.7, color = "grey55") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.08, color = NA) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(-5, 9, 1)) +
  scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"), legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Decomposing the sanctions event study: A -> A' -> B",
       subtitle = "A->A' = geometry+treatment bundle (directed+MRT -> pkey world). A'->B = pure scale (PPML -> OLS log+1, same panel).",
       x = "Years relative to sanction onset", y = "Effect on trade with Russia (semi-elasticity)",
       color = NULL, fill = NULL,
       caption = "A = 07_ppml full window 2008-2023 (directed, exp^year+imp^year+pair). A'/B = pkey + year FE, pair-clustered, same panel/treatment/window.")
ggsave(file.path(PATH_FIG, "es_fig_sunab_ppml_vs_ols.png"), p, width = 10, height = 6, dpi = 300)
cat("  - ecrit es_fig_sunab_ppml_vs_ols.png (3 courbes)\n")

cat("\n=============================================================\n")
cat("RECAP — 08_sunab_ols.R\n")
cat(sprintf("  panel pkey (B/A') : %d obs, %d paires (exclusion %s)\n",
            nrow(pk), uniqueN(pk$pkey), paste(pairs_reporting_gap, collapse = ",")))
cat(sprintf("  ATT  B  (OLS log+1, post k>=0) = %.4f\n", att$estimate))
if (aprime_ok) cat(sprintf("  ATT  A' (PPML pkey,  post k>=0) = %.4f\n", attA$estimate))
cat(sprintf("  ATT  A  (PPML dirige 07 full)  = %s\n",
            if (have_A) sprintf("%.4f", fread(pathA)[window=="2008_2023" & term=="ATT", estimate]) else "n/a"))
cat("  DECOMPOSITION : A->A' = bundle geometrie/traitement ; A'->B = echelle pure.\n")
cat(sprintf("  tri extinctions Russie : %d gardees, %d flaguees (exclusion C = %s)\n",
            tri[grepl("^KEEP", verdict), .N], tri[grepl("^FLAG", verdict), .N],
            paste(pairs_reporting_gap, collapse = ",")))
cat("=============================================================\n")
log_step("08_sunab_ols.R termine.")
