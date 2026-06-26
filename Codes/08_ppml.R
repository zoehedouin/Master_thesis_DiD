# =============================================================================
# 08_ppml.R — fusion of 04_gravity_estimation + 10_event_study_sanctions
#             (feuille de route §3). Gravity PPML backbone.
# -----------------------------------------------------------------------------
# Backbone PPML de gravite (FE exp_iso3^year + imp_iso3^year + pair, cluster
# paire, zeros gardes). Roadmap §3 :
#   (i)   PPML statique du traitement
#         -> DiD statique (treated_post sous sanction non-commerciale).
#   (ii)  Contraste par type (replique col. 2 GSDB-R4, Yalcin et al. 2025 :
#         commercial complet vs partiel vs non-commercial).
#   (iii) 2x2 en interaction (condamne seulement / sanctionne / les deux /
#         ni l'un ni l'autre). NOTE : ce 2x2 condamnation x sanction n'existe
#         PAS encore (les votes ONU ne sont pas construits) -> voir le TODO
#         §3-iii ci-dessous. Aucun resultat n'est fabrique.
#   (iv)  Event study Sun & Abraham (sunab) — a lire a partir de k=+1.
#
# NOTE : l'IPD reste une mesure d'ALIGNEMENT a valider, plus comme outcome
#        pilote. Les specifications gravite IPD de 04 sont conservees telles
#        quelles (diagnostics within, table progressive, heterogeneites,
#        time-varying, robustesse) comme backbone et validation de l'alignement.
#
# Provenance des blocs : voir les bannieres
#   # ===== [from 04_gravity_estimation.R] =====
#   # ===== [from 10_event_study_sanctions.R] =====
# =============================================================================


# ---- Section 0 : Setup (consolide) -----------------------------------------

# Garde d'installation (union des paquets des deux scripts sources).
need <- c("data.table", "arrow", "fixest", "modelsummary",
          "ggplot2", "scales", "kableExtra")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) {
  install.packages(miss, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(modelsummary)
  library(ggplot2)
  library(scales)
  library(kableExtra)
})

source("00_setup.R")

setFixest_etable(markdown = FALSE)
# (setFixest_nthreads(0) deja regle dans 00_setup.R ; log_step/tic/toc fournis.)

# Variable dictionary for etable
DICT <- c(ipd            = "IPD",
          ipd_sq         = "IPD$^2$",
          rta            = "RTA",
          log_dist       = "log(Distance)",
          contig         = "Contiguity",
          comlang_off    = "Common Language",
          colony         = "Colonial Tie",
          log_gdp_exp    = "log(GDP exp.)",
          log_gdp_imp    = "log(GDP imp.)")

log_step("Setup termine.")


# =============================================================================
# ===== [from 04_gravity_estimation.R] =======================================
# Backbone PPML de gravite : IPD comme mesure d'alignement (a valider).
# Sections i-iii du backbone : within-diagnostic, table progressive,
# heterogeneites (strategic, NATO), time-varying, diagnostics, robustesse.
# =============================================================================


# ---- Section 0bis : Preparation des donnees --------------------------------

log_step("Chargement et preparation du panel.")
df <- read_parquet_safe(PATH_STRATEGIC)

df[, pair      := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year  := paste(exp_iso3, year,     sep = "_")]
df[, imp_year  := paste(imp_iso3, year,     sep = "_")]
df[, log_trade   := log(trade_value + 1)]
df[, log_dist    := log(dist)]
df[, log_gdp_exp := log(exp_gdp_real)]
df[, log_gdp_imp := log(imp_gdp_real)]
df[, ipd_sq      := ipd^2]
df[, non_strategic_trade := pmax(trade_value - strategic_trade_value, 0)]

cat("  - Obs total                          :", nrow(df), "\n")
cat("  - Obs avec ipd non-NA                :", df[!is.na(ipd), .N], "\n")
cat("  - Obs avec trade>0 ET ipd non-NA     :", df[trade_value > 0 & !is.na(ipd), .N], "\n")
cat("  - Paires uniques (panel)             :", uniqueN(df$pair), "\n")
cat("  - Paires uniques avec ipd non-NA     :",
    uniqueN(df[!is.na(ipd), pair]), "\n")


# =============================================================================
# SECTION 1 : Within-variation diagnostic
# =============================================================================

log_step("Section 1 : within-variation diagnostic de l'IPD.")
tic()
res_ipd <- feols(ipd ~ 1 | exp_year + imp_year + pair,
                 data = df[!is.na(ipd)], notes = FALSE)
within_var <- var(residuals(res_ipd), na.rm = TRUE)
total_var  <- var(df[!is.na(ipd), ipd])
ratio_w    <- within_var / total_var

cat(sprintf("  - Variance totale IPD                : %.4f\n", total_var))
cat(sprintf("  - Variance within (apres FE 3-way)   : %.4f\n", within_var))
cat(sprintf("  - Ratio within/total                 : %.4f\n", ratio_w))
if (ratio_w < 0.20) {
  cat("  - WARNING : ratio < 0.20. Identification surtout via interactions temporelles.\n")
} else {
  cat("  - OK : ratio >= 0.20.\n")
}
cat("  - Time:", toc(), "s\n")


# =============================================================================
# SECTION 2 : Specifications progressives (table principale)
# =============================================================================

log_step("Section 2 : 5 specifications progressives (peut prendre 5-10 min).")

# --- Spec 1 : OLS log-lineaire (benchmark naif) ----------------------------
log_step("  Spec 1 : OLS (trade > 0).")
tic()
spec1 <- feols(
  log_trade ~ ipd + log_dist + contig + comlang_off + colony +
              log_gdp_exp + log_gdp_imp + rta,
  data = df[trade_value > 0 & !is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec1), "\n")

# --- Spec 2 : PPML avec FE separes ----------------------------------------
log_step("  Spec 2 : PPML separated FE (exp + imp + year).")
tic()
spec2 <- fepois(
  trade_value ~ ipd + log_dist + contig + comlang_off + colony +
                log_gdp_exp + log_gdp_imp + rta
                | exp_iso3 + imp_iso3 + year,
  data = df[!is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec2), "\n")

# --- Spec 3 : PPML avec FE country x year ---------------------------------
log_step("  Spec 3 : PPML with exp-year + imp-year FE.")
tic()
spec3 <- fepois(
  trade_value ~ ipd + log_dist + contig + comlang_off + colony + rta
                | exp_year + imp_year,
  data = df[!is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec3), "\n")

# --- Spec 4 : PPML three-way FE (WORKHORSE) -------------------------------
log_step("  Spec 4 : PPML three-way FE (workhorse).")
tic()
spec4 <- fepois(
  trade_value ~ ipd + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec4), "\n")

# --- Spec 5 : Spec 4 avec multiway clustering -----------------------------
log_step("  Spec 5 : PPML three-way FE + multiway clustering.")
tic()
spec5 <- fepois(
  trade_value ~ ipd + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd)],
  vcov = ~pair + exp_year + imp_year
)
cat("    Time:", toc(), "s | N =", nobs(spec5), "\n")

# --- Table principale ------------------------------------------------------
log_step("  Sauvegarde table principale.")
etable(spec1, spec2, spec3, spec4, spec5,
       title = "Gravity Model: From OLS to Three-Way FE PPML",
       headers = c("OLS", "PPML\\\\Sep. FE", "PPML\\\\Ctry-Yr",
                   "PPML\\\\3-Way FE", "PPML\\\\3-Way MW"),
       dict = DICT,
       notes = paste("Cluster-robust SE in parentheses (clustered by pair).",
                     "Spec 5 uses multiway clustering (pair + exp-year + imp-year)."),
       tex = TRUE,
       replace = TRUE,
       file = file.path(out_tab("Estimation"), "tab_main_progression.tex"))

# CSV equivalent
extract_coefs <- function(model, name) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  # PPML donne "z value"/"Pr(>|z|)", OLS donne "t value"/"Pr(>|t|)"
  # On renomme par position pour rester robuste.
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct[, model := name]
  ct[]
}
main_csv <- rbindlist(list(
  extract_coefs(spec1, "1_OLS"),
  extract_coefs(spec2, "2_PPML_sep"),
  extract_coefs(spec3, "3_PPML_ctry_yr"),
  extract_coefs(spec4, "4_PPML_3way"),
  extract_coefs(spec5, "5_PPML_3way_MW")
))
fwrite(main_csv, file.path(out_tab("Estimation"), "tab_main_progression.csv"))


# =============================================================================
# SECTION 3 : Heterogeneite sectorielle
# =============================================================================

log_step("Section 3 : heterogeneite strategic vs non-strategic.")

log_step("  Spec 6 : PPML 3-way FE sur strategic_trade_value.")
tic()
spec6_strat <- fepois(
  strategic_trade_value ~ ipd + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec6_strat), "\n")

log_step("  Spec 7 : PPML 3-way FE sur non_strategic_trade.")
tic()
spec7_nonstrat <- fepois(
  non_strategic_trade ~ ipd + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec7_nonstrat), "\n")

etable(spec4, spec6_strat, spec7_nonstrat,
       title = "IPD Effect: Total vs Strategic vs Non-Strategic Trade",
       headers = c("Total", "Strategic", "Non-Strategic"),
       dict = DICT,
       tex = TRUE, replace = TRUE,
       file = file.path(out_tab("Estimation"), "tab_strategic_hetero.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec4,           "Total"),
  extract_coefs(spec6_strat,     "Strategic"),
  extract_coefs(spec7_nonstrat,  "NonStrategic"))),
  file.path(out_tab("Estimation"), "tab_strategic_hetero.csv"))


# =============================================================================
# SECTION 4 : Heterogeneite NATO
# =============================================================================

log_step("Section 4 : heterogeneite par pair_nato.")

df[, pair_nato := factor(pair_nato, levels = c("non", "inter", "intra"))]

log_step("  Spec 8 : IPD x pair_nato sur total trade.")
tic()
spec8_nato <- fepois(
  trade_value ~ i(pair_nato, ipd) + rta
                | exp_year + imp_year + pair,
  data = df[!is.na(ipd) & !is.na(pair_nato)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec8_nato), "\n")

log_step("  Spec 9 : IPD x pair_nato sur strategic trade.")
tic()
spec9_nato_strat <- fepois(
  strategic_trade_value ~ i(pair_nato, ipd) + rta
                          | exp_year + imp_year + pair,
  data = df[!is.na(ipd) & !is.na(pair_nato)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec9_nato_strat), "\n")

etable(spec8_nato, spec9_nato_strat,
       title = "IPD Effect by NATO Pair Type",
       headers = c("Total Trade", "Strategic Trade"),
       dict = c(DICT,
                `pair_nato::non:ipd`   = "IPD x non-NATO",
                `pair_nato::inter:ipd` = "IPD x inter-NATO",
                `pair_nato::intra:ipd` = "IPD x intra-NATO"),
       tex = TRUE, replace = TRUE,
       file = file.path(out_tab("Estimation"), "tab_nato_hetero.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec8_nato,        "Total_x_NATO"),
  extract_coefs(spec9_nato_strat,  "Strategic_x_NATO"))),
  file.path(out_tab("Estimation"), "tab_nato_hetero.csv"))


# =============================================================================
# SECTION 5 : IPD time-varying par periode
# =============================================================================

log_step("Section 5 : IPD time-varying.")

df[, period := fcase(
  year <= 2007, "1995-2007",
  year <= 2013, "2008-2013",
  year <= 2017, "2014-2017",
  year <= 2021, "2018-2021",
  default      = "2022-2024"
)]
df[, period := factor(period, levels = c("1995-2007", "2008-2013",
                                          "2014-2017", "2018-2021",
                                          "2022-2024"))]

log_step("  Spec 10 : IPD x period.")
tic()
spec10_timevar <- fepois(
  trade_value ~ i(period, ipd) + rta
                | exp_year + imp_year + pair,
  data = df[!is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(spec10_timevar), "\n")

etable(spec10_timevar,
       title = "Time-Varying IPD Effect",
       dict = c(DICT,
                `period::1995-2007:ipd` = "IPD x 1995-2007",
                `period::2008-2013:ipd` = "IPD x 2008-2013",
                `period::2014-2017:ipd` = "IPD x 2014-2017",
                `period::2018-2021:ipd` = "IPD x 2018-2021",
                `period::2022-2024:ipd` = "IPD x 2022-2024"),
       tex = TRUE, replace = TRUE,
       file = file.path(out_tab("Estimation"), "tab_timevarying.tex"))

# Coefplot
ct10 <- as.data.table(coeftable(spec10_timevar), keep.rownames = "term")
setnames(ct10, 2:5, c("estimate", "se", "stat", "p"))
ct10 <- ct10[grepl("period.*ipd", term)]
ct10[, period := factor(sub(".*::([0-9-]+):.*", "\\1", term),
                        levels = c("1995-2007", "2008-2013",
                                   "2014-2017", "2018-2021",
                                   "2022-2024"))]
ct10[, `:=`(ci_lo = estimate - 1.96 * se,
            ci_hi = estimate + 1.96 * se)]

fwrite(ct10[, .(period, estimate, se, ci_lo, ci_hi)],
       file.path(out_tab("Estimation"), "tab_timevarying.csv"))

p_time <- ggplot(ct10, aes(period, estimate)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2,
                color = "#2166AC", size = 0.8) +
  geom_point(size = 3, color = "#2166AC") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Time-Varying Effect of Geopolitical Distance on Trade",
       subtitle = "PPML with three-way fixed effects. Cluster-robust 95% CI.",
       x = NULL, y = "IPD semi-elasticity",
       caption = "Source: BACI-CEPII, Bailey et al. (2017), NATO")
ggsave(file.path(out_fig("Estimation"), "est_fig01_ipd_timevarying.png"),
       p_time, width = 10, height = 6, dpi = 300)


# =============================================================================
# SECTION 6 : Diagnostics post-estimation
# =============================================================================

log_step("Section 6 : diagnostics post-estimation.")

# Summary table
get_ipd <- function(model, label) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ipd_row <- ct[term == "ipd"][1]
  if (nrow(ipd_row) == 0) {
    # Cas des specs avec interactions : pas de coef 'ipd' brut
    return(data.table(spec = label, N = nobs(model),
                      ipd_coef = NA, ipd_se = NA, ipd_p = NA,
                      pseudo_r2 = tryCatch(r2(model, type = "pr2"),
                                           error = function(e) NA)))
  }
  data.table(spec = label, N = nobs(model),
             ipd_coef  = ipd_row$estimate,
             ipd_se    = ipd_row$se,
             ipd_p     = ipd_row$p,
             pseudo_r2 = tryCatch(r2(model, type = "pr2"),
                                  error = function(e) {
                                    tryCatch(r2(model, type = "r2"),
                                             error = function(e2) NA)
                                  }))
}

diag_tab <- rbindlist(list(
  get_ipd(spec1,            "1_OLS"),
  get_ipd(spec2,            "2_PPML_sep"),
  get_ipd(spec3,            "3_PPML_ctry_yr"),
  get_ipd(spec4,            "4_PPML_3way"),
  get_ipd(spec5,            "5_PPML_3way_MW"),
  get_ipd(spec6_strat,      "6_Strategic"),
  get_ipd(spec7_nonstrat,   "7_NonStrategic"),
  get_ipd(spec8_nato,       "8_x_NATO"),
  get_ipd(spec9_nato_strat, "9_x_NATO_strat"),
  get_ipd(spec10_timevar,   "10_TimeVar")
))
fwrite(diag_tab, file.path(out_tab("Estimation"), "tab_diagnostics.csv"))
writeLines(as.character(
  kbl(diag_tab, format = "latex", booktabs = TRUE, digits = 4,
      caption = "Summary of all estimated specifications",
      label = "tab:diagnostics") |>
  kable_styling(latex_options = c("scale_down"))),
  file.path(out_tab("Estimation"), "tab_diagnostics.tex"))

# Comparaison SE Spec 4 vs Spec 5
se_compare <- merge(
  as.data.table(coeftable(spec4), keep.rownames = "term"
                )[, .(term, se_pair = `Std. Error`)],
  as.data.table(coeftable(spec5), keep.rownames = "term"
                )[, .(term, se_multiway = `Std. Error`)],
  by = "term")
se_compare[, ratio := se_multiway / se_pair]
cat("\n  - Comparison SE Spec 4 (pair) vs Spec 5 (multiway):\n")
print(se_compare)
fwrite(se_compare, file.path(out_tab("Estimation"), "tab_se_comparison.csv"))

# Test d'egalite Spec 6 (strategic) vs Spec 7 (non-strategic) sur IPD
ipd6 <- as.data.table(coeftable(spec6_strat),
                      keep.rownames = "term")[term == "ipd"]
ipd7 <- as.data.table(coeftable(spec7_nonstrat),
                      keep.rownames = "term")[term == "ipd"]
delta    <- ipd6$Estimate - ipd7$Estimate
delta_se <- sqrt(ipd6$`Std. Error`^2 + ipd7$`Std. Error`^2)
delta_z  <- delta / delta_se
delta_p  <- 2 * pnorm(-abs(delta_z))
cat(sprintf("\n  - Test diff IPD coefs Strategic vs Non-Strategic:\n"))
cat(sprintf("    diff = %.4f (SE %.4f), z = %.3f, p = %.4f\n",
            delta, delta_se, delta_z, delta_p))


# =============================================================================
# SECTION 7 : Robustness checks
# =============================================================================

log_step("Section 7 : robustness checks.")

log_step("  Rob 1 : IPD + IPD^2 (non-linearite).")
tic()
rob1 <- fepois(
  trade_value ~ ipd + ipd_sq + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd)],
  vcov = ~pair
)
cat("    Time:", toc(), "s\n")

log_step("  Rob 2 : exclure micro-Etats (<1M habitants).")
tic()
rob2 <- fepois(
  trade_value ~ ipd + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd) & exp_pop > 1e6 & imp_pop > 1e6],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(rob2), "\n")

log_step("  Rob 3 : post-2002 uniquement.")
tic()
rob3 <- fepois(
  trade_value ~ ipd + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd) & year >= 2002],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(rob3), "\n")

log_step("  Rob 4 : exclure USA/CHN/RUS.")
tic()
big3 <- c("USA", "CHN", "RUS")
rob4 <- fepois(
  trade_value ~ ipd + rta | exp_year + imp_year + pair,
  data = df[!is.na(ipd) &
            !(exp_iso3 %in% big3) & !(imp_iso3 %in% big3)],
  vcov = ~pair
)
cat("    Time:", toc(), "s | N =", nobs(rob4), "\n")

etable(spec4, rob1, rob2, rob3, rob4,
       title = "Robustness Checks",
       headers = c("Baseline", "IPD$^2$", "No micro",
                   "Post-2002", "Excl. US/CN/RU"),
       dict = DICT,
       tex = TRUE, replace = TRUE,
       file = file.path(out_tab("Estimation"), "tab_robustness.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec4, "Baseline"),
  extract_coefs(rob1,  "IPD_sq"),
  extract_coefs(rob2,  "No_micro"),
  extract_coefs(rob3,  "Post_2002"),
  extract_coefs(rob4,  "Excl_US_CN_RU"))),
  file.path(out_tab("Estimation"), "tab_robustness.csv"))


# ---- Fin du backbone gravite (04) ------------------------------------------

log_step("Backbone gravite (04) termine.")
cat("\nTables (", out_tab("Estimation"), ") :\n", sep = "")
print(list.files(out_tab("Estimation")))
cat("\nFigures (", out_fig("Estimation"), ") :\n", sep = "")
print(list.files(out_fig("Estimation")))


# =============================================================================
# ===== [from 10_event_study_sanctions.R] ====================================
# Event study staggered de l'effet des sanctions (choc geopolitique) sur le
# commerce bilateral, cadre gravite PPML. PHASE 1 (minimum viable).
#
# Spec PPML identique a 04 : FE exp_iso3^year + imp_iso3^year + pair,
# cluster = ~pair, zeros gardes. Traitement = sanction NON-COMMERCIALE
# (sanction_nontrade, GSDB-R4) pour eviter la tautologie de l'embargo.
#
#   Etape 0 : construction du traitement (onset, cohort, treated_post)
#   Etape 1 : validation descriptive du traitement (cohortes, Russie)
#   Etape 2 : DiD statique (ancre) + contraste par type (any/trade/non-trade)
#   Etape 3 : event study dynamique Sun & Abraham (sunab)
# =============================================================================


# =============================================================================
# ETAPE 0 : Construction du traitement
# =============================================================================

log_step("Etape 0 : chargement et construction du traitement.")
# NOTE : 10 charge son PROPRE panel (iv_panel.parquet) et reconstruit pair ;
# distinct du panel strategic charge plus haut (04). On garde les deux loads.
df <- read_parquet_safe(PATH_IV_PANEL)
cat("  - Obs total                :", nrow(df), "\n")
cat("  - Annees                   :", paste(range(df$year), collapse = "-"), "\n")

# Conventions de 04 : pair directionnel pour les FE.
df[, pair := paste(exp_iso3, imp_iso3, sep = "_")]

# sanction_nontrade est SYMETRIQUE (verifie : 0 paire en desaccord entre les
# deux directions). On definit l'onset au niveau de la PAIRE NON ORDONNEE.
df[, pkey := ifelse(exp_iso3 < imp_iso3,
                    paste(exp_iso3, imp_iso3, sep = "_"),
                    paste(imp_iso3, exp_iso3, sep = "_"))]

# Onset = premiere annee ou la paire (non ordonnee) est sous sanction non-trade.
onset <- df[sanction_nontrade == 1, .(onset_year = min(year)), by = pkey]
df <- merge(df, onset, by = "pkey", all.x = TRUE)

df[, ever_sanctioned := !is.na(onset_year)]
df[, rel_time := ifelse(ever_sanctioned, year - onset_year, NA_integer_)]
# Robuste au NA-2024 : derive de l'onset (scalaire fixe par paire), donc les
# flux 2024 restent utilisables meme si l'indicateur en-vigueur est NA en 2024.
df[, treated_post := as.integer(ever_sanctioned & year >= onset_year)]
# Cohorte pour sunab : annee d'onset, 10000 = jamais-traites (controles).
df[, cohort := ifelse(ever_sanctioned, onset_year, 10000L)]

cat("  - Paires (non ordonnees) traitees       :", uniqueN(df[ever_sanctioned == TRUE, pkey]), "\n")
cat("  - Paires (non ordonnees) jamais-traitees :", uniqueN(df[ever_sanctioned == FALSE, pkey]), "\n")


# =============================================================================
# ETAPE 1 : Validation descriptive du traitement (AVANT estimation)
# =============================================================================

log_step("Etape 1 : validation descriptive du traitement.")

# Tailles des cohortes par annee d'onset (au niveau paire non ordonnee).
cohort_sizes <- unique(df[ever_sanctioned == TRUE, .(pkey, onset_year)])[
  , .(n_pairs = .N), by = onset_year][order(onset_year)]
cat("\n  Tailles de cohortes par annee d'onset :\n")
print(cohort_sizes)

n_treated <- uniqueN(df[ever_sanctioned == TRUE, pkey])
n_never   <- uniqueN(df[ever_sanctioned == FALSE, pkey])
n_lc      <- cohort_sizes[onset_year == min(df$year), n_pairs]  # left-censored

# Sanity check Russie : onsets des paires impliquant RUS.
rus <- unique(df[ever_sanctioned == TRUE & grepl("RUS", pkey), .(pkey, onset_year)])
rus_by_year <- rus[, .(n_new_onsets = .N), by = onset_year][order(onset_year)]
cat("\n  Russie - nouvelles paires sanctionnees (onset) par annee :\n")
print(rus_by_year)
cat("  Russie - total partenaires jamais sanctionnes :", nrow(rus), "\n")

# Table de validation : on empile cohortes globales + colonne Russie.
val_tab <- merge(cohort_sizes, rus_by_year, by = "onset_year", all.x = TRUE)
setnames(val_tab, c("onset_year", "n_pairs", "n_new_onsets"),
         c("onset_year", "n_pairs_all", "n_new_onsets_RUS"))
val_tab[is.na(n_new_onsets_RUS), n_new_onsets_RUS := 0L]
fwrite(val_tab, file.path(out_tab("EventStudy"), "tab_treatment_validation.csv"))

# Couverture / caveats (lignes meta).
meta_val <- data.table(
  metric = c("n_pairs_treated", "n_pairs_never_treated",
             "n_pairs_left_censored_onset_min_year",
             "n_RUS_treated_partners", "year_min", "year_max",
             "caveat_NA"),
  value  = c(n_treated, n_never, n_lc, nrow(rus),
             min(df$year), max(df$year),
             "sanction_nontrade NA en 2024 (annee entiere) ; treated_post derive de l'onset reste valide"))
fwrite(meta_val, file.path(out_tab("EventStudy"), "tab_treatment_validation_meta.csv"))


# =============================================================================
# ETAPE 2 : DiD statique (ancre) + contraste par type
# =============================================================================
#   (i)  PPML statique du traitement  -> m_static (treated_post).
#   (ii) Contraste par type (replique col. 2 GSDB-R4 : commercial complet vs
#        partiel vs non-commercial) -> m_any, m_split.

log_step("Etape 2 : DiD statique + contraste par type.")

extract_coefs <- function(model, name) {
  n_obs <- nobs(model)  # capter AVANT de creer la colonne 'model' (sinon
                        # data.table resout 'model' comme la colonne, pas l'objet)
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct[, model := name]
  ct[, N := n_obs]
  ct[]
}

# --- (i) Ancre : effet moyen d'etre sous sanction non-commerciale -----------
log_step("  DiD statique (treated_post, non-commercial).")
tic()
m_static <- fepois(trade_value ~ treated_post + rta |
                     exp_iso3^year + imp_iso3^year + pair,
                   data = df, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_static), "\n")

# --- (ii) Contraste par type (replique col. (2) GSDB-R4, Yalcin et al. 2025) -
log_step("  Contraste : sanction_any.")
tic()
m_any <- fepois(trade_value ~ sanction_any + rta |
                  exp_iso3^year + imp_iso3^year + pair,
                data = df, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_any), "\n")

log_step("  Contraste : sanction_trade + sanction_nontrade.")
tic()
m_split <- fepois(trade_value ~ sanction_trade + sanction_nontrade + rta |
                    exp_iso3^year + imp_iso3^year + pair,
                  data = df, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_split), "\n")

static_csv <- rbindlist(list(
  extract_coefs(m_static, "static_treated_post"),
  extract_coefs(m_any,    "contrast_any"),
  extract_coefs(m_split,  "contrast_trade_nontrade")))
fwrite(static_csv, file.path(out_tab("EventStudy"), "tab_static_did.csv"))
cat("\n  --- Resume DiD statique + contraste ---\n")
print(static_csv[!grepl("^rta$", term), .(model, term, estimate = round(estimate, 4),
                                           se = round(se, 4), p = round(p, 4))])


# =============================================================================
# (iii) 2x2 condamnation x sanction — NON ENCORE DISPONIBLE
# =============================================================================
## TODO (feuille de route §3-iii) : construire le veritable 2x2 en interaction
## (condamne seulement / sanctionne seulement / les deux / ni l'un ni l'autre).
## Ce contraste croise la variable `condamne` (vote ONU de condamnation,
## produite par 04_build_un_votes.R — PAS encore construite) avec l'indicateur
## de sanction (p.ex. sanction_nontrade / sanction_any ci-dessus). Specification
## envisagee, dans le meme cadre PPML 3-way FE :
##   fepois(trade_value ~ i(interaction(condamne, sanctionne)) + rta |
##            exp_iso3^year + imp_iso3^year + pair, data = df, cluster = ~ pair)
## avec la categorie de reference "ni l'un ni l'autre".
## Tant que les votes ONU (`condamne`) ne sont pas disponibles, ce 2x2 ne peut
## pas etre estime : NE PAS fabriquer de resultats. Aucune des deux sources
## (04_gravity_estimation.R, 10_event_study_sanctions.R) ne construit ce 2x2 ;
## il reste a implementer une fois `condamne` disponible.


# =============================================================================
# ETAPE 3 : Event study dynamique (Sun & Abraham)  [roadmap §3-iv]
# =============================================================================
#   (iv) Event study Sun & Abraham (sunab) — a lire a partir de k=+1.

log_step("Etape 3 : event study Sun & Abraham (sunab).")

# Fenetre event study : 2008-2023 (cohorente avec le dCDH ; donne 6 ans de
# pre-periode a la vague d'onset 2014). On exclut :
#   - les paires dont l'onset precede la fenetre (left-censored in-window : pas
#     de rel_time = -1, ne contribuent pas aux pre-tendances) ;
#   en gardant les jamais-traites (cohort = 10000) comme controles.
# Cela reduit fortement le nombre de cohortes (donc de termes sunab) -> tractable
# sur 8 Go : le full 1995-2023 (~27 cohortes, ~400 termes) saturait la RAM.
ES_Y0 <- 2008L
df_es <- df[year >= ES_Y0 & (cohort == 10000L | onset_year >= ES_Y0 + 1L)]
cat("  - Obs full :", nrow(df), " | obs event study (", ES_Y0, "-2023, onset>=",
    ES_Y0 + 1L, ") :", nrow(df_es),
    "| cohortes traitees :", uniqueN(df_es[ever_sanctioned == TRUE, cohort]), "\n")

# Binning des extremites de l'event-time pour limiter le nombre de termes et
# stabiliser les bords. On INLINE les bornes numeriques dans la formule : fixest
# evalue sunab() dans l'env. de la formule et ne resout pas une variable-liste
# externe (bin.rel = <var> -> erreur).
rel_rng <- range(df_es[ever_sanctioned == TRUE, rel_time])
fml_es <- as.formula(sprintf(
  "trade_value ~ sunab(cohort, year, bin.rel = list('-5' = %d:-5, '5' = 5:%d)) + rta | exp_iso3^year + imp_iso3^year + pair",
  rel_rng[1], rel_rng[2]))

tic()
m_es <- fepois(fml_es, data = df_es, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_es), "\n")

# ATT agrege.
att <- summary(m_es, agg = "att")
att_ct <- as.data.table(att$coeftable, keep.rownames = "term")
setnames(att_ct, 2:5, c("estimate", "se", "stat", "p"))
cat("\n  --- ATT agrege ---\n"); print(att_ct)

# Coefficients event-time.
ct_es <- as.data.table(coeftable(m_es), keep.rownames = "term")
setnames(ct_es, 2:5, c("estimate", "se", "stat", "p"))
es_dyn <- ct_es[grepl("year::", term)]
es_dyn[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
es_dyn[, `:=`(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]
setorder(es_dyn, rel_time)
cat("\n  --- Coefficients event-time ---\n")
print(es_dyn[, .(rel_time, estimate = round(estimate, 4), se = round(se, 4),
                 ci_lo = round(ci_lo, 4), ci_hi = round(ci_hi, 4))])

# Sortie combinee : event-time + ligne ATT.
out_es <- rbindlist(list(
  es_dyn[, .(term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
  att_ct[, .(term = "ATT", rel_time = NA_integer_, estimate, se,
             ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]),
  use.names = TRUE)
fwrite(out_es, file.path(out_tab("EventStudy"), "tab_eventstudy_sunab.csv"))

# --- Figure maitresse : event-time (ggplot, fenetre lisible) ----------------
p_es <- ggplot(es_dyn, aes(rel_time, estimate)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = -0.5, lty = 3, color = "grey60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166AC") +
  geom_line(color = "#2166AC", linewidth = 0.6) +
  geom_point(color = "#2166AC", size = 2) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Effet des sanctions non-commerciales sur le commerce bilateral",
       subtitle = "Event study Sun & Abraham. PPML 3-way FE. IC 95% (cluster paire). k=0 = transition.",
       x = "Temps relatif a l'onset de la sanction (annees)",
       y = "Semi-elasticite (effet sur log trade)",
       caption = "Source : BACI-CEPII, GSDB-R4 (Yalcin et al. 2025). Fenetre 2008-2023, bornes binnees a +/-5.")
ggsave(file.path(out_fig("EventStudy"), "es_fig01_sunab_2014.png"),
       p_es, width = 10, height = 6, dpi = 300)


# =============================================================================
# Resume console
# =============================================================================

log_step("Termine. Sorties :")
cat("  Tables  :", out_tab("EventStudy"), "\n");  print(list.files(out_tab("EventStudy")))
cat("  Figures :", out_fig("EventStudy"), "\n");  print(list.files(out_fig("EventStudy")))
