# =============================================================================
# 04_gravity_estimation.R
# -----------------------------------------------------------------------------
# Estimation du modele de gravite PPML avec progression OLS -> PPML FE three-way.
#   Section 1 : within-variation diagnostic de l'IPD
#   Section 2 : 5 specifications progressives (table principale)
#   Section 3 : heterogeneite sectorielle (strategic vs non)
#   Section 4 : heterogeneite NATO (pair_nato interaction)
#   Section 5 : IPD time-varying par periode + coefplot
#   Section 6 : diagnostics post-estimation
#   Section 7 : robustness checks
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

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

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_DATA <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "Estimation")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "Estimation")
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_etable(markdown = FALSE)
setFixest_nthreads(0)  # all available cores

log_step <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

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


# ---- Section 0bis : Preparation des donnees --------------------------------

log_step("Chargement et preparation du panel.")
df <- as.data.table(read_parquet(PATH_DATA))

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
       file = file.path(PATH_TAB, "tab_main_progression.tex"))

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
fwrite(main_csv, file.path(PATH_TAB, "tab_main_progression.csv"))


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
       file = file.path(PATH_TAB, "tab_strategic_hetero.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec4,           "Total"),
  extract_coefs(spec6_strat,     "Strategic"),
  extract_coefs(spec7_nonstrat,  "NonStrategic"))),
  file.path(PATH_TAB, "tab_strategic_hetero.csv"))


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
       file = file.path(PATH_TAB, "tab_nato_hetero.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec8_nato,        "Total_x_NATO"),
  extract_coefs(spec9_nato_strat,  "Strategic_x_NATO"))),
  file.path(PATH_TAB, "tab_nato_hetero.csv"))


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
       file = file.path(PATH_TAB, "tab_timevarying.tex"))

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
       file.path(PATH_TAB, "tab_timevarying.csv"))

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
ggsave(file.path(PATH_FIG, "est_fig01_ipd_timevarying.png"),
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
fwrite(diag_tab, file.path(PATH_TAB, "tab_diagnostics.csv"))
writeLines(as.character(
  kbl(diag_tab, format = "latex", booktabs = TRUE, digits = 4,
      caption = "Summary of all estimated specifications",
      label = "tab:diagnostics") |>
  kable_styling(latex_options = c("scale_down"))),
  file.path(PATH_TAB, "tab_diagnostics.tex"))

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
fwrite(se_compare, file.path(PATH_TAB, "tab_se_comparison.csv"))

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
       file = file.path(PATH_TAB, "tab_robustness.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec4, "Baseline"),
  extract_coefs(rob1,  "IPD_sq"),
  extract_coefs(rob2,  "No_micro"),
  extract_coefs(rob3,  "Post_2002"),
  extract_coefs(rob4,  "Excl_US_CN_RU"))),
  file.path(PATH_TAB, "tab_robustness.csv"))


# ---- Done -------------------------------------------------------------------

log_step("Termine.")
cat("\nTables (", PATH_TAB, ") :\n", sep = ""); print(list.files(PATH_TAB))
cat("\nFigures (", PATH_FIG, ") :\n", sep = ""); print(list.files(PATH_FIG))
