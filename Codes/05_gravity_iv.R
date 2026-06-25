# =============================================================================
# 05_gravity_iv.R
# -----------------------------------------------------------------------------
# Control function IV pour le modele de gravite PPML (Wooldridge-Terza).
# Instrument : distance euclidienne aux alignements geopolitiques USA/CHN/RUS,
# laggee (lag 2 = spec principale). Pairs avec ces poles exclues.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "ggplot2", "scales", "kableExtra")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
  library(ggplot2); library(scales); library(kableExtra)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_DATA <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "Estimation_IV")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "Estimation_IV")
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)
setFixest_etable(markdown = FALSE)

log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

theme_memoir <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
theme_set(theme_memoir)


# ---- Section 0bis : Load + prep -------------------------------------------

log_step("Chargement et preparation du panel.")
df <- as.data.table(read_parquet(PATH_DATA))

df[, pair      := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year  := paste(exp_iso3, year,     sep = "_")]
df[, imp_year  := paste(imp_iso3, year,     sep = "_")]
df[, log_dist  := log(dist)]
df[, ipd_sq    := ipd^2]
df[, non_strategic_trade := pmax(trade_value - strategic_trade_value, 0)]
df[, period := fcase(year <= 2007, "1995-2007",
                     year <= 2013, "2008-2013",
                     year <= 2017, "2014-2017",
                     year <= 2021, "2018-2021",
                     default      = "2022-2024")]
df[, period := factor(period, levels = c("1995-2007", "2008-2013",
                                          "2014-2017", "2018-2021",
                                          "2022-2024"))]

cat("  - Obs total :", nrow(df), "\n")


# =============================================================================
# SECTION 1 : Construction des instruments
# =============================================================================

log_step("Section 1 : construction des instruments (grille de lags).")

poles <- c("USA", "CHN", "RUS")

# Alignement de chaque pays aux 3 poles, par annee
align <- df[imp_iso3 %in% poles & !is.na(ipd),
            .(iso3 = exp_iso3, pole = imp_iso3, year, ipd)]
align <- unique(align)
align_wide <- dcast(align, iso3 + year ~ pole, value.var = "ipd")
setnames(align_wide, poles, c("ipd_usa", "ipd_chn", "ipd_rus"))
cat("  - Alignement aux poles : ", nrow(align_wide), "pays-annees,",
    uniqueN(align_wide$iso3), "pays\n")

lags <- c(0, 1, 2, 3, 5)

for (lg in lags) {
  al <- copy(align_wide)
  al[, year_target := year + lg]
  cols_old <- c("ipd_usa", "ipd_chn", "ipd_rus")
  cols_new <- paste0(cols_old, "_l", lg)
  setnames(al, cols_old, cols_new)
  al[, year := NULL]

  # Merge cote exporter
  df <- merge(df, al, by.x = c("exp_iso3", "year"),
              by.y = c("iso3", "year_target"), all.x = TRUE)
  exp_renamed <- paste0("exp_", cols_new)
  setnames(df, cols_new, exp_renamed)

  # Merge cote importer
  df <- merge(df, al, by.x = c("imp_iso3", "year"),
              by.y = c("iso3", "year_target"), all.x = TRUE)
  imp_renamed <- paste0("imp_", cols_new)
  setnames(df, cols_new, imp_renamed)

  # Construire l'instrument (distance euclidienne)
  inst_name <- paste0("instrument_l", lg)
  df[, (inst_name) := sqrt(
    (get(exp_renamed[1]) - get(imp_renamed[1]))^2 +
    (get(exp_renamed[2]) - get(imp_renamed[2]))^2 +
    (get(exp_renamed[3]) - get(imp_renamed[3]))^2)]

  n_ok <- sum(!is.na(df[[inst_name]]))
  cat(sprintf("  - Lag %d : %d obs avec instrument non-NA\n", lg, n_ok))
}

# Echantillon IV : exclure les paires impliquant un pole
df_iv <- df[!(exp_iso3 %in% poles) & !(imp_iso3 %in% poles) & !is.na(ipd)]
cat("  - Echantillon IV (hors poles) :", nrow(df_iv), "obs\n")

# Diagnostics : correlation instrument vs ipd
cat("\n  - Diagnostics des instruments :\n")
for (lg in lags) {
  inst_name <- paste0("instrument_l", lg)
  d <- df_iv[!is.na(get(inst_name)), .(ipd, inst = get(inst_name))]
  cat(sprintf("    Lag %d : N=%d, cor(inst, ipd)=%.4f, mean=%.3f, sd=%.3f, min=%.3f, max=%.3f\n",
              lg, nrow(d), cor(d$inst, d$ipd),
              mean(d$inst), sd(d$inst), min(d$inst), max(d$inst)))
}


# =============================================================================
# SECTION 2 : First stage - grille complete
# =============================================================================

log_step("Section 2 : first stage par lag.")

fs_results <- list()
for (lg in lags) {
  inst_name <- paste0("instrument_l", lg)
  df_sub <- df_iv[!is.na(get(inst_name))]
  tic()
  fs <- feols(
    as.formula(paste0("ipd ~ ", inst_name,
                      " + rta | exp_year + imp_year + pair")),
    data = df_sub, vcov = ~pair, notes = FALSE)
  ct <- as.data.table(coeftable(fs), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  row <- ct[term == inst_name]
  f_stat <- row$stat^2
  fs_results[[as.character(lg)]] <- data.table(
    lag = lg, coef = row$estimate, se = row$se,
    t_stat = row$stat, F_stat = f_stat, N = nobs(fs))
  cat(sprintf("  Lag %d : coef=%.4f SE=%.4f F=%.1f N=%d (%.1fs)\n",
              lg, row$estimate, row$se, f_stat, nobs(fs), toc()))
  print(summary(fs))
}
fs_summary <- rbindlist(fs_results)

# Table
fwrite(fs_summary, file.path(PATH_TAB, "tab_iv_first_stage_lags.csv"))
writeLines(as.character(
  kbl(fs_summary, format = "latex", booktabs = TRUE, digits = 4,
      caption = "First Stage: Instrument Strength by Lag",
      label = "tab:iv_fs_lags") |>
  kable_styling(latex_options = c("hold_position"))),
  file.path(PATH_TAB, "tab_iv_first_stage_lags.tex"))

# Figure F-stat par lag
p0 <- ggplot(fs_summary, aes(lag, F_stat)) +
  geom_point(size = 3, color = "#2166AC") +
  geom_line(color = "#2166AC") +
  geom_hline(yintercept = 10, lty = 2, color = "red") +
  annotate("text", x = max(lags), y = 10, label = "Weak IV threshold",
           vjust = -0.5, color = "red", size = 3, hjust = 1) +
  labs(title = "First-Stage F-Statistic by Instrument Lag",
       subtitle = "Instrument: Euclidean distance in alignment to US/CN/RU",
       x = "Lag (years)", y = "F-statistic",
       caption = "Source: Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "iv_fig00_fstat_by_lag.png"),
       p0, width = 8, height = 5, dpi = 300)


# =============================================================================
# SECTION 3 : Control function - spec principale (lag 2)
# =============================================================================

log_step("Section 3 : control function spec principale (lag 2).")

df_iv2 <- df_iv[!is.na(instrument_l2)]

# First stage principal
tic()
fs_main <- feols(ipd ~ instrument_l2 + rta | exp_year + imp_year + pair,
                 data = df_iv2, vcov = ~pair, notes = FALSE)
cat("  First stage time:", toc(), "s, N =", nobs(fs_main), "\n")
print(summary(fs_main))

# Verifier alignement
if (nobs(fs_main) != nrow(df_iv2)) {
  warning(sprintf("nobs(fs_main)=%d != nrow(df_iv2)=%d",
                  nobs(fs_main), nrow(df_iv2)))
  df_iv2 <- df_iv2[!is.na(predict(fs_main, newdata = df_iv2))]
  fs_main <- feols(ipd ~ instrument_l2 + rta | exp_year + imp_year + pair,
                   data = df_iv2, vcov = ~pair, notes = FALSE)
}
df_iv2[, v_hat := residuals(fs_main)]

# PPML standard sur meme echantillon (comparabilite)
tic()
spec4_same <- fepois(trade_value ~ ipd + rta | exp_year + imp_year + pair,
                     data = df_iv2, vcov = ~pair)
cat("  PPML same sample time:", toc(), "s, N =", nobs(spec4_same), "\n")
print(summary(spec4_same))

# Control function
tic()
spec4_iv <- fepois(trade_value ~ ipd + v_hat + rta
                                 | exp_year + imp_year + pair,
                   data = df_iv2, vcov = ~pair)
cat("  CF-IV time:", toc(), "s, N =", nobs(spec4_iv), "\n")
print(summary(spec4_iv))

# Table IV-1
etable(spec4_same, spec4_iv,
       title = "PPML vs Control Function IV (Lag 2)",
       headers = c("PPML", "CF-IV"),
       dict = c(ipd = "IPD", v_hat = "CF residual (endog. test)", rta = "RTA"),
       notes = paste("Control function approach (Wooldridge-Terza).",
                     "Instrument: 2-year lagged Euclidean distance in geopolitical",
                     "alignment to USA, China, Russia. Pairs involving USA, China,",
                     "Russia excluded. SE clustered by pair (not corrected for",
                     "generated regressor)."),
       tex = TRUE, replace = TRUE,
       file = file.path(PATH_TAB, "tab_iv_main.tex"))

extract_coefs <- function(m, name) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct[, model := name][]
}
fwrite(rbindlist(list(
  extract_coefs(spec4_same, "PPML_same_sample"),
  extract_coefs(spec4_iv,   "CF_IV_lag2"))),
  file.path(PATH_TAB, "tab_iv_main.csv"))


# =============================================================================
# SECTION 4 : Sensibilite au lag
# =============================================================================

log_step("Section 4 : sensibilite au lag.")

iv_by_lag <- list()
for (lg in lags) {
  inst_name <- paste0("instrument_l", lg)
  df_sub <- copy(df_iv[!is.na(get(inst_name))])
  tic()
  fs_lg <- feols(
    as.formula(paste0("ipd ~ ", inst_name,
                      " + rta | exp_year + imp_year + pair")),
    data = df_sub, vcov = ~pair, notes = FALSE)
  if (nobs(fs_lg) != nrow(df_sub)) {
    df_sub <- df_sub[!is.na(predict(fs_lg, newdata = df_sub))]
    fs_lg <- feols(
      as.formula(paste0("ipd ~ ", inst_name,
                        " + rta | exp_year + imp_year + pair")),
      data = df_sub, vcov = ~pair, notes = FALSE)
  }
  df_sub[, v_hat_lg := residuals(fs_lg)]

  cf_lg <- fepois(trade_value ~ ipd + v_hat_lg + rta
                                | exp_year + imp_year + pair,
                  data = df_sub, vcov = ~pair)
  iv_by_lag[[as.character(lg)]] <- cf_lg

  ct <- as.data.table(coeftable(cf_lg), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  cat(sprintf("  Lag %d : IPD=%.4f (SE=%.4f), v_hat p=%.4f, N=%d (%.1fs)\n",
              lg,
              ct[term == "ipd", estimate],
              ct[term == "ipd", se],
              ct[term == "v_hat_lg", p],
              nobs(cf_lg), toc()))
}

# Table IV-2
etable(iv_by_lag[["0"]], iv_by_lag[["1"]], iv_by_lag[["2"]],
       iv_by_lag[["3"]], iv_by_lag[["5"]],
       title = "CF-IV Sensitivity to Instrument Lag",
       headers = c("Lag 0", "Lag 1", "Lag 2", "Lag 3", "Lag 5"),
       dict = c(ipd = "IPD", v_hat_lg = "CF residual", rta = "RTA"),
       notes = "Each column uses a different lag for the instrument. SE clustered by pair.",
       tex = TRUE, replace = TRUE,
       file = file.path(PATH_TAB, "tab_iv_lag_sensitivity.tex"))

# Figure : coef IPD par lag
coef_by_lag <- rbindlist(lapply(names(iv_by_lag), function(n) {
  m <- iv_by_lag[[n]]
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ipd_row <- ct[term == "ipd"]
  data.table(lag = as.integer(n), coef = ipd_row$estimate, se = ipd_row$se)
}))
coef_by_lag[, ci_lo := coef - 1.96 * se]
coef_by_lag[, ci_hi := coef + 1.96 * se]

ppml_coef <- as.numeric(coef(spec4_same)["ipd"])

p1 <- ggplot(coef_by_lag, aes(lag, coef)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_hline(yintercept = ppml_coef, lty = 2, color = "#2166AC", alpha = 0.6) +
  annotate("text", x = max(lags), y = ppml_coef,
           label = "PPML (no IV)", vjust = -0.5, color = "#2166AC",
           size = 3, hjust = 1) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2,
                color = "#B2182B") +
  geom_point(size = 3, color = "#B2182B") +
  labs(title = "IPD Coefficient Sensitivity to Instrument Lag",
       subtitle = "CF-IV with three-way FE. Dashed line = PPML without IV.",
       x = "Instrument lag (years)", y = "IPD coefficient",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "iv_fig01_coef_by_lag.png"),
       p1, width = 8, height = 5, dpi = 300)

fwrite(coef_by_lag, file.path(PATH_TAB, "tab_iv_coef_by_lag.csv"))


# =============================================================================
# SECTION 5 : Heterogeneite sectorielle (IV, lag 2)
# =============================================================================

log_step("Section 5 : heterogeneite sectorielle IV.")

tic()
spec6_iv <- fepois(strategic_trade_value ~ ipd + v_hat + rta
                                          | exp_year + imp_year + pair,
                   data = df_iv2, vcov = ~pair)
cat("  Strategic IV time:", toc(), "s, N =", nobs(spec6_iv), "\n")
print(summary(spec6_iv))

tic()
spec7_iv <- fepois(non_strategic_trade ~ ipd + v_hat + rta
                                        | exp_year + imp_year + pair,
                   data = df_iv2, vcov = ~pair)
cat("  Non-strategic IV time:", toc(), "s, N =", nobs(spec7_iv), "\n")
print(summary(spec7_iv))

etable(spec4_iv, spec6_iv, spec7_iv,
       title = "CF-IV: Total vs Strategic vs Non-Strategic Trade",
       headers = c("Total", "Strategic", "Non-Strategic"),
       dict = c(ipd = "IPD", v_hat = "CF residual", rta = "RTA"),
       notes = "CF-IV with 2-year lagged instrument. SE clustered by pair.",
       tex = TRUE, replace = TRUE,
       file = file.path(PATH_TAB, "tab_iv_strategic.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec4_iv, "Total_IV"),
  extract_coefs(spec6_iv, "Strategic_IV"),
  extract_coefs(spec7_iv, "NonStrategic_IV"))),
  file.path(PATH_TAB, "tab_iv_strategic.csv"))


# =============================================================================
# SECTION 6 : Heterogeneite NATO (IV, lag 2)
# =============================================================================

log_step("Section 6 : heterogeneite NATO IV.")

df_iv2[, pair_nato := factor(pair_nato, levels = c("non", "inter", "intra"))]
df_iv2_n <- df_iv2[!is.na(pair_nato)]

# First stage avec instrument interagi
tic()
fs_nato <- feols(ipd ~ i(pair_nato, instrument_l2) + rta
                       | exp_year + imp_year + pair,
                 data = df_iv2_n, vcov = ~pair, notes = FALSE)
cat("  FS NATO time:", toc(), "s, N =", nobs(fs_nato), "\n")
print(summary(fs_nato))
if (nobs(fs_nato) != nrow(df_iv2_n)) {
  df_iv2_n <- df_iv2_n[!is.na(predict(fs_nato, newdata = df_iv2_n))]
  fs_nato <- feols(ipd ~ i(pair_nato, instrument_l2) + rta
                         | exp_year + imp_year + pair,
                   data = df_iv2_n, vcov = ~pair, notes = FALSE)
}
df_iv2_n[, v_hat_nato := residuals(fs_nato)]

# CF avec interactions
tic()
spec8_iv <- fepois(trade_value ~ i(pair_nato, ipd) + v_hat_nato + rta
                                 | exp_year + imp_year + pair,
                   data = df_iv2_n, vcov = ~pair)
cat("  Spec 8 IV time:", toc(), "s\n")
print(summary(spec8_iv))

tic()
spec9_iv <- fepois(strategic_trade_value ~ i(pair_nato, ipd) + v_hat_nato + rta
                                          | exp_year + imp_year + pair,
                   data = df_iv2_n, vcov = ~pair)
cat("  Spec 9 IV time:", toc(), "s\n")
print(summary(spec9_iv))

etable(spec8_iv, spec9_iv,
       title = "CF-IV: IPD Effect by NATO Pair Type",
       headers = c("Total Trade", "Strategic Trade"),
       dict = c(rta = "RTA", v_hat_nato = "CF residual",
                `pair_nato::non:ipd`   = "IPD x non-NATO",
                `pair_nato::inter:ipd` = "IPD x inter-NATO",
                `pair_nato::intra:ipd` = "IPD x intra-NATO"),
       notes = "CF-IV with 2-year lagged instrument. SE clustered by pair.",
       tex = TRUE, replace = TRUE,
       file = file.path(PATH_TAB, "tab_iv_nato.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec8_iv, "Total_NATO_IV"),
  extract_coefs(spec9_iv, "Strategic_NATO_IV"))),
  file.path(PATH_TAB, "tab_iv_nato.csv"))


# =============================================================================
# SECTION 7 : Time-varying (IV, lag 2) + comparison PPML
# =============================================================================

log_step("Section 7 : time-varying IV.")

tic()
spec10_iv <- fepois(trade_value ~ i(period, ipd) + v_hat + rta
                                  | exp_year + imp_year + pair,
                    data = df_iv2, vcov = ~pair)
cat("  Spec 10 IV time:", toc(), "s\n")
print(summary(spec10_iv))

# Equivalent PPML sans IV sur meme echantillon
tic()
spec10_ppml <- fepois(trade_value ~ i(period, ipd) + rta
                                    | exp_year + imp_year + pair,
                      data = df_iv2, vcov = ~pair)
cat("  Spec 10 PPML same sample time:", toc(), "s\n")

etable(spec10_ppml, spec10_iv,
       title = "Time-Varying IPD Effect: PPML vs CF-IV",
       headers = c("PPML", "CF-IV"),
       dict = c(v_hat = "CF residual", rta = "RTA",
                `period::1995-2007:ipd` = "IPD x 1995-2007",
                `period::2008-2013:ipd` = "IPD x 2008-2013",
                `period::2014-2017:ipd` = "IPD x 2014-2017",
                `period::2018-2021:ipd` = "IPD x 2018-2021",
                `period::2022-2024:ipd` = "IPD x 2022-2024"),
       notes = "Both columns on the same IV sample (df_iv2). CF-IV with 2-year lagged instrument.",
       tex = TRUE, replace = TRUE,
       file = file.path(PATH_TAB, "tab_iv_timevarying.tex"))

# Figure : PPML vs IV par periode
extract_periods <- function(m, label) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct <- ct[grepl("period.*ipd", term)]
  ct[, period := factor(sub(".*::([0-9-]+):.*", "\\1", term),
                        levels = c("1995-2007", "2008-2013",
                                   "2014-2017", "2018-2021",
                                   "2022-2024"))]
  ct[, model := label]
  ct[, .(model, period, estimate, se,
         ci_lo = estimate - 1.96 * se,
         ci_hi = estimate + 1.96 * se)]
}
tv_df <- rbind(extract_periods(spec10_ppml, "PPML"),
               extract_periods(spec10_iv,   "CF-IV"))

p2 <- ggplot(tv_df, aes(period, estimate, color = model)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2,
                position = position_dodge(width = 0.4)) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("PPML" = "#2166AC", "CF-IV" = "#B2182B")) +
  labs(title = "Time-Varying Effect of IPD: PPML vs Control Function IV",
       subtitle = "Same IV sample. 95% CI cluster-robust by pair.",
       x = NULL, y = "IPD semi-elasticity", color = NULL,
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "iv_fig02_timevarying_comparison.png"),
       p2, width = 10, height = 6, dpi = 300)

fwrite(tv_df, file.path(PATH_TAB, "tab_iv_timevarying.csv"))


# =============================================================================
# SECTION 8 : Instruments alternatifs
# =============================================================================

log_step("Section 8 : instruments alternatifs.")

# Rob-IV 1 : 3 instruments separes (over-identification)
df_iv2[, z_usa := exp_ipd_usa_l2 * imp_ipd_usa_l2]
df_iv2[, z_chn := exp_ipd_chn_l2 * imp_ipd_chn_l2]
df_iv2[, z_rus := exp_ipd_rus_l2 * imp_ipd_rus_l2]

tic()
fs_3iv <- feols(ipd ~ z_usa + z_chn + z_rus + rta
                      | exp_year + imp_year + pair,
                data = df_iv2, vcov = ~pair, notes = FALSE)
cat("  FS 3 IVs time:", toc(), "s\n")
print(summary(fs_3iv))
df_iv2[, v_hat_3iv := residuals(fs_3iv)]

tic()
spec4_iv_3 <- fepois(trade_value ~ ipd + v_hat_3iv + rta
                                  | exp_year + imp_year + pair,
                     data = df_iv2, vcov = ~pair)
cat("  CF 3 IVs time:", toc(), "s\n")
print(summary(spec4_iv_3))

# Rob-IV 2 : instrument pre-sample (1995, time-invariant)
align_1995 <- align_wide[year == 1995,
                          .(iso3, ipd_usa_pre = ipd_usa,
                            ipd_chn_pre = ipd_chn,
                            ipd_rus_pre = ipd_rus)]
df_iv2 <- merge(df_iv2, align_1995, by.x = "exp_iso3", by.y = "iso3",
                all.x = TRUE)
setnames(df_iv2, c("ipd_usa_pre", "ipd_chn_pre", "ipd_rus_pre"),
                  c("exp_ipd_usa_pre", "exp_ipd_chn_pre", "exp_ipd_rus_pre"))
df_iv2 <- merge(df_iv2, align_1995, by.x = "imp_iso3", by.y = "iso3",
                all.x = TRUE)
setnames(df_iv2, c("ipd_usa_pre", "ipd_chn_pre", "ipd_rus_pre"),
                  c("imp_ipd_usa_pre", "imp_ipd_chn_pre", "imp_ipd_rus_pre"))
df_iv2[, instrument_pre := sqrt(
  (exp_ipd_usa_pre - imp_ipd_usa_pre)^2 +
  (exp_ipd_chn_pre - imp_ipd_chn_pre)^2 +
  (exp_ipd_rus_pre - imp_ipd_rus_pre)^2)]

df_iv2_pre <- df_iv2[!is.na(instrument_pre)]
tic()
fs_pre <- feols(ipd ~ instrument_pre + rta
                      | exp_year + imp_year + pair,
                data = df_iv2_pre, vcov = ~pair, notes = FALSE)
cat("  FS pre-sample time:", toc(), "s\n")
print(summary(fs_pre))
df_iv2_pre[, v_hat_pre := residuals(fs_pre)]

tic()
spec4_iv_pre <- fepois(trade_value ~ ipd + v_hat_pre + rta
                                    | exp_year + imp_year + pair,
                       data = df_iv2_pre, vcov = ~pair)
cat("  CF pre-sample time:", toc(), "s\n")
print(summary(spec4_iv_pre))

etable(spec4_iv, spec4_iv_3, spec4_iv_pre,
       title = "CF-IV Robustness: Alternative Instruments",
       headers = c("Euclidean L2", "3 Separate IVs", "Pre-sample 1995"),
       dict = c(ipd = "IPD", rta = "RTA",
                v_hat = "CF residual", v_hat_3iv = "CF residual",
                v_hat_pre = "CF residual"),
       notes = paste("SE clustered by pair. Pairs involving USA, China,",
                     "Russia excluded."),
       tex = TRUE, replace = TRUE,
       file = file.path(PATH_TAB, "tab_iv_instrument_robustness.tex"))

fwrite(rbindlist(list(
  extract_coefs(spec4_iv,     "Euclidean_L2"),
  extract_coefs(spec4_iv_3,   "Three_IVs"),
  extract_coefs(spec4_iv_pre, "Pre_sample_1995"))),
  file.path(PATH_TAB, "tab_iv_instrument_robustness.csv"))


# =============================================================================
# SECTION 9 : Resume consolide
# =============================================================================

cat("\n")
cat("==========================================================\n")
cat("              RESUME COMPLET IV ESTIMATION                \n")
cat("==========================================================\n")

cat("\n--- First Stage (by lag) ---\n")
print(fs_summary)

ct4_same <- as.data.table(coeftable(spec4_same), keep.rownames = "term")
setnames(ct4_same, 2:5, c("estimate", "se", "stat", "p"))
ct4_iv   <- as.data.table(coeftable(spec4_iv),   keep.rownames = "term")
setnames(ct4_iv,   2:5, c("estimate", "se", "stat", "p"))

cat("\n--- Main Results (lag 2) ---\n")
cat(sprintf("PPML same sample : IPD = %.4f (SE %.4f, p %.4f)\n",
            ct4_same[term == "ipd", estimate],
            ct4_same[term == "ipd", se],
            ct4_same[term == "ipd", p]))
cat(sprintf("CF-IV            : IPD = %.4f (SE %.4f, p %.4f)\n",
            ct4_iv[term == "ipd", estimate],
            ct4_iv[term == "ipd", se],
            ct4_iv[term == "ipd", p]))
v_p <- ct4_iv[term == "v_hat", p]
cat(sprintf("v_hat            : p = %.4f %s\n", v_p,
            ifelse(v_p < 0.05, ">>> ENDOGENEITY DETECTED",
                   ">>> No endogeneity detected")))
f2 <- fs_summary[lag == 2, F_stat]
cat(sprintf("First-stage F    : %.1f %s\n", f2,
            ifelse(f2 < 10, "WARNING: WEAK INSTRUMENT", "OK")))

ct6 <- as.data.table(coeftable(spec6_iv), keep.rownames = "term")
setnames(ct6, 2:5, c("estimate", "se", "stat", "p"))
ct7 <- as.data.table(coeftable(spec7_iv), keep.rownames = "term")
setnames(ct7, 2:5, c("estimate", "se", "stat", "p"))
cat("\n--- Strategic vs Non-Strategic (IV) ---\n")
cat(sprintf("IPD on strategic     : %.4f (SE %.4f, p %.4f)\n",
            ct6[term == "ipd", estimate],
            ct6[term == "ipd", se],
            ct6[term == "ipd", p]))
cat(sprintf("IPD on non-strategic : %.4f (SE %.4f, p %.4f)\n",
            ct7[term == "ipd", estimate],
            ct7[term == "ipd", se],
            ct7[term == "ipd", p]))

cat("\n--- Sensitivity to Lag ---\n")
for (lg in lags) {
  m  <- iv_by_lag[[as.character(lg)]]
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  cat(sprintf("Lag %d : IPD = %.4f (SE %.4f), v_hat p = %.4f\n",
              lg,
              ct[term == "ipd", estimate],
              ct[term == "ipd", se],
              ct[term == "v_hat_lg", p]))
}
cat("==========================================================\n")

log_step("Termine.")
cat("\nTables :\n");  print(list.files(PATH_TAB))
cat("\nFigures :\n"); print(list.files(PATH_FIG))
