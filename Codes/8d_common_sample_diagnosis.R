# =============================================================================
# 8d_common_sample_diagnosis.R
# -----------------------------------------------------------------------------
# Objectif : comprendre pourquoi, sur le "sample commun" (intersection des 6
# mesures alternatives), tous les coefficients deviennent positifs alors que
# l'IPD est negatif sur le full sample.
# Hypothese : le sample commun est biaise vers de grandes economies developpees,
# diplomatiquement alignees, ou la complementarite economique domine la
# friction geopolitique.
# Conventions de 04_gravity_estimation.R : lecture parquet, fepois, FE 3-way
# exp_year + imp_year + pair (pre-construits), vcov = ~pair.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_IV   <- file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")
PATH_MS   <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables", "Robustness", "_archive")
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)

log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

log_step("Setup termine.")


# ---- Section 1 : Load + flag in_common --------------------------------------

log_step("Section 1 : load iv_panel + merge GDP depuis master_panel.")

df <- as.data.table(read_parquet(PATH_IV))
# Merge GDP/pop pour proxy developpement (absent de iv_panel)
ms <- as.data.table(read_parquet(PATH_MS,
        col_select = c("exp_iso3", "imp_iso3", "year",
                       "exp_gdp_real", "imp_gdp_real",
                       "exp_pop", "imp_pop")))
df <- merge(df, ms, by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
df[, exp_gdppc := exp_gdp_real / exp_pop]
df[, imp_gdppc := imp_gdp_real / imp_pop]
rm(ms); gc(verbose = FALSE)

# IDs FE (conventions de 04)
df[, pair     := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year := paste(exp_iso3, year,     sep = "_")]
df[, imp_year := paste(imp_iso3, year,     sep = "_")]

# Restriction au domaine "IPD non-NA" (le baseline de 04 utilise df[!is.na(ipd)])
df <- df[!is.na(ipd)]
N_full <- nrow(df)
cat("  - N (ipd non-NA) :", N_full, "\n")

# Flag in_common : les 6 mesures non-NA simultanement (idem 08c passe 2)
six_measures <- c("polyarchy_dist", "polity_dist", "allied_atop",
                  "shared_rival_mid", "sanction_nontrade",
                  "n_common_sanctioners")
df[, in_common := Reduce(`&`, lapply(six_measures, function(v) !is.na(df[[v]])))]

N_common <- df[in_common == TRUE, .N]
cat("  - N (sample commun)    :", N_common, "\n")
cat("  - Reference 08c passe 2 :", 476778, "\n")
if (N_common != 476778) {
  cat("  - WARNING : ecart de", N_common - 476778,
      "obs vs ref 08c. A enqueter si non trivial.\n")
} else {
  cat("  - OK : meme echantillon que passe 2 du script 08c.\n")
}


# ---- Section 2 : Table de couverture ---------------------------------------

log_step("Section 2 : table de couverture.")

V_full <- sum(df$trade_value, na.rm = TRUE)
Z_full <- sum(df$trade_value == 0, na.rm = TRUE)

cov_table <- data.table(
  groupe = c("commun", "reste"),
  N = c(N_common, N_full - N_common),
  pct_panel = round(100 * c(N_common, N_full - N_common) / N_full, 2),
  trade_sum = c(df[in_common == TRUE, sum(trade_value, na.rm = TRUE)],
                df[in_common == FALSE, sum(trade_value, na.rm = TRUE)]),
  pct_trade = round(100 * c(df[in_common == TRUE,  sum(trade_value, na.rm = TRUE)],
                            df[in_common == FALSE, sum(trade_value, na.rm = TRUE)]) /
                          V_full, 2),
  n_zeros   = c(df[in_common == TRUE  & trade_value == 0, .N],
                df[in_common == FALSE & trade_value == 0, .N]),
  pct_zeros = round(100 * c(df[in_common == TRUE  & trade_value == 0, .N],
                            df[in_common == FALSE & trade_value == 0, .N]) /
                          c(N_common, N_full - N_common), 2)
)
print(cov_table)
fwrite(cov_table, file.path(PATH_TAB, "tab_8d_coverage.csv"))


# ---- Section 3 : Comparaison commun vs reste sur covariables ---------------

log_step("Section 3 : comparaison covariables commun vs reste.")

vars_cmp <- c("ipd", "trade_value", "log_dist", "rta",
              "contig", "comlang_off", "colony",
              "exp_gdppc", "imp_gdppc")
vars_cmp <- vars_cmp[vars_cmp %in% names(df)]   # garde celles presentes

compare_var <- function(v) {
  x1 <- df[in_common == TRUE,  get(v)]
  x0 <- df[in_common == FALSE, get(v)]
  x1 <- x1[is.finite(x1)]; x0 <- x0[is.finite(x0)]
  if (length(x1) == 0 || length(x0) == 0)
    return(data.table(variable = v,
                      mean_commun = NA, mean_reste = NA,
                      median_commun = NA, median_reste = NA,
                      smd = NA))
  smd_val <- (mean(x1) - mean(x0)) /
             sqrt((var(x1) + var(x0)) / 2)
  data.table(
    variable      = v,
    mean_commun   = mean(x1),
    mean_reste    = mean(x0),
    median_commun = median(x1),
    median_reste  = median(x0),
    smd           = smd_val
  )
}

cmp_table <- rbindlist(lapply(vars_cmp, compare_var))
cmp_table <- cmp_table[order(-abs(smd))]
print(cmp_table)
fwrite(cmp_table, file.path(PATH_TAB, "tab_8d_covariate_balance.csv"))


# ---- Section 4 : Ancrage - PPML baseline full vs commun --------------------

log_step("Section 4 : PPML baseline (Spec 4 de 04) - full vs commun.")

# Spec EXACTE de 04 Spec 4 (workhorse) - aucun changement
est_baseline <- function(data) {
  fepois(trade_value ~ ipd + rta | exp_year + imp_year + pair,
         data = data, vcov = ~pair, notes = FALSE)
}

tic(); m_full   <- est_baseline(df);                  cat("  full   :", toc(), "s\n")
tic(); m_common <- est_baseline(df[in_common == TRUE]); cat("  commun :", toc(), "s\n")

extract_ipd <- function(m) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct[term == "ipd"][1]
}
r_full   <- extract_ipd(m_full)
r_common <- extract_ipd(m_common)

anchor <- data.table(
  echantillon = c("full",          "commun"),
  N           = c(nobs(m_full),    nobs(m_common)),
  coef_ipd    = c(r_full$estimate, r_common$estimate),
  se_ipd      = c(r_full$se,       r_common$se),
  p_ipd       = c(r_full$p,        r_common$p)
)
print(anchor)
fwrite(anchor, file.path(PATH_TAB, "tab_8d_anchor.csv"))


# ---- Section 5 : Recap console ---------------------------------------------

cat("\n==========================================================\n")
cat("  RECAP                                                    \n")
cat("==========================================================\n")
cat(sprintf("Sample full   : N = %s, IPD = %+.4f (p = %.3g)\n",
            format(nobs(m_full), big.mark = ","),
            r_full$estimate, r_full$p))
cat(sprintf("Sample commun : N = %s, IPD = %+.4f (p = %.3g)\n",
            format(nobs(m_common), big.mark = ","),
            r_common$estimate, r_common$p))

cat("\nTrois variables avec le plus grand ecart standardise commun vs reste :\n")
top_smd <- cmp_table[!is.na(smd)][1:3]
for (i in seq_len(nrow(top_smd))) {
  cat(sprintf("  %-12s : SMD = %+.3f  (mean commun = %.3g, reste = %.3g)\n",
              top_smd$variable[i], top_smd$smd[i],
              top_smd$mean_commun[i], top_smd$mean_reste[i]))
}
cat("\nSorties :\n")
cat("  -", file.path(PATH_TAB, "tab_8d_coverage.csv"), "\n")
cat("  -", file.path(PATH_TAB, "tab_8d_covariate_balance.csv"), "\n")
cat("  -", file.path(PATH_TAB, "tab_8d_anchor.csv"), "\n")

log_step("Termine.")
