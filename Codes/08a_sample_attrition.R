# 08a_sample_attrition.R
# Diagnostic simple : pourquoi chaque mesure alternative couvre-t-elle si peu
# d'observations par rapport au panel principal ?
#
# Idee : la perte d'obs vient de deux sources distinctes -
#   (1) troncature TEMPORELLE (ex. MID s'arrete 2014, Polity 2018)
#   (2) couverture PAYS, qui pese de facon QUADRATIQUE sur les dyades
#       (les DEUX pays d'une paire doivent etre couverts : si c% des pays
#        sont couverts, ~c²% des dyades le sont)
# On separe aussi, dans la fenetre temporelle, la part de NA "internes"
# (pays/donnees manquants) du reste.

suppressPackageStartupMessages({
  library(arrow); library(data.table)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
df <- as.data.table(read_parquet(
  file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")))
df <- df[!is.na(trade_value)]   # base = obs estimables du baseline

N_full    <- nrow(df)
all_years <- sort(unique(df$year))
all_ctry  <- unique(c(df$exp_iso3, df$imp_iso3))

measures <- c("polyarchy_dist", "ideol_dist", "polity_dist",
              "allied_atop", "shared_rival_mid",
              "sanction_nontrade", "n_common_sanctioners")

diag_one <- function(m) {
  obs <- !is.na(df[[m]])
  n_m <- sum(obs)
  yrs <- range(df$year[obs])
  in_window <- df$year >= yrs[1] & df$year <= yrs[2]

  lost_total <- N_full - n_m
  lost_time  <- sum(!in_window)               # perdu hors fenetre temporelle
  lost_inwin <- sum(in_window & !obs)         # perdu DANS la fenetre

  ctry_cov <- length(unique(c(df$exp_iso3[obs], df$imp_iso3[obs]))) /
              length(all_ctry)

  data.table(
    mesure          = m,
    pct_du_panel    = round(100 * n_m / N_full, 1),
    n_obs           = n_m,
    annee_min       = yrs[1],
    annee_max       = yrs[2],
    n_annees        = sum(all_years >= yrs[1] & all_years <= yrs[2]),
    couv_pays_pct   = round(100 * ctry_cov, 1),
    couv_dyad_theo  = round(100 * ctry_cov^2, 1),
    perte_temps_pct = round(100 * lost_time  / max(lost_total, 1), 1),
    perte_inwin_pct = round(100 * lost_inwin / max(lost_total, 1), 1)
  )
}

tab <- rbindlist(lapply(measures, diag_one))
print(tab)

dir.create(file.path(PATH_ROOT, "Output", "Tables", "Robustness"),
           recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(PATH_ROOT, "Output", "Tables", "Robustness", "_archive"),
           recursive = TRUE, showWarnings = FALSE)
fwrite(tab, file.path(PATH_ROOT, "Output", "Tables", "Robustness", "_archive",
                      "tab_sample_attrition.csv"))

cat("\nLecture :\n")
cat("- perte_temps_pct vs perte_inwin_pct -> origine de la perte (temps ou pays/NA)\n")
cat("- couv_dyad_theo ~ pct_du_panel      -> si proches, la perte est l'effet\n")
cat("                                        quadratique de la couverture pays.\n")
