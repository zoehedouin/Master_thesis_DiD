# 08b_identifiability_checks.R
# Trois controles courts AVANT la robustesse de mesure :
#  (A) part de VALEUR commerciale retenue par mesure (vs part de dyades)
#  (B) ratio de variance WITHIN apres FE three-way (analogue du 0.083 IPD)
#  (C) la manquance de ideol_dist depend-elle du niveau de democratie ?

suppressPackageStartupMessages({
  library(arrow); library(data.table); library(fixest)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
df <- as.data.table(read_parquet(
  file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")))
df <- df[!is.na(trade_value)]

df[, pair := paste(exp_iso3, imp_iso3, sep = "_")]
df[, ey   := paste(exp_iso3, year,     sep = "_")]
df[, iy   := paste(imp_iso3, year,     sep = "_")]

measures <- c("polyarchy_dist", "ideol_dist", "polity_dist",
              "allied_atop", "shared_rival_mid",
              "sanction_nontrade", "n_common_sanctioners")

N_full <- nrow(df)
V_full <- sum(df$trade_value)

# ---- (A) + (B) : valeur retenue et ratio within, par mesure ---------------
within_ratio <- function(x, fe_dt) {
  ok <- is.finite(x)
  xd <- demean(x[ok], fe_dt[ok])           # residu apres projection FE 3-way
  as.numeric(var(xd) / var(x[ok]))
}

fe_dt <- df[, .(ey, iy, pair)]

checks <- function(m) {
  ok <- !is.na(df[[m]])
  data.table(
    mesure         = m,
    pct_dyades     = round(100 * sum(ok) / N_full, 1),
    pct_valeur     = round(100 * sum(df$trade_value[ok]) / V_full, 1),
    ecart_val_dyad = round(100 * sum(df$trade_value[ok]) / V_full
                             - 100 * sum(ok) / N_full, 1),
    within_ratio   = round(within_ratio(df[[m]], fe_dt), 4)
  )
}

tab_AB <- rbindlist(lapply(measures, checks))
print(tab_AB)

# ---- (C) : selection de ideol_dist sur le niveau de democratie ------------
sub <- df[!is.na(polyarchy_dist)]
sub[, dem_bin := cut(joint_dem_vdem, breaks = seq(0, 1, 0.2),
                     include.lowest = TRUE)]
tab_C <- sub[, .(n = .N,
                 pct_ideol_NA = round(100 * mean(is.na(ideol_dist)), 1)),
             by = dem_bin][order(dem_bin)]
print(tab_C)

# ---- Sorties --------------------------------------------------------------
dir.create(file.path(PATH_ROOT, "Output", "Tables", "Robustness", "_archive"),
           recursive = TRUE, showWarnings = FALSE)
fwrite(tab_AB, file.path(PATH_ROOT, "Output", "Tables", "Robustness", "_archive",
                          "tab_identifiability.csv"))
fwrite(tab_C, file.path(PATH_ROOT, "Output", "Tables", "Robustness", "_archive",
                         "tab_ideol_selection.csv"))

cat("\nLecture :\n")
cat("(A) ecart_val_dyad tres negatif -> on perd plus de valeur que de dyades\n")
cat("    (gros entrepots droppes) : comparaison baseline a relativiser.\n")
cat("(B) within_ratio proche de 0   -> mesure absorbee par les pair FE :\n")
cat("    a presenter comme 'non identifiee', pas 'effet nul'. Reference IPD = 0.083.\n")
cat("(C) pct_ideol_NA decroissant de la tranche basse vers la haute -> selection\n")
cat("    confirmee : ideol_dist manque surtout pour les paires autocratiques.\n")
