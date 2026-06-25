# =============================================================================
# 07_estimation_iv_alternative.R  (PARTIE 1 : FIRST STAGES + DIAGNOSTICS)
# -----------------------------------------------------------------------------
# Control function PPML avec instruments des deux familles "institutional" et
# "strategic_relations" (cf 06_build_iv_alternative.R).
#
# Cette execution s'arrete apres les premiers stages : on remonte les
# diagnostics (KP rk Wald F, effective F Montiel-Pflueger, Hansen J, signes)
# AVANT de lancer les block-bootstraps couteux du 2nd stage.
#
# Echantillon : meme que la spec principale (script 04 Spec 4) - on garde
# USA/CHN/RUS (PAS d'exclusion comme dans 05_gravity_iv.R).
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "kableExtra")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest); library(kableExtra)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_DATA <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_IV   <- file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures",
                       "Estimation_IV_alternative")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",
                       "Estimation_IV_alternative")
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)
log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)


# ---- Section 1 : Load + merge panel ----------------------------------------

log_step("Section 1 : load master panel + iv_panel.")
df  <- as.data.table(read_parquet(PATH_DATA))
iv  <- as.data.table(read_parquet(PATH_IV))

cat("  - master panel :", nrow(df), "obs,", ncol(df), "cols\n")
cat("  - iv_panel     :", nrow(iv), "obs,", ncol(iv), "cols\n")

df <- merge(df, iv, by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
cat("  - merged       :", nrow(df), "obs,", ncol(df), "cols\n")

df[, pair      := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year  := paste(exp_iso3, year,     sep = "_")]
df[, imp_year  := paste(imp_iso3, year,     sep = "_")]
df[, log_dist  := log(dist)]

# Sample de base : meme que Spec 4 du script 04 = tout le panel avec ipd
df_base <- df[!is.na(ipd)]
cat("  - df_base (ipd non-NA) :", nrow(df_base), "obs\n")

rm(iv); gc(verbose = FALSE)


# ---- Section 2 : Helpers de diagnostics ------------------------------------

# Diagnostics IV avec fixest : on utilise la forme `y ~ ctrl | FE | endo ~ inst`
# qui produit un objet feols 2SLS-style, dont fitstat() extrait KP rk, IVF,
# IVF2 (effective F), Hansen J ("sargan"), etc.
#
# Strategie : on construit un "OLS-IV" auxiliaire pour les diagnostics
# (utilisant log(trade+1) comme LHS pour rester en feols), puis on extrait.
# La CF reelle (PPML) sera faite en sortie de cette etape 1.

run_fs_diag <- function(label, instruments, controls, data) {
  log_step(sprintf("First stage : %s", label))
  tic()

  # First stage explicite : ipd sur instruments + controls + FE
  rhs_fs <- paste(c(instruments, controls), collapse = " + ")
  fs_form <- as.formula(paste0("ipd ~ ", rhs_fs,
                               " | exp_year + imp_year + pair"))
  fs <- feols(fs_form, data = data, vcov = ~pair, notes = FALSE)

  # Wald F sur les instruments (joint exclusion)
  joint_wald <- tryCatch(
    wald(fs, keep = paste0("^", instruments, "$", collapse = "|"))$stat,
    error = function(e) NA_real_)

  # Diagnostics IV via la forme `feols(... | endo ~ inst)`. Note : le LHS
  # peut etre n'importe quoi - on n'utilise que les diagnostics du 1st stage.
  # On utilise log(trade_value+1) car PPML n'est pas dispo en IV-fixest.
  data[, log_trade1 := log(trade_value + 1)]
  ctrl_str <- if (length(controls)) paste(controls, collapse = " + ") else "1"
  iv_form_str <- sprintf(
    "log_trade1 ~ %s | exp_year + imp_year + pair | ipd ~ %s",
    ctrl_str, paste(instruments, collapse = " + "))
  iv_form <- as.formula(iv_form_str)
  iv_aux <- tryCatch(
    feols(iv_form, data = data, vcov = ~pair, notes = FALSE),
    error = function(e) NULL)

  diag <- list(label = label, N = nobs(fs),
               n_instruments = length(instruments),
               n_controls    = length(controls))

  if (!is.null(iv_aux)) {
    fs_iv <- tryCatch(fitstat(iv_aux, "kpr"), error = function(e) NULL)
    if (!is.null(fs_iv)) {
      diag$kp_rk_F <- fs_iv$kpr$stat
    } else {
      diag$kp_rk_F <- NA_real_
    }
    eff <- tryCatch(fitstat(iv_aux, "ivf2"), error = function(e) NULL)
    if (!is.null(eff) && length(eff)) {
      # ivf2 = Montiel Olea-Pflueger effective F (cluster-robust)
      diag$effective_F <- eff[[1]]$stat
    } else {
      diag$effective_F <- NA_real_
    }
    if (length(instruments) >= 2) {
      hj <- tryCatch(fitstat(iv_aux, "sargan"), error = function(e) NULL)
      diag$hansen_J  <- if (!is.null(hj)) hj$sargan$stat   else NA_real_
      diag$hansen_p  <- if (!is.null(hj)) hj$sargan$p      else NA_real_
    } else {
      diag$hansen_J <- NA_real_; diag$hansen_p <- NA_real_
    }
  } else {
    diag$kp_rk_F   <- NA_real_
    diag$effective_F <- NA_real_
    diag$hansen_J <- NA_real_; diag$hansen_p <- NA_real_
  }

  diag$wald_joint <- joint_wald

  # Coefs des instruments dans le 1st stage
  ct <- as.data.table(coeftable(fs), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  diag$coefs <- ct[term %in% instruments]

  cat(sprintf("  N=%d | KP rk F=%.1f | Eff F=%.1f | Wald joint F=%.1f",
              diag$N,
              if (is.na(diag$kp_rk_F)) NA else diag$kp_rk_F,
              if (is.na(diag$effective_F)) NA else diag$effective_F,
              joint_wald))
  if (!is.na(diag$hansen_J))
    cat(sprintf(" | Hansen J=%.2f (p=%.3f)", diag$hansen_J, diag$hansen_p))
  cat(sprintf(" | %.1fs\n", toc()))

  cat("  Coefs des instruments :\n")
  print(diag$coefs[, .(term, estimate = round(estimate, 4),
                        se = round(se, 4),
                        z = round(stat, 2),
                        p = round(p, 4))])
  diag
}


# ---- Section 3 : First stages par famille ---------------------------------

log_step("Section 3 : first stages par famille.")

# 3a. Institutional - V-Dem + DPI (3 instruments)
inst_institutional <- c("polyarchy_dist", "joint_dem_vdem", "ideol_dist")
data_inst <- df_base[!is.na(polyarchy_dist) & !is.na(ideol_dist) &
                       !is.na(joint_dem_vdem)]
cat("\n[3a] Sample institutional :", nrow(data_inst), "obs\n")
d_inst <- run_fs_diag("institutional (V-Dem + DPI)",
                      inst_institutional,
                      controls = c("rta"),
                      data = data_inst)

# 3b. Strategic relations - ATOP + MID (3 instruments + 1 control)
inst_strategic <- c("allied_atop", "shared_ally_atop", "shared_rival_mid")
data_strat <- df_base[!is.na(allied_atop) & !is.na(shared_ally_atop) &
                        !is.na(shared_rival_mid) & !is.na(mid_direct)]
cat("\n[3b] Sample strategic_relations :", nrow(data_strat), "obs\n")
d_strat <- run_fs_diag("strategic_relations (ATOP + MID)",
                       inst_strategic,
                       controls = c("rta", "mid_direct"),
                       data = data_strat)

# 3c. Combined - les deux familles ensemble (6 instruments)
inst_combined <- c(inst_institutional, inst_strategic)
data_comb <- df_base[!is.na(polyarchy_dist) & !is.na(ideol_dist) &
                       !is.na(joint_dem_vdem) & !is.na(allied_atop) &
                       !is.na(shared_ally_atop) & !is.na(shared_rival_mid) &
                       !is.na(mid_direct)]
cat("\n[3c] Sample combined :", nrow(data_comb), "obs\n")
d_comb <- run_fs_diag("combined (institutional + strategic)",
                      inst_combined,
                      controls = c("rta", "mid_direct"),
                      data = data_comb)

# 3d. Polity robustness - polyarchy remplace par polity_dist (separe de V-Dem)
inst_polity <- c("polity_dist", "ideol_dist")
data_pol <- df_base[!is.na(polity_dist) & !is.na(ideol_dist)]
cat("\n[3d] Sample Polity robustness :", nrow(data_pol), "obs\n")
d_pol <- run_fs_diag("institutional Polity (Polity5 + DPI)",
                     inst_polity,
                     controls = c("rta"),
                     data = data_pol)


# ---- Section 4 : Table de synthese des diagnostics -------------------------

log_step("Section 4 : table de synthese.")

diag_dt <- rbindlist(list(d_inst, d_strat, d_comb, d_pol), fill = TRUE,
                    use.names = TRUE, idcol = NULL)
diag_dt <- data.table(
  family       = c("institutional", "strategic_relations",
                   "combined", "Polity robustness"),
  label        = sapply(list(d_inst, d_strat, d_comb, d_pol), `[[`, "label"),
  N            = sapply(list(d_inst, d_strat, d_comb, d_pol), `[[`, "N"),
  n_inst       = sapply(list(d_inst, d_strat, d_comb, d_pol),
                        `[[`, "n_instruments"),
  KP_rk_F      = sapply(list(d_inst, d_strat, d_comb, d_pol), `[[`, "kp_rk_F"),
  Effective_F  = sapply(list(d_inst, d_strat, d_comb, d_pol),
                        `[[`, "effective_F"),
  Wald_joint_F = sapply(list(d_inst, d_strat, d_comb, d_pol),
                        `[[`, "wald_joint"),
  Hansen_J     = sapply(list(d_inst, d_strat, d_comb, d_pol), `[[`, "hansen_J"),
  Hansen_p     = sapply(list(d_inst, d_strat, d_comb, d_pol), `[[`, "hansen_p")
)
print(diag_dt)

fwrite(diag_dt, file.path(PATH_TAB, "tab_iv_alt_first_stage_diagnostics.csv"))
writeLines(as.character(
  kbl(diag_dt, format = "latex", booktabs = TRUE, digits = 2,
      caption = "First-stage diagnostics: alternative IV families",
      label = "tab:iv_alt_fs") |>
  kable_styling(latex_options = c("hold_position", "scale_down"))),
  file.path(PATH_TAB, "tab_iv_alt_first_stage_diagnostics.tex"))


# ---- Section 5 : Tables detaillees des coefs ------------------------------

log_step("Section 5 : coefs detailles par famille.")

dump_coefs <- function(diag, family) {
  out <- copy(diag$coefs)
  out[, family := family]
  out[, label  := diag$label]
  out
}
coefs_all <- rbindlist(list(
  dump_coefs(d_inst,  "institutional"),
  dump_coefs(d_strat, "strategic_relations"),
  dump_coefs(d_comb,  "combined"),
  dump_coefs(d_pol,   "Polity robustness")
))
print(coefs_all[, .(family, term, estimate = round(estimate, 4),
                    se = round(se, 4), p = round(p, 4))])
fwrite(coefs_all, file.path(PATH_TAB, "tab_iv_alt_first_stage_coefs.csv"))


# ---- Section 6 : STOP - en attente de confirmation utilisateur -------------

cat("\n")
cat("==========================================================\n")
cat("  ARRET PROVISOIRE : first stages terminees                \n")
cat("  Diagnostics dans Output/Tables/Estimation_IV_alternative/\n")
cat("  Prochaine etape (sur feu vert) : control function PPML  \n")
cat("  + block-bootstrap des SE.                               \n")
cat("==========================================================\n")

log_step("Termine (partie 1).")
