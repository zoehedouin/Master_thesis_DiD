# =============================================================================
# 07c_estimation_iv_clean.R   (PARTIE A : ESTIMATIONS PONCTUELLES UNIQUEMENT)
# -----------------------------------------------------------------------------
# Control function PPML (2SRI : residual inclusion, PAS substitution de
# valeurs predites). Conforme a la check-list de l'utilisateur.
#
# Methode :
#   1er stage OLS : ipd ~ instruments + controls | exp_year + imp_year + pair
#                    vcov = ~pair  ->  v_hat = residuals
#   2nd stage PPML : trade_value ~ ipd + v_hat + controls | exp_year +
#                    imp_year + pair        vcov = ~pair
#
# Specs :
#   (S7)   just-id        : polyarchy_dist
#   (S8a)  institutional  : polyarchy_dist + ideol_dist
#   (S8b)  strategic      : allied_atop + shared_rival_mid
#                           (sans shared_ally_atop ; mid_direct en CONTROLE)
#   (S8c)  combined       : poly + ideol + allied + shared_rival (mid_direct
#                           toujours controle)
#   (S8bis) Polity annex  : polity_dist + ideol_dist
#   (S8ter-1) PPML baseline (no IV, meme controles)
#   (S8ter-2) IV existant  : alignement aux poles USA/CHN/RUS, lag 2
#                            (reconstruit inline depuis iv_panel)
#
# Controles time-varying = ceux de la spec principale du script 04 (rta).
#                           strategic / combined ajoutent mid_direct.
#
# Diagnostics par spec : F joint 1st stage, F par instrument, signes, coef
# IPD CF + SE + p, coef v_hat + p (test endogeneite), Sargan + Hansen J
# cluster-robust (over-id seulement).
#
# !!! AUCUN BOOTSTRAP DANS CE FICHIER. La partie B (bootstrap) sera ajoutee
#     APRES validation manuelle de la table de cette partie A.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "kableExtra")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest); library(kableExtra)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_IV   <- file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",
                       "Estimation_IV_alternative")
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)
log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)


# ---- Section 1 : Load + reconstitution IV existant -------------------------

log_step("Load iv_panel.")
df <- as.data.table(read_parquet(PATH_IV))
df[, pair     := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year := paste(exp_iso3, year,     sep = "_")]
df[, imp_year := paste(imp_iso3, year,     sep = "_")]

cat("  - Panel :", nrow(df), "obs,", ncol(df), "cols\n")

# Reconstruction inline de l'instrument "IV existant" (script 05, lag 2)
# Distance euclidienne d'alignement aux poles USA/CHN/RUS a t-2.
log_step("Construction inline : IV existant (alignement lag 2).")
poles <- c("USA", "CHN", "RUS")
align <- unique(df[imp_iso3 %in% poles & !is.na(ipd),
                   .(iso3 = exp_iso3, pole = imp_iso3, year,
                     ipd_pole = ipd)])
al_wide <- dcast(align, iso3 + year ~ pole, value.var = "ipd_pole")

al_l2 <- copy(al_wide)
al_l2[, year_target := year + 2L]
al_l2[, year := NULL]
setnames(al_l2, poles, c("USA_l2", "CHN_l2", "RUS_l2"))

df <- merge(df, al_l2, by.x = c("exp_iso3", "year"),
            by.y = c("iso3", "year_target"), all.x = TRUE)
setnames(df, c("USA_l2", "CHN_l2", "RUS_l2"),
              c("exp_USA_l2", "exp_CHN_l2", "exp_RUS_l2"))
df <- merge(df, al_l2, by.x = c("imp_iso3", "year"),
            by.y = c("iso3", "year_target"), all.x = TRUE)
setnames(df, c("USA_l2", "CHN_l2", "RUS_l2"),
              c("imp_USA_l2", "imp_CHN_l2", "imp_RUS_l2"))
df[, instrument_l2 := sqrt(
  (exp_USA_l2 - imp_USA_l2)^2 +
  (exp_CHN_l2 - imp_CHN_l2)^2 +
  (exp_RUS_l2 - imp_RUS_l2)^2)]
cat("  - Obs avec instrument_l2 non-NA :", sum(!is.na(df$instrument_l2)), "\n")

# Echantillon : meme que spec principale -> ipd non-NA. Aucun filtre poles.
df_base <- df[!is.na(ipd)]
cat("  - Sample (ipd non-NA) :", nrow(df_base), "obs\n")
rm(df, align, al_wide, al_l2); gc(verbose = FALSE)


# ---- Section 2 : Specifications --------------------------------------------
#
# Format : (label, instruments, extra_controls_time_varying, sample_filter,
#          notes)
# Tous les specs partagent CTRL_BASE = "rta" et FE three-way.

CTRL_BASE <- c("rta")
FE_3WAY   <- "exp_year + imp_year + pair"

# Roles (utilisateur) :
#   tete d'affiche : polyarchy
#   triangulation  : ideol, shared_rival
#   cautionary     : allied, IV existant lag2
#   robustesse de mesure : polity
ROLES <- c(
  "S7"               = "tete affiche",
  "S7_ideol"         = "triangulation",
  "S7_allied"        = "cautionary",
  "S7_rival"         = "triangulation",
  "S7_polity"        = "robust. mesure",
  "S8a"              = "famille institutional",
  "S8b"              = "famille strategic",
  "S8c"              = "famille combined",
  "S8bis"            = "annex Polity",
  "S8ter_existant"   = "cautionary (lag2)",
  "S0_baseline_S7"   = "baseline",
  "S0_baseline_full" = "baseline",
  "S0_baseline_common" = "baseline (sample commun)"
)

# Pour les juste-identifies, on garde uniquement rta en controle (pour
# que les 5 specs single-IV soient parfaitement comparables, sans glissement
# de controles entre instruments institutional vs strategic).
specs <- list(
  # ---- Juste-identifies, un instrument par spec ----
  S7         = list(label = "S7 just-id polyarchy",
                    insts = "polyarchy_dist", ctrls = CTRL_BASE),
  S7_ideol   = list(label = "S7b just-id ideol",
                    insts = "ideol_dist",     ctrls = CTRL_BASE),
  S7_allied  = list(label = "S7c just-id allied",
                    insts = "allied_atop",    ctrls = CTRL_BASE),
  S7_rival   = list(label = "S7d just-id shared_rival",
                    insts = "shared_rival_mid", ctrls = CTRL_BASE),
  S7_polity  = list(label = "S7e just-id polity",
                    insts = "polity_dist",    ctrls = CTRL_BASE),
  S8ter_existant = list(label = "S8ter IV existant lag2",
                    insts = "instrument_l2",  ctrls = CTRL_BASE),
  # ---- Sur-identifies (familles) ----
  S8a = list(label = "S8a family institutional",
             insts = c("polyarchy_dist", "ideol_dist"), ctrls = CTRL_BASE),
  S8b = list(label = "S8b family strategic",
             insts = c("allied_atop", "shared_rival_mid"),
             ctrls = c(CTRL_BASE, "mid_direct")),
  S8c = list(label = "S8c family combined",
             insts = c("polyarchy_dist", "ideol_dist",
                       "allied_atop", "shared_rival_mid"),
             ctrls = c(CTRL_BASE, "mid_direct")),
  S8bis = list(label = "S8bis Polity annex",
               insts = c("polity_dist", "ideol_dist"), ctrls = CTRL_BASE)
)


# ---- Section 3 : Helpers ----------------------------------------------------

# Echantillon valide pour une spec : ipd + instruments + controls non-NA
sample_for_spec <- function(s, data = df_base) {
  d <- data
  for (v in c(s$insts, s$ctrls)) d <- d[!is.na(get(v))]
  d
}

# Estimation CF-PPML pour une spec
run_cf_ppml <- function(s, data = df_base) {
  d <- sample_for_spec(s, data)

  # ---- 1er stage : OLS ipd ~ instruments + controls | FE three-way ----
  rhs_fs <- paste(c(s$insts, s$ctrls), collapse = " + ")
  fs_form <- as.formula(sprintf("ipd ~ %s | %s", rhs_fs, FE_3WAY))
  fs <- feols(fs_form, data = d, vcov = ~pair, notes = FALSE)

  # Securisation : alignement v_hat <-> obs effectivement utilisees
  if (nobs(fs) != nrow(d)) {
    d <- d[!is.na(predict(fs, newdata = d))]
    fs <- feols(fs_form, data = d, vcov = ~pair, notes = FALSE)
  }
  d[, v_hat := residuals(fs)]

  # ---- F joint des instruments (cluster-robust ~pair) ----
  w_joint <- wald(fs, keep = paste0("^", s$insts, "$", collapse = "|"))
  F_joint <- w_joint$stat

  # ---- F par instrument seul (juste-id) : t^2 cluster-robust ----
  ct_fs <- as.data.table(coeftable(fs), keep.rownames = "term")
  setnames(ct_fs, 2:5, c("est", "se", "stat", "p"))
  per_inst <- ct_fs[term %in% s$insts,
                    .(term, est, se, F_eff = stat^2, p)]

  # ---- 2nd stage : PPML trade_value ~ ipd + v_hat + controls | FE ----
  ss_rhs <- paste(c("ipd", "v_hat", s$ctrls), collapse = " + ")
  ss_form <- as.formula(sprintf("trade_value ~ %s | %s", ss_rhs, FE_3WAY))
  ss <- fepois(ss_form, data = d, vcov = ~pair, notes = FALSE)

  ct_ss <- as.data.table(coeftable(ss), keep.rownames = "term")
  setnames(ct_ss, 2:5, c("est", "se", "stat", "p"))
  ipd_row  <- ct_ss[term == "ipd"][1]
  vhat_row <- ct_ss[term == "v_hat"][1]

  # ---- Sargan + Hansen J cluster-robust (over-id seulement, k_inst >= 2) ---
  sargan_J <- NA_real_; sargan_p <- NA_real_
  hansen_J <- NA_real_; hansen_p <- NA_real_
  if (length(s$insts) >= 2) {
    iv_form <- as.formula(sprintf(
      "trade_value ~ %s | %s | ipd ~ %s",
      paste(s$ctrls, collapse = " + "), FE_3WAY,
      paste(s$insts, collapse = " + ")))
    iv_aux <- tryCatch(feols(iv_form, data = d, vcov = ~pair, notes = FALSE),
                       error = function(e) NULL)
    if (!is.null(iv_aux)) {
      sgn <- tryCatch(fitstat(iv_aux, "sargan"), error = function(e) NULL)
      if (!is.null(sgn) && !is.null(sgn$sargan)) {
        sargan_J <- sgn$sargan$stat
        sargan_p <- sgn$sargan$p
      }
      # Hansen J cluster-robust manuel :
      #  1) residus 2SLS u
      #  2) regress u ~ instruments + controls | FE (vcov ~pair)
      #  3) Wald joint sur instruments -> stat * df1 ~= chi2(k_inst - k_endo)
      u <- residuals(iv_aux)
      d_aux <- copy(d)
      d_aux[, u_2sls := u]
      aux_form <- as.formula(sprintf(
        "u_2sls ~ %s | %s",
        paste(c(s$insts, s$ctrls), collapse = " + "), FE_3WAY))
      aux <- tryCatch(feols(aux_form, data = d_aux, vcov = ~pair,
                            notes = FALSE),
                      error = function(e) NULL)
      if (!is.null(aux)) {
        w_aux <- tryCatch(wald(aux,
                               keep = paste0("^", s$insts, "$",
                                              collapse = "|")),
                          error = function(e) NULL)
        if (!is.null(w_aux)) {
          # k_overid = k_inst - 1 (un seul endogene)
          k_overid <- length(s$insts) - 1L
          hansen_J <- w_aux$stat * w_aux$df1
          hansen_p <- pchisq(hansen_J, df = k_overid, lower.tail = FALSE)
        }
      }
    }
  }

  list(label = s$label, N = nobs(ss), n_inst = length(s$insts),
       coef_ipd    = ipd_row$est,  se_ipd  = ipd_row$se,  p_ipd  = ipd_row$p,
       coef_vhat   = vhat_row$est, se_vhat = vhat_row$se, p_vhat = vhat_row$p,
       F_joint     = F_joint,
       per_inst    = per_inst,
       sargan_J    = sargan_J, sargan_p = sargan_p,
       hansen_J    = hansen_J, hansen_p = hansen_p,
       fs          = fs,  ss = ss)
}

# PPML baseline (no IV) sur l'echantillon le plus restrictif ou le plus large ?
# -> on lance deux versions : "full" (1.03M obs) ET "sur sample S7" (pour comp.).
run_ppml_baseline <- function(data, label) {
  ss_form <- as.formula(sprintf(
    "trade_value ~ ipd + %s | %s",
    paste(CTRL_BASE, collapse = " + "), FE_3WAY))
  m <- fepois(ss_form, data = data, vcov = ~pair, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, 2:5, c("est", "se", "stat", "p"))
  r <- ct[term == "ipd"][1]
  list(label = label, N = nobs(m), n_inst = 0L,
       coef_ipd = r$est, se_ipd = r$se, p_ipd = r$p,
       coef_vhat = NA_real_, se_vhat = NA_real_, p_vhat = NA_real_,
       F_joint  = NA_real_, per_inst = NULL,
       sargan_J = NA_real_, sargan_p = NA_real_,
       hansen_J = NA_real_, hansen_p = NA_real_)
}


# ---- Section 4 : Estimations ponctuelles (Partie A) ------------------------

log_step("Partie A : estimations ponctuelles CF-PPML.")

results <- list()

# Echantillon commun : intersection des couvertures des 5 instruments
# juste-identifies (poly, ideol, allied, shared_rival, polity) + lag2.
log_step("Construction echantillon commun (intersection).")
all_insts_singles <- c("polyarchy_dist", "ideol_dist", "allied_atop",
                       "shared_rival_mid", "polity_dist", "instrument_l2")
df_common <- copy(df_base)
for (v in all_insts_singles) df_common <- df_common[!is.na(get(v))]
cat("  - Sample commun :", nrow(df_common), "obs\n")

# 4a. PPML baselines (no IV)
log_step("  PPML baseline (sample S7).")
tic()
d_s7 <- sample_for_spec(specs$S7)
results[["S0_baseline_S7"]] <- run_ppml_baseline(d_s7,
  sprintf("S0 PPML baseline (sample S7, N=%s)", format(nrow(d_s7), big.mark=",")))
cat("    time:", toc(), "s\n")

log_step("  PPML baseline (sample full).")
tic()
results[["S0_baseline_full"]] <- run_ppml_baseline(df_base,
  sprintf("S0 PPML baseline (full, N=%s)", format(nrow(df_base), big.mark=",")))
cat("    time:", toc(), "s\n")

log_step("  PPML baseline (sample commun).")
tic()
results[["S0_baseline_common"]] <- run_ppml_baseline(df_common,
  sprintf("S0 PPML baseline (commun, N=%s)", format(nrow(df_common), big.mark=",")))
cat("    time:", toc(), "s\n")

# 4b. CF-PPML pour chaque spec sur son echantillon natif
for (sname in names(specs)) {
  log_step(sprintf("  [own sample] %s", specs[[sname]]$label))
  tic()
  results[[sname]] <- run_cf_ppml(specs[[sname]])
  cat(sprintf("    N=%d  IPD=%.4f (p=%.3g)  v_hat p=%.3g  F_joint=%.1f  %.1fs\n",
              results[[sname]]$N,
              results[[sname]]$coef_ipd, results[[sname]]$p_ipd,
              results[[sname]]$p_vhat,
              results[[sname]]$F_joint, toc()))
}

# 4c. CF-PPML juste-identifies sur ECHANTILLON COMMUN
log_step("  CF-PPML juste-identifies sur sample commun (intersection).")
single_specs <- c("S7", "S7_ideol", "S7_allied", "S7_rival",
                  "S7_polity", "S8ter_existant")
# Pre-enregistrer les roles "commun" pour eviter l'assignment indexe via <<-
for (sname in single_specs) {
  ROLES[paste0(sname, "_common")] <- paste0(ROLES[sname], " (commun)")
}
for (sname in single_specs) {
  s <- specs[[sname]]
  key <- paste0(sname, "_common")
  log_step(sprintf("  [common sample] %s", s$label))
  tic()
  r <- run_cf_ppml(s, data = df_common)
  r$label <- paste("[common]", s$label)
  results[[key]] <- r
  cat(sprintf("    N=%d  IPD=%.4f (p=%.3g)  v_hat p=%.3g  F_joint=%.1f  %.1fs\n",
              r$N, r$coef_ipd, r$p_ipd, r$p_vhat, r$F_joint, toc()))
}


# ---- Section 5 : Tableau side-by-side ---------------------------------------

log_step("Tableau de synthese.")

summary_dt <- rbindlist(lapply(names(results), function(k) {
  r <- results[[k]]
  data.table(
    spec        = k,
    role        = if (k %in% names(ROLES)) ROLES[[k]] else "",
    label       = r$label,
    N           = r$N,
    n_inst      = r$n_inst,
    coef_ipd    = r$coef_ipd,
    se_ipd      = r$se_ipd,
    p_ipd       = r$p_ipd,
    coef_vhat   = r$coef_vhat,
    p_vhat_endo = r$p_vhat,
    F_joint     = r$F_joint,
    sargan_J    = r$sargan_J,
    sargan_p    = r$sargan_p,
    hansen_J    = r$hansen_J,
    hansen_p    = r$hansen_p
  )
}))
print(summary_dt[, .(spec, role, N,
                      coef = round(coef_ipd, 4),
                      se   = round(se_ipd,   3),
                      p_ipd = round(p_ipd, 4),
                      p_vhat = round(p_vhat_endo, 4),
                      F = round(F_joint, 1),
                      Sargan_p  = round(sargan_p, 3),
                      HansenJ_p = round(hansen_p, 3))])

fwrite(summary_dt, file.path(PATH_TAB, "tab_07c_point_estimates.csv"))

# Detail per-instrument (signes 1er stage)
per_inst_dt <- rbindlist(lapply(names(results), function(k) {
  r <- results[[k]]
  if (is.null(r$per_inst) || nrow(r$per_inst) == 0) return(NULL)
  out <- copy(r$per_inst); out[, spec := k]; out[]
}), fill = TRUE)
cat("\nSignes du 1er stage par instrument :\n")
print(per_inst_dt[, .(spec, term,
                       coef = round(est, 4),
                       F_eff = round(F_eff, 1),
                       p = round(p, 4))])
fwrite(per_inst_dt, file.path(PATH_TAB, "tab_07c_first_stage_signs.csv"))


# ---- Section 6 : STOP -------------------------------------------------------

cat("\n")
cat("==========================================================\n")
cat("  PARTIE A TERMINEE : estimations ponctuelles uniquement   \n")
cat("  AUCUN BOOTSTRAP LANCE.                                   \n")
cat("  Verifier signes / significativites / Hansen J / vhat     \n")
cat("  AVANT de lancer la partie B (block-bootstrap pair-level) \n")
cat("  sur la spec principale (S7 + S8a).                       \n")
cat("==========================================================\n")

log_step("Termine (Partie A).")
