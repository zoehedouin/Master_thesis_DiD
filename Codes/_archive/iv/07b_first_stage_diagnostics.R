# ===========================================================================
# 07b_first_stage_diagnostics.R
# Diagnostics de PREMIER STAGE, a lancer AVANT tout bootstrap.
# Lecture seule : ne modifie aucun script/output existant, ecrit seulement
# dans Output/Tables/Estimation_IV_alternative/diagnostics/.
#
# Objectifs :
#   1. Tester CHAQUE instrument SEUL (juste-identifie). Pour 1 instrument,
#      le F cluster-robust EST l'effective F -> diagnostic propre, sans MOP,
#      et ca contourne la divergence KP / effective F.
#   2. Tester les familles en JOINT + reconcilier les F sur le MEME vcov ~pair.
#   3. Comparer la pertinence selon la structure de FE
#      (paire complete / sans paire / paire-region).
#   4. Inspecter shared_ally_atop (signe + contre-intuitif).
#   5. Intervalle d'Anderson-Rubin (inference robuste aux instruments faibles).
#
# Adapte au repo : panel auto-contenu Data/Clean/iv_panel.parquet ecrit par
# 06_build_iv_alternative.R (contient instruments + ipd + trade_value + rta
# + log_dist + contig + comlang_off + colony).
# region_pair construit via countrycode (continents) car master_panel n'a
# pas de region_o/region_d natifs.
# ===========================================================================

suppressMessages({
  library(fixest)
  library(arrow)
  library(data.table)
  library(countrycode)
})
set.seed(123)

# ---- 0. CONFIG ------------------------------------------------------------
PATH_ROOT  <- "/Users/zoe/Desktop/Master_thesis"
panel_path <- file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")
out_dir    <- file.path(PATH_ROOT, "Output", "Tables",
                        "Estimation_IV_alternative", "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

Y       <- "trade_value"
ENDOG   <- "ipd"
CL      <- ~pair
CTRL_TV <- c("rta")
CTRL_TI <- c("log_dist", "contig", "comlang_off", "colony")

INST <- list(
  institutional = c("polyarchy_dist", "ideol_dist"),
  strategic     = c("allied_atop", "shared_ally_atop", "shared_rival_mid")
)

FE <- list(
  pair_full   = "exp_year + imp_year + pair",
  no_pair     = "exp_year + imp_year",
  pair_region = "exp_year + imp_year + region_pair"
)

df <- as.data.table(read_parquet(panel_path))

# IDs FE (au cas ou pas deja dans le panel)
if (!"pair"     %in% names(df)) df[, pair     := paste(exp_iso3, imp_iso3, sep = "_")]
if (!"exp_year" %in% names(df)) df[, exp_year := paste(exp_iso3, year,     sep = "_")]
if (!"imp_year" %in% names(df)) df[, imp_year := paste(imp_iso3, year,     sep = "_")]

# region_pair : continents via countrycode si pas dispo natif
if (!"region_pair" %in% names(df)) {
  iso_uniq <- unique(c(df$exp_iso3, df$imp_iso3))
  cmap <- data.table(iso3 = iso_uniq,
                     cont = countrycode(iso_uniq, origin = "iso3c",
                                        destination = "continent",
                                        warn = FALSE))
  df <- merge(df, cmap[, .(exp_iso3 = iso3, cont_e = cont)],
              by = "exp_iso3", all.x = TRUE)
  df <- merge(df, cmap[, .(imp_iso3 = iso3, cont_i = cont)],
              by = "imp_iso3", all.x = TRUE)
  df[, region_pair := paste(pmin(cont_e, cont_i),
                            pmax(cont_e, cont_i), sep = "_")]
  df[is.na(cont_e) | is.na(cont_i), region_pair := NA_character_]
}

# Sample : ipd non-NA
df <- df[!is.na(get(ENDOG))]
cat("Sample :", nrow(df), "obs (ipd non-NA)\n")


# ===========================================================================
# 1. PREMIER STAGE PAR INSTRUMENT SEUL (juste-identifie)
# ===========================================================================
one_inst_fs <- function(inst, fe_rhs, ti = FALSE) {
  rhs <- c(inst, CTRL_TV, if (ti) CTRL_TI)
  f <- as.formula(sprintf("%s ~ %s | %s",
                          ENDOG, paste(rhs, collapse = " + "), fe_rhs))
  d  <- df[!is.na(get(inst))]
  if (ti) for (var in CTRL_TI) d <- d[!is.na(get(var))]
  m <- tryCatch(feols(f, data = d, vcov = CL, notes = FALSE),
                error = function(e) NULL)
  if (is.null(m))
    return(data.table(instrument = inst, coef = NA, se = NA, t = NA,
                      F_eff = NA, p = NA, N = nrow(d)))
  ct <- coeftable(m)[inst, ]
  data.table(instrument = inst,
             coef = ct[["Estimate"]], se = ct[["Std. Error"]],
             t = ct[["t value"]],
             F_eff = ct[["t value"]]^2,
             p = ct[["Pr(>|t|)"]], N = m$nobs)
}

res_single <- rbindlist(lapply(names(FE), function(fe_name) {
  ti  <- (fe_name == "no_pair")
  tab <- rbindlist(lapply(unlist(INST, use.names = FALSE),
                          one_inst_fs, fe_rhs = FE[[fe_name]], ti = ti))
  tab[, fe := fe_name][]
}))
fwrite(res_single, file.path(out_dir, "first_stage_per_instrument.csv"))
cat("\n===== 1. INSTRUMENTS SEULS (F_eff = effective F) =====\n")
print(res_single)


# ===========================================================================
# 2. PREMIER STAGE JOINT PAR FAMILLE + reconciliation des F
# ===========================================================================
cat("\n===== 2. FAMILLES EN JOINT (F de Wald cluster-robust) =====\n")
joint_records <- list()
for (fe_name in names(FE)) {
  ti <- (fe_name == "no_pair")
  cat("\n----- FE:", fe_name, "-----\n")
  for (fam in names(INST)) {
    rhs <- c(INST[[fam]], CTRL_TV, if (ti) CTRL_TI)
    f <- as.formula(sprintf("%s ~ %s | %s",
                            ENDOG, paste(rhs, collapse = " + "),
                            FE[[fe_name]]))
    d <- df[!is.na(get(INST[[fam]][1]))]
    for (var in INST[[fam]][-1]) d <- d[!is.na(get(var))]
    if (ti) for (var in CTRL_TI) d <- d[!is.na(get(var))]
    m <- tryCatch(feols(f, data = d, vcov = CL, notes = FALSE),
                  error = function(e) NULL)
    if (is.null(m)) { cat("[", fam, "] failed\n"); next }
    w <- tryCatch(wald(m, keep = paste0("^", INST[[fam]], "$",
                                         collapse = "|")),
                  error = function(e) NULL)
    cat("[", fam, "] N =", nobs(m))
    if (!is.null(w)) {
      cat(sprintf("  Wald F=%.2f  p=%.3g  df=(%d, %d)\n",
                  w$stat, w$p, w$df1, w$df2))
      joint_records[[paste(fe_name, fam, sep = "/")]] <-
        data.table(fe = fe_name, family = fam, N = nobs(m),
                   wald_F = w$stat, wald_p = w$p,
                   df1 = w$df1, df2 = w$df2)
    } else cat(" wald failed\n")
  }
}
fwrite(rbindlist(joint_records),
       file.path(out_dir, "first_stage_joint_family.csv"))

# --- Reconciliation explicite : fitstat (ivf / kpr) sur le MEME vcov ~pair ---
iv_model <- function(insts, fe_rhs, ti = FALSE) {
  rhs_exo <- c(CTRL_TV, if (ti) CTRL_TI)
  f <- as.formula(sprintf("%s ~ %s | %s | %s ~ %s",
                          Y, paste(rhs_exo, collapse = " + "), fe_rhs,
                          ENDOG, paste(insts, collapse = " + ")))
  d <- df[!is.na(get(insts[1]))]
  for (var in insts[-1]) d <- d[!is.na(get(var))]
  if (ti) for (var in CTRL_TI) d <- d[!is.na(get(var))]
  tryCatch(feols(f, data = d, vcov = CL, notes = FALSE),
           error = function(e) NULL)
}
safe_fit <- function(m, tag) {
  cat("\n[", tag, "]\n")
  if (is.null(m)) { cat("  (model NULL)\n"); return(invisible()) }
  print(tryCatch(fitstat(m, c("ivf", "ivwald", "kpr", "wh", "sargan")),
                 error = function(e) {
                   cat("  fitstat error :", conditionMessage(e), "\n")
                   NULL
                 }))
}
cat("\n----- Reconciliation KP / F (vcov ~pair) -----\n")
safe_fit(iv_model(INST$institutional, FE$pair_full),
         "institutional / pair_full")
safe_fit(iv_model(INST$institutional, FE$no_pair, ti = TRUE),
         "institutional / no_pair")
safe_fit(iv_model(INST$strategic, FE$pair_full),
         "strategic / pair_full")


# ===========================================================================
# 3. INSPECTION shared_ally_atop
# ===========================================================================
cat("\n===== 3. shared_ally_atop =====\n")
d3 <- df[!is.na(shared_ally_atop) & !is.na(allied_atop)]
print(d3[, .(N = .N,
             mean = mean(shared_ally_atop),
             sd   = sd(shared_ally_atop),
             min  = min(shared_ally_atop),
             max  = max(shared_ally_atop),
             cor_ipd  = cor(shared_ally_atop, get(ENDOG),
                             use = "complete.obs"),
             cor_ally = cor(shared_ally_atop, allied_atop,
                             use = "complete.obs"))])
cat("\nTop 10 paires-annees avec shared_ally le plus eleve :\n")
print(d3[order(-shared_ally_atop)][1:10,
         .(exp_iso3, imp_iso3, year, shared_ally_atop,
           allied_atop, ipd = get(ENDOG))])


# ===========================================================================
# 4. ANDERSON-RUBIN par grille (polyarchy_dist, juste-identifie)
#    NOTE : sur LHS = trade_value (level), la dependance domine ; on calcule
#    aussi avec LHS = log(trade+1) pour interpretabilite.
# ===========================================================================
ar_grid <- function(Zname, fe_rhs, ti = FALSE,
                    grid = seq(-3, 3, by = 0.01),
                    lhs = Y) {
  rhs_exo <- c(CTRL_TV, if (ti) CTRL_TI)
  d <- df[!is.na(get(Zname))]
  if (ti) for (var in CTRL_TI) d <- d[!is.na(get(var))]
  d[, .lhs := if (lhs == "log_trade1") log(get(Y) + 1) else get(lhs)]
  pvals <- vapply(grid, function(b0) {
    d[, .ytilde := .lhs - b0 * get(ENDOG)]
    f <- as.formula(sprintf(".ytilde ~ %s | %s",
                            paste(c(Zname, rhs_exo), collapse = " + "),
                            fe_rhs))
    tryCatch(coeftable(feols(f, data = d, vcov = CL,
                              notes = FALSE))[Zname, "Pr(>|t|)"],
             error = function(e) NA_real_)
  }, numeric(1))
  d[, c(".lhs", ".ytilde") := NULL]
  accepted <- grid[!is.na(pvals) & pvals > 0.05]
  list(set = if (length(accepted)) range(accepted) else NA,
       n_accept = length(accepted),
       hits_edge = length(accepted) &&
                   (min(accepted) <= min(grid) ||
                    max(accepted) >= max(grid)))
}
cat("\n===== 4. ANDERSON-RUBIN (polyarchy_dist) =====\n")
for (lhs in c(Y, "log_trade1")) {
  cat("\n--- LHS =", lhs, "---\n")
  for (fe_name in c("pair_full", "no_pair")) {
    ar <- ar_grid("polyarchy_dist", FE[[fe_name]],
                  ti = (fe_name == "no_pair"),
                  grid = seq(-2, 2, by = 0.05),
                  lhs = lhs)
    cat("  FE:", fe_name, " IC AR 95% =",
        if (length(ar$set) == 1 && is.na(ar$set)) "vide"
        else paste(round(ar$set, 3), collapse = " ; "),
        sprintf("  | %d pts acceptes", ar$n_accept),
        if (isTRUE(ar$hits_edge)) "  <-- TOUCHE LE BORD" else "",
        "\n")
  }
}

cat("\n--- Diagnostics termines. Aucun bootstrap lance. ---\n")
