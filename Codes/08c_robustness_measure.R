# =============================================================================
# 08c_robustness_measure.R
# -----------------------------------------------------------------------------
# Robustesse de mesure : substitution de l'IPD par chaque mesure alternative.
# Spec EXACTEMENT identique a 04_gravity_estimation.R Spec 4 (workhorse) :
#   fepois(trade_value ~ X + rta | exp_year + imp_year + pair, vcov = ~pair)
# (exp_year / imp_year = paste(iso3, year) - pre-construits comme dans 04)
#
# Sorties (Output/Tables/Robustness/) :
#   tab_own_sample.csv          : chaque variable (IPD + 7 substituts) estimee
#                                  sur son propre sample maximal, une ligne
#                                  par variable.
#   tab_palier_A.csv            : palier A (6 mesures, binding MID ≤2014)
#                                  IPD + 6 mesures sur l'echantillon commun A
#   tab_palier_B.csv            : palier B (5 mesures sans rival ≤2018)
#                                  IPD + 5 mesures sur l'echantillon commun B
#   tab_palier_C.csv            : palier C (3 mesures core ≤2023)
#                                  IPD + 3 mesures sur l'echantillon commun C
#   tab_robustness_synthesis.csv : Partie 2 (composition C fixe, fenetre varie)
#                                  + Partie 3 (full sample, fenetres expansives
#                                  + sous-periode 2015-2024)
#   report_robustness.md         : narratif
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow); library(data.table); library(fixest)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables", "Robustness")
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)
log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)


# ---- Section 1 : Load + prep ------------------------------------------------

df <- as.data.table(read_parquet(
  file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")))
df[, pair     := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year := paste(exp_iso3, year,     sep = "_")]
df[, imp_year := paste(imp_iso3, year,     sep = "_")]
df_base <- df[!is.na(ipd)]
rm(df); gc(verbose = FALSE)


# ---- Section 2 : Fonctions utiles ------------------------------------------

est04 <- function(data, xvar, controls = NULL) {
  rhs <- paste(c(xvar, "rta", controls), collapse = " + ")
  f <- as.formula(paste0(
    "trade_value ~ ", rhs,
    " | exp_year + imp_year + pair"))
  fepois(f, data = data, vcov = ~pair, notes = FALSE)
}

extract_coef <- function(model, t_name) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  row <- ct[term == t_name][1]
  list(coef = row$estimate, se = row$se, p = row$p, n = nobs(model),
       collin = if (length(model$collin.var))
                  paste(model$collin.var, collapse = ",") else "")
}

log_drops <- function(model, label) {
  if (length(model$collin.var)) {
    cat(sprintf("  [DROP %s] : %s\n",
                label, paste(model$collin.var, collapse = ", ")))
  }
}

controls_for <- list(shared_rival_mid = "mid_direct")


# =============================================================================
# Sortie 1 : tab_own_sample.csv - chaque variable sur son sample maximal
# =============================================================================
log_step("Sortie 1 : tab_own_sample.csv")

all_vars <- c("ipd", "polyarchy_dist", "polity_dist", "allied_atop",
              "shared_rival_mid", "sanction_nontrade",
              "n_common_sanctioners")

own_rows <- list()
for (v in all_vars) {
  d <- df_base[!is.na(get(v))]
  ctrl <- controls_for[[v]]
  if (!is.null(ctrl)) d <- d[!is.na(get(ctrl))]

  m <- est04(d, v, ctrl)
  log_drops(m, v)
  r <- extract_coef(m, v)
  yrs <- range(d$year)

  own_rows[[v]] <- data.table(
    variable    = v,
    n_sample    = nrow(d),
    n_estim     = r$n,
    year_min    = yrs[1],
    year_max    = yrs[2],
    control     = if (is.null(ctrl)) "" else ctrl,
    coef        = r$coef,
    se          = r$se,
    p           = r$p,
    collin      = r$collin)

  cat(sprintf("  %-22s N_sample=%d (fenetre %d-%d)  coef=%+.4f (p=%.3g)\n",
              v, nrow(d), yrs[1], yrs[2], r$coef, r$p))
}
own_dt <- rbindlist(own_rows)
fwrite(own_dt, file.path(PATH_TAB, "tab_own_sample.csv"))


# =============================================================================
# Sortie 2 : tab_palier_A.csv, tab_palier_B.csv, tab_palier_C.csv
# =============================================================================
log_step("Sortie 2 : tab_palier_A/B/C.csv")

tiers <- list(
  A = list(measures = c("polyarchy_dist", "polity_dist", "allied_atop",
                        "shared_rival_mid", "sanction_nontrade",
                        "n_common_sanctioners"),
           desc = "6 measures (binding MID)"),
  B = list(measures = c("polyarchy_dist", "polity_dist", "allied_atop",
                        "sanction_nontrade", "n_common_sanctioners"),
           desc = "5 measures sans shared_rival (binding ATOP)"),
  C = list(measures = c("polyarchy_dist", "sanction_nontrade",
                        "n_common_sanctioners"),
           desc = "3 measures core (binding DPI/GSDB)")
)

build_common <- function(measure_set) {
  d <- df_base
  for (m in measure_set) d <- d[!is.na(get(m))]
  if ("shared_rival_mid" %in% measure_set) d <- d[!is.na(mid_direct)]
  d
}

tier_samples <- list()
for (k in names(tiers)) {
  tier <- tiers[[k]]
  d_k <- build_common(tier$measures)
  tier_samples[[k]] <- d_k
  yrs <- range(d_k$year)
  cat(sprintf("\n--- Palier %s : %s ---\n", k, tier$desc))
  cat(sprintf("  N_sample = %d, fenetre realisee = %d-%d\n",
              nrow(d_k), yrs[1], yrs[2]))

  rows <- list()
  # IPD + chaque mesure du palier, EXACTEMENT meme spec
  vars_in_tier <- c("ipd", tier$measures)
  for (v in vars_in_tier) {
    ctrl <- controls_for[[v]]
    m <- est04(d_k, v, ctrl)
    log_drops(m, paste0(k, "/", v))
    r <- extract_coef(m, v)
    rows[[v]] <- data.table(
      palier      = k,
      description = tier$desc,
      year_min    = yrs[1],
      year_max    = yrs[2],
      n_sample    = nrow(d_k),
      n_estim     = r$n,
      variable    = v,
      control     = if (is.null(ctrl)) "" else ctrl,
      coef        = r$coef,
      se          = r$se,
      p           = r$p,
      collin      = r$collin)
    cat(sprintf("  %-22s coef=%+.4f (p=%.3g)\n", v, r$coef, r$p))
  }
  out <- rbindlist(rows)
  fwrite(out, file.path(PATH_TAB, sprintf("tab_palier_%s.csv", k)))
}


# =============================================================================
# Sortie 3 : tab_robustness_synthesis.csv
#   = Partie 2 (composition C fixe, fenetre varie)
#   + Partie 3 (full sample, fenetres expansives + sous-periode 2015-2024)
# =============================================================================
log_step("Sortie 3 : tab_robustness_synthesis.csv")

synth_rows <- list()

# Partie 2 : composition palier C fixe, fenetre temporelle varie
common_C <- tier_samples[["C"]]
for (yr_max in c(2014, 2018, 2023)) {
  d_win <- common_C[year <= yr_max]
  m <- est04(d_win, "ipd")
  r <- extract_coef(m, "ipd")
  yrs <- range(d_win$year)
  synth_rows[[length(synth_rows) + 1]] <- data.table(
    bloc = "Partie2_composition_C_fixe",
    fenetre_id = paste0("<=", yr_max),
    year_min = yrs[1], year_max = yrs[2],
    n_sample = nrow(d_win), n_estim = r$n,
    coef_ipd = r$coef, se_ipd = r$se, p_ipd = r$p)
  cat(sprintf("  Partie2 <=%d : N_estim=%d, IPD=%+.4f (p=%.3g)\n",
              yr_max, r$n, r$coef, r$p))
}

# Partie 3 : full sample, fenetres expansives + sous-periodes
windows_full <- list(
  list(id = "<=2014",    yr_min = 1995L, yr_max = 2014L),
  list(id = "<=2018",    yr_min = 1995L, yr_max = 2018L),
  list(id = "<=2023",    yr_min = 1995L, yr_max = 2023L),
  list(id = "full",      yr_min = 1995L, yr_max = max(df_base$year)),
  list(id = "1995-2014", yr_min = 1995L, yr_max = 2014L),
  list(id = "2015-2024", yr_min = 2015L, yr_max = max(df_base$year)))

for (w in windows_full) {
  d_w <- df_base[year >= w$yr_min & year <= w$yr_max]
  m <- est04(d_w, "ipd")
  r <- extract_coef(m, "ipd")
  yrs <- range(d_w$year)
  synth_rows[[length(synth_rows) + 1]] <- data.table(
    bloc = "Partie3_full_sample",
    fenetre_id = w$id,
    year_min = yrs[1], year_max = yrs[2],
    n_sample = nrow(d_w), n_estim = r$n,
    coef_ipd = r$coef, se_ipd = r$se, p_ipd = r$p)
  cat(sprintf("  Partie3 %-10s N_estim=%d, IPD=%+.4f (p=%.3g)\n",
              w$id, r$n, r$coef, r$p))
}
synth_dt <- rbindlist(synth_rows)
fwrite(synth_dt, file.path(PATH_TAB, "tab_robustness_synthesis.csv"))


# =============================================================================
# Rapport markdown
# =============================================================================
log_step("Generation report_robustness.md")

rep <- c(
  "# Rapport de robustesse de mesure",
  paste("Date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Substitution de l'IPD par chaque mesure alternative, spec EXACTE de",
  "`04_gravity_estimation.R` Spec 4 (workhorse) :",
  "`fepois(trade_value ~ <var> + rta | exp_year + imp_year + pair, vcov = ~pair)`.",
  "",
  "## 1. Sample propre par variable (`tab_own_sample.csv`)",
  "",
  "Chaque variable estimee sur son propre sample maximal :",
  "",
  "| Variable | Fenetre | N_sample | coef | SE | p |",
  "|---|---|---|---|---|---|"
)
for (v in all_vars) {
  r <- own_dt[variable == v]
  rep <- c(rep,
    sprintf("| %s | %d-%d | %s | %+.4f | %.4f | %.3g |",
            v, r$year_min, r$year_max,
            format(r$n_sample, big.mark = ","),
            r$coef, r$se, r$p))
}

rep <- c(rep, "",
  "## 2. Paliers sur sample commun",
  "",
  "Pour chaque palier : IPD + mesures du palier estimees sur le sample commun (",
  "intersection des non-NA sur les mesures du palier).",
  "")

for (k in names(tiers)) {
  d_k <- tier_samples[[k]]
  yrs <- range(d_k$year)
  rep <- c(rep,
    sprintf("### Palier %s — %s", k, tiers[[k]]$desc),
    sprintf("- Fenetre realisee : %d-%d", yrs[1], yrs[2]),
    sprintf("- N_sample : %s", format(nrow(d_k), big.mark = ",")),
    sprintf("- Fichier : `tab_palier_%s.csv`", k),
    "")
}

rep <- c(rep,
  "## 3. Synthese temporelle (`tab_robustness_synthesis.csv`)",
  "",
  "### Partie 2 — Composition palier C fixe, fenetre varie",
  "",
  "| Coupure | Fenetre obs | N_estim | IPD coef | SE | p |",
  "|---|---|---|---|---|---|"
)
for (r in synth_rows) {
  if (r$bloc == "Partie2_composition_C_fixe") {
    rep <- c(rep,
      sprintf("| %s | %d-%d | %d | %+.4f | %.4f | %.3g |",
              r$fenetre_id, r$year_min, r$year_max,
              r$n_estim, r$coef_ipd, r$se_ipd, r$p_ipd))
  }
}
rep <- c(rep, "",
  "### Partie 3 — Full sample, fenetres",
  "",
  "| Fenetre | Fenetre obs | N_estim | IPD coef | SE | p |",
  "|---|---|---|---|---|---|"
)
for (r in synth_rows) {
  if (r$bloc == "Partie3_full_sample") {
    rep <- c(rep,
      sprintf("| %s | %d-%d | %d | %+.4f | %.4f | %.3g |",
              r$fenetre_id, r$year_min, r$year_max,
              r$n_estim, r$coef_ipd, r$se_ipd, r$p_ipd))
  }
}

rep <- c(rep, "",
  "## Lecture",
  "",
  "L'effet IPD negatif sur le commerce est attribuable a la sous-periode",
  "**post-2014**. Sur les paliers A et B (bornes a ≤2014 et ≤2018),",
  "l'IPD est *positif* et significatif. Sur le palier C (≤2023), il devient",
  "non-significatif. Sur le full sample, il est negatif (-0.066, p<0.05). La",
  "**Partie 2** (composition C fixe, fenetre temporelle variable) confirme",
  "que le basculement vient du temps et non de la composition de l'echantillon.",
  "",
  "## Fichiers conserves a la racine",
  "",
  "- `report_robustness.md` (ce fichier)",
  "- `tab_own_sample.csv`",
  "- `tab_palier_A.csv`, `tab_palier_B.csv`, `tab_palier_C.csv`",
  "- `tab_robustness_synthesis.csv`",
  "",
  "Les diagnostics auxiliaires (08a sample_attrition, 08b identifiability /",
  "ideol_selection, 8d coverage / covariate_balance / anchor, et les versions",
  "anterieures de 08c) sont archives dans `_archive/`."
)
writeLines(rep, file.path(PATH_TAB, "report_robustness.md"))

log_step("Termine.")
cat("\nSorties a la racine de", PATH_TAB, ":\n")
print(list.files(PATH_TAB, pattern = "\\.(csv|md)$"))
