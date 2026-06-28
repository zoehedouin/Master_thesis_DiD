# =============================================================================
# 08_zeros_diagnostic.R — DIAGNOSTIC DES ZEROS sur le panel pkey du §4.1.
# -----------------------------------------------------------------------------
# But : AVANT d'implementer le dist_lag_het (§4.2) et de figer la transformation
# de l'outcome, decrire la structure des zeros du commerce sur le panel EXACT du
# dCDH (§4.1). DIAGNOSTIC DE DONNEES UNIQUEMENT : aucune estimation, aucun
# did_multiplegt_dyn, aucun chiffre fabrique -- on lit le panel et on le decrit.
# Leger en RAM (agregations seulement) -> tourne sur l'ECHANTILLON COMPLET (pas
# de sous-echantillonnage des controles, pas de N_CTRL).
#
# Construction du panel pkey reprise VERBATIM de 08_ols.R §4.1 :
#   read_parquet_safe(PATH_SANCTIONS_PANEL) ; pkey non ordonnee ; fenetre
#   2008-2023 ; trade_tot = sum(trade_value) ; n_active_core = max(...) ;
#   tier 0/1/2-5/6+. AUCUN sous-echantillonnage.
#
# Sorties : 08_ols/tables/tab_zeros_diagnostic.csv (lignes longues etiquetees)
#           + recap console + 08_ols/08_zeros_report.md.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})
# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))
})
PART <- "08_ols"
PATH_TAB <- out_tab("EventStudy")

YR_MIN <- 2008L; YR_MAX <- 2023L          # fenetre VERBATIM du §4.1
PRE  <- 2018:2021                          # base pre-guerre (cf. 06/07)
POST <- 2022:2023                          # escalade

# Collecteur de lignes longues (section, label, metric, value, note).
ROWS <- list()
add <- function(section, label, metric, value = NA_real_, note = NA_character_)
  ROWS[[length(ROWS) + 1L]] <<- data.table(section = section, label = as.character(label),
    metric = metric, value = suppressWarnings(as.numeric(value)), note = as.character(note))

# ---- 1. Reconstruction du panel pkey (VERBATIM §4.1) ------------------------
log_step("1. Reconstruction du panel pkey (verbatim 08_ols.R §4.1, sample COMPLET).")
d <- read_parquet_safe(PATH_SANCTIONS_PANEL)
d[, pkey := ifelse(exp_iso3 < imp_iso3,
                   paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]
pk <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot     = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core),
  n_active_all  = max(sanc_n_active_all)),
  by = .(pkey, year)]
pk[, tier := fcase(n_active_core == 0L, 0L,
                   n_active_core == 1L, 1L,
                   n_active_core <= 5L, 2L,
                   default = 3L)]
pk[, is_zero := as.integer(trade_tot == 0)]
pk[, rus := grepl("(^|_)RUS($|_)", pkey)]
rm(d); gc(verbose = FALSE)

ever_treated <- pk[tier > 0L, unique(pkey)]
pk[, ever := as.integer(pkey %in% ever_treated)]
n_pairs <- uniqueN(pk$pkey)
cat(sprintf("  - pkey-year obs : %d | paires : %d | ever-traitees : %d | jamais-traitees : %d\n",
            nrow(pk), n_pairs, length(ever_treated), n_pairs - length(ever_treated)))
add("0_panel", "obs_pkey_year", "n", nrow(pk))
add("0_panel", "pairs", "n", n_pairs)
add("0_panel", "pairs_ever_treated", "n", length(ever_treated))
add("0_panel", "pairs_never_treated", "n", n_pairs - length(ever_treated))
add("0_panel", "window", "years", NA, sprintf("%d-%d", YR_MIN, YR_MAX))

# ---- 2. UNITES de trade_value (situer le "+1") ------------------------------
log_step("2. Unites de trade_value : quantiles des flux strictement positifs.")
pos <- pk[trade_tot > 0, trade_tot]
qpos <- quantile(pos, c(0, .01, .05, .50), names = FALSE)
cat(sprintf("  - flux > 0 : min=%.4f | p1=%.4f | p5=%.4f | mediane=%.3f (unite supposee BACI ~ milliers USD)\n",
            qpos[1], qpos[2], qpos[3], qpos[4]))
cat(sprintf("  - le '+1' (de log(trade+1)) vaut %.1f%% de la mediane et %.0f%% du p5 des flux positifs.\n",
            100/qpos[4], 100/qpos[3]))
for (nm in c("min", "p1", "p5", "median"))
  add("2_units_positive_flows", nm, "trade_value",
      switch(nm, min = qpos[1], p1 = qpos[2], p5 = qpos[3], median = qpos[4]),
      note = "BACI ~ thousands USD (to confirm)")
add("2_units_positive_flows", "plus1_vs_median", "pct", 100/qpos[4], "weight of +1 vs median positive flow")
add("2_units_positive_flows", "plus1_vs_p5", "pct", 100/qpos[3], "weight of +1 vs p5 positive flow")

# ---- 3. PART DE ZEROS (trade_tot == 0) --------------------------------------
log_step("3. Part de zeros par coupe.")
g_share <- mean(pk$is_zero)
cat(sprintf("  - GLOBAL : %.2f%% de zeros (%d / %d obs)\n", 100*g_share, sum(pk$is_zero), nrow(pk)))
add("3_zeros_global", "all", "share_zero", g_share)
add("3_zeros_global", "all", "n_zero", sum(pk$is_zero))
add("3_zeros_global", "all", "n_obs", nrow(pk))

zy <- pk[, .(n_obs = .N, n_zero = sum(is_zero), share_zero = mean(is_zero)), by = year][order(year)]
cat("  - par annee (surveiller 2014 et 2022) :\n"); print(zy)
for (i in seq_len(nrow(zy))) add("3_zeros_by_year", zy$year[i], "share_zero", zy$share_zero[i],
                                 note = sprintf("n_zero=%d / n_obs=%d", zy$n_zero[i], zy$n_obs[i]))

zt <- pk[, .(n_obs = .N, n_zero = sum(is_zero), share_zero = mean(is_zero)), by = tier][order(tier)]
cat("  - par tier :\n"); print(zt)
for (i in seq_len(nrow(zt))) add("3_zeros_by_tier", zt$tier[i], "share_zero", zt$share_zero[i],
                                 note = sprintf("n_zero=%d / n_obs=%d", zt$n_zero[i], zt$n_obs[i]))

ze <- pk[, .(n_obs = .N, n_zero = sum(is_zero), share_zero = mean(is_zero)), by = ever][order(ever)]
cat("  - ever-traitees vs jamais-traitees :\n"); print(ze)
for (i in seq_len(nrow(ze))) add("3_zeros_by_evertreated",
    ifelse(ze$ever[i] == 1L, "ever_treated", "never_treated"), "share_zero", ze$share_zero[i],
    note = sprintf("n_zero=%d / n_obs=%d", ze$n_zero[i], ze$n_obs[i]))

# Sous-panel Russie (pkey contenant RUS) : global + par annee
rg <- mean(pk[rus == TRUE, is_zero])
cat(sprintf("  - sous-panel RUSSIE : %.2f%% de zeros (%d paires-RUS)\n",
            100*rg, uniqueN(pk[rus == TRUE, pkey])))
add("3_zeros_russia_global", "russia_all", "share_zero", rg,
    note = sprintf("%d russia pairs", uniqueN(pk[rus == TRUE, pkey])))
zry <- pk[rus == TRUE, .(n_obs = .N, n_zero = sum(is_zero), share_zero = mean(is_zero)), by = year][order(year)]
cat("  - Russie par annee :\n"); print(zry)
for (i in seq_len(nrow(zry))) add("3_zeros_russia_by_year", zry$year[i], "share_zero", zry$share_zero[i],
                                  note = sprintf("n_zero=%d / n_obs=%d", zry$n_zero[i], zry$n_obs[i]))

# ---- 4. STRUCTUREL vs INTERMITTENT (au sein des paires) ---------------------
log_step("4. Zeros structurels vs intermittents (au sein des paires).")
pc <- pk[, .(n_years = .N, n_zero = sum(is_zero), ever = ever[1], rus = rus[1]), by = pkey]
pc[, type := fcase(n_zero == n_years, "always_zero",
                   n_zero == 0L,      "always_positive",
                   default = "intermittent")]
ct <- pc[, .N, by = type][order(type)]
cat("  - typologie des paires :\n"); print(ct)
for (i in seq_len(nrow(ct))) add("4_pair_structure", ct$type[i], "n_pairs", ct$N[i],
                                 note = sprintf("%.1f%% of pairs", 100*ct$N[i]/n_pairs))
# Croisement avec ever-traitee / controle
cx <- pc[, .N, by = .(grp = ifelse(ever == 1L, "ever_treated", "control"), type)][order(grp, type)]
cat("  - typologie x statut :\n"); print(cx)
for (i in seq_len(nrow(cx))) add("4_pair_structure_x_status", paste(cx$grp[i], cx$type[i], sep = ":"),
                                 "n_pairs", cx$N[i])
# Focus Russie
cr <- pc[rus == TRUE, .N, by = type][order(type)]
cat("  - paires Russie par type :\n"); print(cr)
for (i in seq_len(nrow(cr))) add("4_pair_structure_russia", cr$type[i], "n_pairs", cr$N[i])

# ---- 5. ZEROS INDUITS PAR LE TRAITEMENT (le plus important) -----------------
log_step("5. Zeros induits par le traitement (positif pre -> zero reel post).")
# Pre = moyenne 2018-2021 ; post = etat 2022-2023. "Induit" = pre>0 ET tous les
# post observes == 0 (passage a zero reel). Restreint aux paires ever-traitees.
prepost <- pk[, .(
  pre_mean   = mean(trade_tot[year %in% PRE]),
  pre_n      = sum(year %in% PRE),
  post_n     = sum(year %in% POST),
  post_max   = ifelse(any(year %in% POST), max(trade_tot[year %in% POST]), NA_real_),
  post_zero_all = ifelse(any(year %in% POST), as.integer(all(trade_tot[year %in% POST] == 0)), NA_integer_),
  tier_post  = ifelse(any(year %in% POST), max(tier[year %in% POST]), NA_integer_),
  ever = ever[1], rus = rus[1]), by = pkey]
induced <- prepost[ever == 1L & is.finite(pre_mean) & pre_mean > 0 &
                   !is.na(post_zero_all) & post_zero_all == 1L]
n_ind <- nrow(induced); n_ind_rus <- induced[rus == TRUE, .N]
n_ever_eval <- prepost[ever == 1L & is.finite(pre_mean) & pre_mean > 0 & !is.na(post_zero_all), .N]
cat(sprintf("  - paires ever-traitees evaluables (pre>0, post observe) : %d\n", n_ever_eval))
cat(sprintf("  - dont INDUITES (pre>0 -> post tout-zero) : %d (%.2f%%) ; sous-ensemble Russie : %d\n",
            n_ind, 100*n_ind/max(1, n_ever_eval), n_ind_rus))
add("5_induced_zeros", "ever_treated_evaluable", "n_pairs", n_ever_eval)
add("5_induced_zeros", "induced_pos_to_zero", "n_pairs", n_ind,
    note = sprintf("%.2f%% of evaluable ever-treated", 100*n_ind/max(1, n_ever_eval)))
add("5_induced_zeros", "induced_russia", "n_pairs", n_ind_rus)
# Palier atteint par les induites
if (n_ind > 0) {
  it <- induced[, .N, by = tier_post][order(tier_post)]
  for (i in seq_len(nrow(it))) add("5_induced_zeros_by_tier", it$tier_post[i], "n_pairs", it$N[i])
}
# Lister les paires Russie induites (table)
if (n_ind_rus > 0) {
  rr <- induced[rus == TRUE][order(-pre_mean)]
  cat("  - paires RUSSIE passees de positif a zero en 2022-2023 :\n")
  print(rr[, .(pkey, pre_mean = round(pre_mean, 1), tier_post)])
  for (i in seq_len(nrow(rr))) {
    add("5_induced_zeros_russia_pairs", rr$pkey[i], "pre_mean_2018_2021", rr$pre_mean[i],
        note = sprintf("tier_post=%d", rr$tier_post[i]))
  }
} else cat("  - aucune paire Russie induite (pre>0 -> post tout-zero).\n")

# ---- 6. SENSIBILITE DE LA TRANSFORMATION (sans estimer) ---------------------
log_step("6. Sensibilite de la transformation : log(trade+1) vs IHS vs log(positifs).")
pk[, `:=`(log1p = log(trade_tot + 1),
          ihs   = asinh(trade_tot),
          logpos = ifelse(trade_tot > 0, log(trade_tot), NA_real_))]
n_zero_total <- sum(pk$is_zero)
cat(sprintf("  - IHS et log(trade+1) sont definis sur les %d obs ; log(positifs) en perd %d (les zeros).\n",
            nrow(pk), n_zero_total))
add("6_transform", "obs_total", "n", nrow(pk))
add("6_transform", "obs_kept_by_ihs_lost_by_logpos", "n", n_zero_total, "= number of zeros")
# Correlations globales
c_l1_ihs <- cor(pk$log1p, pk$ihs)
c_l1_lp  <- cor(pk[trade_tot > 0, log1p], pk[trade_tot > 0, logpos])
cat(sprintf("  - cor(log1p, IHS) global = %.5f ; cor(log1p, log(pos)) sur positifs = %.5f\n", c_l1_ihs, c_l1_lp))
add("6_transform", "cor_log1p_ihs_global", "cor", c_l1_ihs)
add("6_transform", "cor_log1p_logpos_positives", "cor", c_l1_lp)
# Queue gauche : deciles bas des positifs + les zeros
p10 <- quantile(pos, 0.10, names = FALSE)
tail_pos <- pk[trade_tot > 0 & trade_tot <= p10]
cat(sprintf("  - queue gauche (positifs <= p10 = %.3f) : %d obs ; ecart moyen |log1p - IHS| = %.4f\n",
            p10, nrow(tail_pos), mean(abs(tail_pos$log1p - tail_pos$ihs))))
cat(sprintf("    moyennes en queue : log1p=%.4f | IHS=%.4f | log(pos)=%.4f\n",
            mean(tail_pos$log1p), mean(tail_pos$ihs), mean(tail_pos$logpos)))
add("6_transform_left_tail", "p10_positive", "threshold", p10)
add("6_transform_left_tail", "n_obs_tail", "n", nrow(tail_pos))
add("6_transform_left_tail", "mean_abs_gap_log1p_ihs", "value", mean(abs(tail_pos$log1p - tail_pos$ihs)))
add("6_transform_left_tail", "mean_log1p", "value", mean(tail_pos$log1p))
add("6_transform_left_tail", "mean_ihs", "value", mean(tail_pos$ihs))
add("6_transform_left_tail", "mean_logpos", "value", mean(tail_pos$logpos))
# Sur les zeros : log1p = 0, IHS = 0, log(pos) = NA (illustration du pari implicite)
add("6_transform_left_tail", "zeros_log1p_value", "value", 0, "log(0+1)=0")
add("6_transform_left_tail", "zeros_ihs_value", "value", 0, "asinh(0)=0")
add("6_transform_left_tail", "zeros_logpos_value", "value", NA, "log(0) undefined -> dropped")
# Ecart asymptotique log1p vs IHS sur gros flux (~log(2))
big <- pk[trade_tot > quantile(pos, 0.90, names = FALSE)]
add("6_transform", "mean_gap_ihs_minus_log1p_top10pct", "value", mean(big$ihs - big$log1p),
    note = "approx log(2)=0.693 for large flows")

# ---- Ecriture de la table longue + recap -----------------------------------
diag <- rbindlist(ROWS, use.names = TRUE)
fwrite(diag, file.path(PATH_TAB, "tab_zeros_diagnostic.csv"))
cat("\n=============================================================\n")
cat("RECAP — tab_zeros_diagnostic.csv ecrit (", nrow(diag), "lignes ).\n", sep = "")
cat(sprintf("  zeros global = %.2f%% | Russie = %.2f%% | induits pos->0 (ever) = %d (Russie %d)\n",
            100*g_share, 100*rg, n_ind, n_ind_rus))
cat(sprintf("  paires : always_zero=%d, always_positive=%d, intermittent=%d (sur %d)\n",
            ct[type=="always_zero", N], ct[type=="always_positive", N],
            ct[type=="intermittent", N], n_pairs))
cat(sprintf("  IHS conserve %d obs (zeros) que log(positifs) perd ; cor(log1p,IHS)=%.4f\n",
            n_zero_total, c_l1_ihs))
cat("=============================================================\n")
log_step("08_zeros_diagnostic.R termine.")
