# =============================================================================
# 08_distlag.R — C : distributed-lag heterogene (de Chaisemartin & D'Haultfoeuille
#                §4.2/4.3), panel pkey (monde OLS/log). Migre l'intention du
#                skeleton dist_lag_het qui etait dans 08_ols.R (retire de la-bas).
# -----------------------------------------------------------------------------
# OBJET. Ou le dCDH dynamique (§4.1) date l'effet au 1er changement de dose et ou
# B (08_sunab_ols.R) est aveugle a 2022 (onset 2014 absorbant), C SEPARE l'effet
# CONTEMPORAIN d'une hausse de dose (beta0) des effets RETARDES (beta1..betaK) :
# "combien de l'impact vient du choc de l'annee t vs de l'accumulation".
#
# PACKAGE REEL (verifie sur le GitHub installe ; le skeleton donnait des noms
# DEVINES, faux) : DistLagHet (chaisemartinPackages/dist_lag_het). Fonction
# estim_RC_model_unified(K, data, group_col, deltaY_col, deltaD_col, D_col,
# X_cols, model, bootstrap, B). Donnees en PREMIERES DIFFERENCES + dummies annee
# en covariables. Sortie : $B_hat (beta_0..beta_K), $se/$ci_lower/$ci_upper si
# bootstrap (resample des GROUPES = clustered pkey), $boot_iterations.
# NOTE PACKAGING : le NAMESPACE upstream declarait useDynLib(distlaghet) alors que
# le package est DistLagHet -> R_init_DistLagHet jamais appele -> routines C++ non
# enregistrees -> tous les .Call echouent. Coquille corrigee a l'install (patch
# NAMESPACE useDynLib -> DistLagHet, reinstall locale). L'estimation reste 100% le
# code du package ; aucun chiffre fabrique. (cf. 08_distlag_report.md.)
#
# Sorties : 08_ols/tables/tab_distlag.csv, 08_ols/figures/es_fig_distlag.png,
#           08_ols/08_distlag_report.md.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(ggplot2)
})
if (!requireNamespace("DistLagHet", quietly = TRUE))
  stop("Package DistLagHet absent. Installer : remotes::install_github('chaisemartinPackages/dist_lag_het') ",
       "PUIS corriger NAMESPACE useDynLib(distlaghet)->useDynLib(DistLagHet) et reinstaller (cf. en-tete). ",
       "Si l'install GitHub echoue : autoriser github.com / codeload.github.com / cloud.r-project.org dans le reseau.")
suppressPackageStartupMessages(library(DistLagHet))
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))
})
PART <- "08_ols"
PATH_TAB <- out_tab("EventStudy")
PATH_FIG <- out_fig("EventStudy")

YR_MIN <- 2008L; YR_MAX <- 2023L
K_LAGS <- 4L            # beta0 + beta1..beta4 (cf. LIMITE ci-dessous)
B_BOOT <- 100L          # replications bootstrap (clustered pkey) ; 100 = bon compromis vitesse/stabilite

# ---- Exclusion reporting-gap (MEME que B/A', issue du tri PARTIE 2) ----------
# Tri reproductible dans 08_sunab_ols.R (tab_reporting_gap_triage.csv) : sur les
# 12 extinctions Russie, les 11 partenaires non-russes gardent un commerce mondial
# BACI substantiel (retention 36-157%) = collapses REELS -> GARDES ; seul BLR_RUS
# reste flague (Russie + Bielorussie toutes deux COMTRADE-dark pour ce flux ;
# 33.5 Md USD -> 0 economiquement impossible). Niveau PAIRE, jamais pays.
pairs_reporting_gap <- c("BLR_RUS")
# Si le tri a flague d'autres paires, on aligne automatiquement (reproductibilite).
.trip <- file.path(PATH_TAB, "tab_reporting_gap_triage.csv")
if (file.exists(.trip)) {
  .t <- fread(.trip); .flag <- .t[grepl("^FLAG", verdict), pkey]
  pairs_reporting_gap <- unique(c(pairs_reporting_gap, .flag))
}

# ---- Panel pkey COMPLET (VERBATIM §4.1) + premieres differences -------------
log_step("Panel pkey (verbatim §4.1) + exclusion reporting-gap + premieres differences.")
d <- read_parquet_safe(PATH_SANCTIONS_PANEL)
d[, pkey := ifelse(exp_iso3 < imp_iso3, paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]
pk <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot     = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core)), by = .(pkey, year)]
pk[, `:=`(log_trade = log(trade_tot + 1), ihs_trade = asinh(trade_tot),
          tier = fcase(n_active_core == 0L, 0L, n_active_core == 1L, 1L,
                       n_active_core <= 5L, 2L, default = 3L))]
rm(d); gc(verbose = FALSE)
n0 <- uniqueN(pk$pkey)
pk <- pk[!(pkey %in% pairs_reporting_gap)]
cat(sprintf("  - exclusion %s : -%d paire(s) | panel : %d obs, %d paires\n",
            paste(pairs_reporting_gap, collapse = ","), n0 - uniqueN(pk$pkey),
            nrow(pk), uniqueN(pk$pkey)))

# Premieres differences (par paire, ordonne par annee) ; le 1er obs/paire = NA.
setorder(pk, pkey, year)
pk[, `:=`(dY      = log_trade - shift(log_trade),
          dY_ihs  = ihs_trade - shift(ihs_trade),
          dD      = tier - shift(tier)), by = pkey]
dd <- pk[!is.na(dY)]                      # exclut la 1ere periode (NA), cf. doc
# Dummies d'annee (controles X_cols, = FE annee dans l'equation en differences).
for (y in sort(unique(dd$year))[-1]) dd[[paste0("yr", y)]] <- as.integer(dd$year == y)
X_COLS <- grep("^yr", names(dd), value = TRUE)
dd_df <- as.data.frame(dd)                # le package indexe data[,X_cols] -> data.frame
cat(sprintf("  - obs en differences : %d | dummies annee : %d | K lags = %d | B boot = %d\n",
            nrow(dd_df), length(X_COLS), K_LAGS, B_BOOT))

# ---- C : dist_lag_het ROBUSTE (base, K lags) en log(trade+1) -----------------
log_step("C : estim_RC_model_unified (base, robuste a l'heterogeneite), bootstrap pkey.")
set.seed(1234)
m_C <- estim_RC_model_unified(
  K = K_LAGS, data = dd_df, group_col = "pkey",
  deltaY_col = "dY", deltaD_col = "dD", D_col = "tier", X_cols = X_COLS,
  model = "base", bootstrap = TRUE, B = B_BOOT, conf_level = 0.95, verbose = FALSE)
robust <- data.table(spec = "robust_log1p", coef = names(m_C$B_hat),
                     lag = 0:K_LAGS, estimate = as.numeric(m_C$B_hat),
                     se = as.numeric(m_C$se), lb = as.numeric(m_C$ci_lower),
                     ub = as.numeric(m_C$ci_upper))
cat(sprintf("  - robuste (log+1) : boot_iterations = %d\n", m_C$boot_iterations))
print(robust[, .(coef, estimate = round(estimate, 4), se = round(se, 4),
                 lb = round(lb, 4), ub = round(ub, 4))])
sum_robust <- sum(robust$estimate)
cat(sprintf("  - effet cumule Sigma beta (long terme) = %.4f\n", sum_robust))

# ---- C-IHS : meme estimateur robuste, outcome asinh(trade) ------------------
log_step("C-IHS : meme dist_lag_het robuste en IHS (controle de transformation).")
set.seed(1234)
m_Cihs <- estim_RC_model_unified(
  K = K_LAGS, data = dd_df, group_col = "pkey",
  deltaY_col = "dY_ihs", deltaD_col = "dD", D_col = "tier", X_cols = X_COLS,
  model = "base", bootstrap = TRUE, B = B_BOOT, conf_level = 0.95, verbose = FALSE)
robust_ihs <- data.table(spec = "robust_ihs", coef = names(m_Cihs$B_hat),
                         lag = 0:K_LAGS, estimate = as.numeric(m_Cihs$B_hat),
                         se = as.numeric(m_Cihs$se), lb = as.numeric(m_Cihs$ci_lower),
                         ub = as.numeric(m_Cihs$ci_upper))
cat("  - IHS :\n"); print(robust_ihs[, .(coef, estimate = round(estimate, 4), se = round(se, 4))])

# ---- COMPARATEUR NAIF : distributed-lag TWFE (l'objet critique par Th. 3) ----
# Coefficients de lag contamines sous heterogeneite (poids potentiellement
# negatifs). L'ecart naif<->robuste EST la contamination -> argument methodo.
log_step("Comparateur NAIF : feols l(tier,0:4) | pkey + year, cluster pkey.")
m_naive <- feols(log_trade ~ l(tier, 0:K_LAGS) | pkey + year,
                 data = pk, panel.id = ~ pkey + year, cluster = ~ pkey)
nv <- as.data.table(coeftable(m_naive), keep.rownames = "term")
setnames(nv, 2:5, c("estimate", "se", "stat", "p"))
nv <- nv[grepl("tier", term)]
nv[, lag := as.integer(sub(".*l\\(tier, ([0-9]+)\\).*|.*tier:l([0-9]+).*", "\\1\\2", term))]
nv[is.na(lag), lag := as.integer(seq_len(.N) - 1L)]   # repli si nommage different
nv[, `:=`(spec = "naive_log1p", coef = paste0("beta_", lag),
          lb = estimate - 1.96 * se, ub = estimate + 1.96 * se)]
setorder(nv, lag)
cat("  - naif (TWFE distributed-lag) :\n")
print(nv[, .(coef, estimate = round(estimate, 4), se = round(se, 4))])
sum_naive <- sum(nv$estimate)

# ---- Table longue : naif vs robuste vs robuste-IHS + Sigma beta -------------
out <- rbindlist(list(
  nv[, .(spec, coef, lag, estimate, se, lb, ub)],
  robust[, .(spec, coef, lag, estimate, se, lb, ub)],
  robust_ihs[, .(spec, coef, lag, estimate, se, lb, ub)],
  data.table(spec = c("naive_log1p", "robust_log1p", "robust_ihs"),
             coef = "sum_beta", lag = NA_integer_,
             estimate = c(sum_naive, sum_robust, sum(robust_ihs$estimate)),
             se = NA_real_, lb = NA_real_, ub = NA_real_)), use.names = TRUE)
fwrite(out, file.path(PATH_TAB, "tab_distlag.csv"))
cat("  - ecrit tab_distlag.csv\n")

# ---- Figure : profil robuste (beta0..K + IC) avec le naif en overlay --------
fig <- rbind(robust[, .(lag, estimate, lb, ub, spec = "Robust (dist_lag_het)")],
             nv[, .(lag, estimate, lb, ub, spec = "Naive TWFE distributed-lag")])
fig[, spec := factor(spec, levels = c("Robust (dist_lag_het)", "Naive TWFE distributed-lag"))]
pal <- c("Robust (dist_lag_het)" = "#B2182B", "Naive TWFE distributed-lag" = "#2166AC")
p <- ggplot(fig, aes(lag, estimate, color = spec, fill = spec)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.6, position = position_dodge(width = 0.12)) +
  geom_point(size = 2.4, position = position_dodge(width = 0.12)) +
  scale_x_continuous(breaks = 0:K_LAGS,
                     labels = c("beta0\n(contemp.)", paste0("beta", 1:K_LAGS, "\n(lag ", 1:K_LAGS, ")"))) +
  scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"), legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Distributed-lag response of trade to sanction dose (tier), pkey panel",
       subtitle = "Heterogeneity-robust (de Chaisemartin & D'Haultfoeuille) vs naive TWFE. Gap = Theorem-3 contamination.",
       x = NULL, y = "Effect on log(trade+1) of a one-tier dose increase", color = NULL, fill = NULL,
       caption = sprintf("First differences, year dummies, pair-clustered bootstrap (B=%d). Same panel/exclusion as B. K=%d lags.", B_BOOT, K_LAGS)) +
  guides(fill = "none")
ggsave(file.path(PATH_FIG, "es_fig_distlag.png"), p, width = 10, height = 6, dpi = 300)
cat("  - ecrit es_fig_distlag.png\n")

# ---- Comparaison au §4.1 (dCDH AVSQ) : note chiffree ------------------------
path41 <- file.path(PATH_TAB, "tab_dcdh_by_tier.csv")
cmp_note <- "n/a (tab_dcdh_by_tier.csv absent)"
if (file.exists(path41)) {
  d41 <- fread(path41)
  ate41 <- d41[grepl("cross_ge1", model) & term == "ATE", estimate]
  if (length(ate41)) cmp_note <- sprintf("dCDH §4.1 ATE (cross_ge1, onset) = %.4f ; C beta0 = %.4f ; Sigma beta = %.4f",
                                         ate41[1], robust$estimate[robust$lag == 0], sum_robust)
}
cat("  - vs §4.1 :", cmp_note, "\n")

cat("\n=============================================================\n")
cat("RECAP — 08_distlag.R (C)\n")
cat(sprintf("  panel : %d obs en differences, %d paires | K=%d | B=%d (cluster pkey)\n",
            nrow(dd_df), uniqueN(dd$pkey), K_LAGS, B_BOOT))
cat(sprintf("  ROBUSTE : beta0 = %.4f | beta1..K = %s | Sigma beta = %.4f\n",
            robust$estimate[1], paste(round(robust$estimate[-1], 4), collapse = ", "), sum_robust))
cat(sprintf("  NAIF    : beta0 = %.4f | beta1..K = %s | Sigma beta = %.4f\n",
            nv$estimate[1], paste(round(nv$estimate[-1], 4), collapse = ", "), sum_naive))
cat(sprintf("  IHS     : beta0 = %.4f | Sigma beta = %.4f\n",
            robust_ihs$estimate[1], sum(robust_ihs$estimate)))
cat("=============================================================\n")
log_step("08_distlag.R termine.")
