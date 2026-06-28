# =============================================================================
# 08_sunab_ols.R — Event study Sun-Abraham en OLS sur logs, panel pkey (monde).
# -----------------------------------------------------------------------------
# Section 08 = monde OLS/log de l'intensite des sanctions (par opposition au PPML
# DIRIGE du 07). Estimateur le plus simple : Sun & Abraham (2021) en OLS sur
# log(trade+1), panel paire NON ORDONNEE (pkey), FE pkey + year. feols gere les
# FE creux -> panel pkey COMPLET (pas de sous-echantillonnage : c'est une
# contrainte propre au dCDH, pas a feols).
#
# Construction du panel pkey reprise VERBATIM du §4.1 (08_ols.R).
# Estimation = VRAIE (feols), imprime des chiffres reels. Aucun chiffre fabrique.
#
# Sorties : 08_ols/tables/tab_sunab_ols.csv,
#           08_ols/figures/es_fig_sunab_ppml_vs_ols.png,
#           08_ols/08_sunab_ols_report.md.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(ggplot2)
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
PATH_FIG <- out_fig("EventStudy")

YR_MIN <- 2008L; YR_MAX <- 2023L           # fenetre VERBATIM du §4.1
PRE  <- 2018:2021; POST <- 2022:2023        # base pre-guerre / escalade (diag. zeros)

# ---- AUDITABLE : paires a exclure pour "trou de reporting" post-2022 ---------
# Sans imputation. Ancre = Bielorussie : "BLR_RUS" est la plus grosse extinction
# pre>0 -> zero post du diagnostic zeros (~33.5 Md USD -> 0), invraisemblable
# comme vrai effondrement (le commerce BLR-RUS a AUGMENTE) -> lacune de
# declaration BACI (la Bielorussie, sanctionnee, cesse de declarer ; les deux
# cotes eteints -> le miroir BACI ne reconstruit rien). Regle : toute paire pkey
# contenant a la fois "BLR" et "RUS". EXTENSIBLE a d'autres non-declarants
# documentes post-2022 (ajouter ci-dessous). NB : exclusion au niveau PAIRE,
# JAMAIS pays -> la Russie reste (c'est le traitement) ; les paires
# Russie-Occident restent (le partenaire occidental declare -> flux reel).
pairs_reporting_gap <- c("BLR_RUS")   # extensible : c("BLR_RUS", "XXX_RUS", ...)

# ---- 1. Panel pkey (VERBATIM §4.1) ------------------------------------------
log_step("1. Panel pkey (verbatim §4.1), sample COMPLET.")
d <- read_parquet_safe(PATH_SANCTIONS_PANEL)
d[, pkey := ifelse(exp_iso3 < imp_iso3,
                   paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]
pk <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot     = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core)),
  by = .(pkey, year)]
pk[, log_trade := log(trade_tot + 1)]
rm(d); gc(verbose = FALSE)
cat(sprintf("  - pkey-year obs : %d | paires : %d\n", nrow(pk), uniqueN(pk$pkey)))

# ---- 2. Liste des extinctions sous traitement + exclusion reporting-gap ------
log_step("2. Extinctions pre>0 -> zero post (ever-traitees) + exclusion reporting-gap.")
ever_treated <- pk[n_active_core > 0L, unique(pkey)]
pp <- pk[, .(
  pre_mean   = mean(trade_tot[year %in% PRE]),
  post_zero_all = ifelse(any(year %in% POST), as.integer(all(trade_tot[year %in% POST] == 0)), NA_integer_),
  tier_post  = ifelse(any(year %in% POST),
                      max(fcase(n_active_core[year %in% POST] == 0L, 0L,
                                n_active_core[year %in% POST] == 1L, 1L,
                                n_active_core[year %in% POST] <= 5L, 2L, default = 3L)), NA_integer_),
  rus = grepl("(^|_)RUS($|_)", pkey[1])), by = pkey]
induced <- pp[pkey %in% ever_treated & is.finite(pre_mean) & pre_mean > 0 &
              !is.na(post_zero_all) & post_zero_all == 1L][order(-pre_mean)]
cat(sprintf("  - paires ever-traitees passant positif(pre 2018-21) -> zero(post 2022-23) : %d (Russie : %d)\n",
            nrow(induced), induced[rus == TRUE, .N]))
cat("  - sous-ensemble RUSSIE (pkey, moy. pre k USD, palier post) :\n")
print(induced[rus == TRUE, .(pkey, pre_mean = round(pre_mean, 1), tier_post)])

# Exclusion (uniforme, servira aussi a C). Verifie l'ancre BLR&RUS.
gap_rule <- unique(pk$pkey)[grepl("BLR", unique(pk$pkey)) & grepl("RUS", unique(pk$pkey))]
stopifnot(all(pairs_reporting_gap %in% gap_rule) || length(gap_rule) == 0)
n_pair0 <- uniqueN(pk$pkey); n_obs0 <- nrow(pk)
pk <- pk[!(pkey %in% pairs_reporting_gap)]
cat(sprintf("  - exclusion reporting-gap %s : -%d paire(s), -%d obs (reste %d paires, %d obs)\n",
            paste(pairs_reporting_gap, collapse = ","),
            n_pair0 - uniqueN(pk$pkey), n_obs0 - nrow(pk), uniqueN(pk$pkey), nrow(pk)))

# ---- 3. Onset / cohorte (Sun-Abraham, traitement binaire absorbant) ---------
log_step("3. Onset / cohorte.")
onset <- pk[n_active_core > 0L, .(onset_year = min(year)), by = pkey]
pk <- merge(pk, onset, by = "pkey", all.x = TRUE)
pk[, cohort := fifelse(is.na(onset_year), 10000L, onset_year)]  # never-treated = sentinelle
# Exclure les left-censored (onset == YR_MIN : pas de pre-periode), comme la
# variante full-window 2008-2023 du sunab du 07 (comparabilite).
lc <- pk[cohort != 10000L & onset_year == YR_MIN, uniqueN(pkey)]
pk <- pk[!(cohort != 10000L & onset_year == YR_MIN)]
pk[, rel_time := fifelse(cohort == 10000L, NA_integer_, year - cohort)]
cat(sprintf("  - left-censored (onset %d) exclus : %d paires | cohortes traitees : %d | never-treated : %d\n",
            YR_MIN, lc, uniqueN(pk[cohort != 10000L, cohort]), uniqueN(pk[cohort == 10000L, pkey])))

# ---- 4. ESTIMATION B : Sun-Abraham OLS --------------------------------------
log_step("4. feols Sun-Abraham OLS (log(trade+1) | pkey + year, cluster pkey).")
rng <- range(pk[cohort != 10000L, rel_time])
# Memes conventions que le 07 : bornes binnees +/-5, reference pre-onset (-1).
m_B <- feols(log_trade ~ sunab(cohort, year,
               bin.rel = list("-5" = rng[1]:-5, "5" = 5:rng[2])) | pkey + year,
             data = pk, cluster = ~ pkey)

att <- as.data.table(summary(m_B, agg = "att")$coeftable, keep.rownames = "term")
setnames(att, 2:5, c("estimate", "se", "stat", "p")); att <- att[term == "ATT"]
es <- as.data.table(coeftable(m_B), keep.rownames = "term")
setnames(es, 2:5, c("estimate", "se", "stat", "p"))
es <- es[grepl("year::", term)]
es[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
es[, `:=`(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]; setorder(es, rel_time)
# Moyenne post k>=+1 (lecture demandee), a cote de l'ATT fixest (post k>=0).
att_post1 <- es[rel_time >= 1, mean(estimate)]
cat(sprintf("  - ATT (fixest agg, post k>=0) = %.4f (se %.4f, p %.4g) | moyenne post k>=+1 = %.4f\n",
            att$estimate, att$se, att$p, att_post1))
cat("  - profil event-study OLS :\n")
print(es[, .(rel_time, estimate = round(estimate, 4), se = round(se, 4),
             ci_lo = round(ci_lo, 4), ci_hi = round(ci_hi, 4))])

out_B <- rbindlist(list(
  es[, .(model = "B_ols_pkey", term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
  att[, .(model = "B_ols_pkey", term = "ATT", rel_time = NA_integer_, estimate, se,
          ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)],
  data.table(model = "B_ols_pkey", term = "ATT_post_k>=1", rel_time = NA_integer_,
             estimate = att_post1, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_)),
  use.names = TRUE)
fwrite(out_B, file.path(PATH_TAB, "tab_sunab_ols.csv"))
cat("  - ecrit tab_sunab_ols.csv\n")

# ---- 5. COMPARAISON A (PPML dirige, 07) vs B (OLS pkey) — pont qualitatif ----
log_step("5. Overlay A (PPML dirige, fenetre full 2008-2023) vs B (OLS pkey).")
pathA <- file.path(ANALYSIS_ROOT, "07_ppml", "tables", "tab_eventstudy_sunab.csv")
have_A <- file.exists(pathA)
if (have_A) {
  A <- fread(pathA)
  A <- A[window == "2008_2023" & term == "event_time",
         .(rel_time, estimate, ci_lo, ci_hi)]
  A[, profil := "A : PPML directed (07, MRT FE)"]
} else cat("  !! 07 event-study CSV introuvable :", pathA, "\n")
B <- es[, .(rel_time, estimate, ci_lo, ci_hi)]; B[, profil := "B : OLS pkey log(trade+1)"]
ov <- if (have_A) rbind(A, B) else B
pal <- c("A : PPML directed (07, MRT FE)" = "#2166AC", "B : OLS pkey log(trade+1)" = "#B2182B")
p <- ggplot(ov, aes(rel_time, estimate, color = profil, fill = profil)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = 0, lty = 3, color = "grey55") +
  geom_vline(xintercept = 8, lty = 3, color = "grey75") +
  annotate("text", x = 0, y = Inf, label = "onset (~2014)", vjust = 1.4, hjust = -0.05, size = 2.7, color = "grey45") +
  annotate("text", x = 8, y = Inf, label = "~2022 (2014 cohort)", vjust = 1.4, hjust = -0.02, size = 2.7, color = "grey55") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.9) +
  scale_x_continuous(breaks = seq(-5, 9, 1)) +
  scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"), legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Sanctions event study: directed PPML (A) vs unordered-pair OLS (B)",
       subtitle = "Qualitative bridge only: A->B varies scale (PPML->OLS), geometry (directed+MRT->pkey), and treatment definition AT ONCE.",
       x = "Years relative to sanction onset", y = "Effect on trade with Russia (semi-elasticity)",
       color = NULL, fill = NULL,
       caption = "A = 07_ppml full window 2008-2023 (directed non-commercial, exp^year+imp^year+pair). B = OLS log(trade+1), pkey + year FE, pair-clustered.")
ggsave(file.path(PATH_FIG, "es_fig_sunab_ppml_vs_ols.png"), p, width = 10, height = 6, dpi = 300)
cat("  - ecrit es_fig_sunab_ppml_vs_ols.png", if (!have_A) "(B seul : A indisponible)" else "", "\n")

cat("\n=============================================================\n")
cat("RECAP — 08_sunab_ols.R\n")
cat(sprintf("  panel pkey : %d obs, %d paires (apres exclusion reporting-gap %s)\n",
            nrow(pk), uniqueN(pk$pkey), paste(pairs_reporting_gap, collapse = ",")))
cat(sprintf("  ATT OLS (post k>=0) = %.4f ; moyenne post k>=+1 = %.4f\n", att$estimate, att_post1))
cat(sprintf("  extinctions sous traitement : %d (Russie %d) ; reporting-gap exclus : %d paire(s)\n",
            nrow(induced), induced[rus == TRUE, .N], length(pairs_reporting_gap)))
cat("=============================================================\n")
log_step("08_sunab_ols.R termine.")
