# =============================================================================
# 10_event_study_sanctions.R
# -----------------------------------------------------------------------------
# Event study staggered de l'effet des sanctions (choc geopolitique) sur le
# commerce bilateral, cadre gravite PPML. PHASE 1 (minimum viable).
#
# Spec PPML identique a 04 : FE exp_iso3^year + imp_iso3^year + pair,
# cluster = ~pair, zeros gardes. Traitement = sanction NON-COMMERCIALE
# (sanction_nontrade, GSDB-R4) pour eviter la tautologie de l'embargo.
#
#   Etape 0 : construction du traitement (onset, cohort, treated_post)
#   Etape 1 : validation descriptive du traitement (cohortes, Russie)
#   Etape 2 : DiD statique (ancre) + contraste par type (any/trade/non-trade)
#   Etape 3 : event study dynamique Sun & Abraham (sunab)
#
# Sorties : Output/Tables/EventStudy/, Output/Figures/EventStudy/,
#           Output/Reports/report_eventstudy_phase1.md
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "ggplot2")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(ggplot2)
})

# Chemin reel du projet (OneDrive). Les sorties (CSV/PNG/MD) passent par l'I/O
# de base R, qui gere le chemin accentue NFD sans probleme.
PATH_ROOT <- "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis"
PATH_DATA <- file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "EventStudy")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "EventStudy")
PATH_REP  <- file.path(PATH_ROOT, "Output", "Reports")
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_REP, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)  # tous les coeurs dispo (cette machine : fixest single-thread)

log_step <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

# arrow::read_parquet ne sait pas ouvrir un chemin accentue normalise NFD
# (le repertoire OneDrive). On copie d'abord le fichier vers un tempfile ASCII
# (file.copy de base R gere bien le NFD), puis on lit.
read_parquet_safe <- function(path) {
  tmp <- tempfile(fileext = ".parquet")
  ok <- file.copy(path, tmp, overwrite = TRUE)
  if (!ok) stop("Echec de la copie du parquet vers le tempfile : ", path)
  on.exit(unlink(tmp), add = TRUE)
  as.data.table(arrow::read_parquet(tmp))
}

log_step("Setup termine.")


# =============================================================================
# ETAPE 0 : Construction du traitement
# =============================================================================

log_step("Etape 0 : chargement et construction du traitement.")
df <- read_parquet_safe(PATH_DATA)
cat("  - Obs total                :", nrow(df), "\n")
cat("  - Annees                   :", paste(range(df$year), collapse = "-"), "\n")

# Conventions de 04 : pair directionnel pour les FE.
df[, pair := paste(exp_iso3, imp_iso3, sep = "_")]

# sanction_nontrade est SYMETRIQUE (verifie : 0 paire en desaccord entre les
# deux directions). On definit l'onset au niveau de la PAIRE NON ORDONNEE.
df[, pkey := ifelse(exp_iso3 < imp_iso3,
                    paste(exp_iso3, imp_iso3, sep = "_"),
                    paste(imp_iso3, exp_iso3, sep = "_"))]

# Onset = premiere annee ou la paire (non ordonnee) est sous sanction non-trade.
onset <- df[sanction_nontrade == 1, .(onset_year = min(year)), by = pkey]
df <- merge(df, onset, by = "pkey", all.x = TRUE)

df[, ever_sanctioned := !is.na(onset_year)]
df[, rel_time := ifelse(ever_sanctioned, year - onset_year, NA_integer_)]
# Robuste au NA-2024 : derive de l'onset (scalaire fixe par paire), donc les
# flux 2024 restent utilisables meme si l'indicateur en-vigueur est NA en 2024.
df[, treated_post := as.integer(ever_sanctioned & year >= onset_year)]
# Cohorte pour sunab : annee d'onset, 10000 = jamais-traites (controles).
df[, cohort := ifelse(ever_sanctioned, onset_year, 10000L)]

cat("  - Paires (non ordonnees) traitees       :", uniqueN(df[ever_sanctioned == TRUE, pkey]), "\n")
cat("  - Paires (non ordonnees) jamais-traitees :", uniqueN(df[ever_sanctioned == FALSE, pkey]), "\n")


# =============================================================================
# ETAPE 1 : Validation descriptive du traitement (AVANT estimation)
# =============================================================================

log_step("Etape 1 : validation descriptive du traitement.")

# Tailles des cohortes par annee d'onset (au niveau paire non ordonnee).
cohort_sizes <- unique(df[ever_sanctioned == TRUE, .(pkey, onset_year)])[
  , .(n_pairs = .N), by = onset_year][order(onset_year)]
cat("\n  Tailles de cohortes par annee d'onset :\n")
print(cohort_sizes)

n_treated <- uniqueN(df[ever_sanctioned == TRUE, pkey])
n_never   <- uniqueN(df[ever_sanctioned == FALSE, pkey])
n_lc      <- cohort_sizes[onset_year == min(df$year), n_pairs]  # left-censored

# Sanity check Russie : onsets des paires impliquant RUS.
rus <- unique(df[ever_sanctioned == TRUE & grepl("RUS", pkey), .(pkey, onset_year)])
rus_by_year <- rus[, .(n_new_onsets = .N), by = onset_year][order(onset_year)]
cat("\n  Russie - nouvelles paires sanctionnees (onset) par annee :\n")
print(rus_by_year)
cat("  Russie - total partenaires jamais sanctionnes :", nrow(rus), "\n")

# Table de validation : on empile cohortes globales + colonne Russie.
val_tab <- merge(cohort_sizes, rus_by_year, by = "onset_year", all.x = TRUE)
setnames(val_tab, c("onset_year", "n_pairs", "n_new_onsets"),
         c("onset_year", "n_pairs_all", "n_new_onsets_RUS"))
val_tab[is.na(n_new_onsets_RUS), n_new_onsets_RUS := 0L]
fwrite(val_tab, file.path(PATH_TAB, "tab_treatment_validation.csv"))

# Couverture / caveats (lignes meta).
meta_val <- data.table(
  metric = c("n_pairs_treated", "n_pairs_never_treated",
             "n_pairs_left_censored_onset_min_year",
             "n_RUS_treated_partners", "year_min", "year_max",
             "caveat_NA"),
  value  = c(n_treated, n_never, n_lc, nrow(rus),
             min(df$year), max(df$year),
             "sanction_nontrade NA en 2024 (annee entiere) ; treated_post derive de l'onset reste valide"))
fwrite(meta_val, file.path(PATH_TAB, "tab_treatment_validation_meta.csv"))


# =============================================================================
# ETAPE 2 : DiD statique (ancre) + contraste par type
# =============================================================================

log_step("Etape 2 : DiD statique + contraste par type.")

extract_coefs <- function(model, name) {
  n_obs <- nobs(model)  # capter AVANT de creer la colonne 'model' (sinon
                        # data.table resout 'model' comme la colonne, pas l'objet)
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct[, model := name]
  ct[, N := n_obs]
  ct[]
}

# --- Ancre : effet moyen d'etre sous sanction non-commerciale ---------------
log_step("  DiD statique (treated_post, non-commercial).")
tic()
m_static <- fepois(trade_value ~ treated_post + rta |
                     exp_iso3^year + imp_iso3^year + pair,
                   data = df, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_static), "\n")

# --- Contraste par type (replique col. (2) GSDB-R4, Yalcin et al. 2025) ------
log_step("  Contraste : sanction_any.")
tic()
m_any <- fepois(trade_value ~ sanction_any + rta |
                  exp_iso3^year + imp_iso3^year + pair,
                data = df, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_any), "\n")

log_step("  Contraste : sanction_trade + sanction_nontrade.")
tic()
m_split <- fepois(trade_value ~ sanction_trade + sanction_nontrade + rta |
                    exp_iso3^year + imp_iso3^year + pair,
                  data = df, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_split), "\n")

static_csv <- rbindlist(list(
  extract_coefs(m_static, "static_treated_post"),
  extract_coefs(m_any,    "contrast_any"),
  extract_coefs(m_split,  "contrast_trade_nontrade")))
fwrite(static_csv, file.path(PATH_TAB, "tab_static_did.csv"))
cat("\n  --- Resume DiD statique + contraste ---\n")
print(static_csv[!grepl("^rta$", term), .(model, term, estimate = round(estimate, 4),
                                           se = round(se, 4), p = round(p, 4))])


# =============================================================================
# ETAPE 3 : Event study dynamique (Sun & Abraham)
# =============================================================================

log_step("Etape 3 : event study Sun & Abraham (sunab).")

# Fenetre event study : 2008-2023 (cohorente avec le dCDH ; donne 6 ans de
# pre-periode a la vague d'onset 2014). On exclut :
#   - les paires dont l'onset precede la fenetre (left-censored in-window : pas
#     de rel_time = -1, ne contribuent pas aux pre-tendances) ;
#   en gardant les jamais-traites (cohort = 10000) comme controles.
# Cela reduit fortement le nombre de cohortes (donc de termes sunab) -> tractable
# sur 8 Go : le full 1995-2023 (~27 cohortes, ~400 termes) saturait la RAM.
ES_Y0 <- 2008L
df_es <- df[year >= ES_Y0 & (cohort == 10000L | onset_year >= ES_Y0 + 1L)]
cat("  - Obs full :", nrow(df), " | obs event study (", ES_Y0, "-2023, onset>=",
    ES_Y0 + 1L, ") :", nrow(df_es),
    "| cohortes traitees :", uniqueN(df_es[ever_sanctioned == TRUE, cohort]), "\n")

# Binning des extremites de l'event-time pour limiter le nombre de termes et
# stabiliser les bords. On INLINE les bornes numeriques dans la formule : fixest
# evalue sunab() dans l'env. de la formule et ne resout pas une variable-liste
# externe (bin.rel = <var> -> erreur).
rel_rng <- range(df_es[ever_sanctioned == TRUE, rel_time])
fml_es <- as.formula(sprintf(
  "trade_value ~ sunab(cohort, year, bin.rel = list('-5' = %d:-5, '5' = 5:%d)) + rta | exp_iso3^year + imp_iso3^year + pair",
  rel_rng[1], rel_rng[2]))

tic()
m_es <- fepois(fml_es, data = df_es, cluster = ~ pair)
cat("    Time:", toc(), "s | N =", nobs(m_es), "\n")

# ATT agrege.
att <- summary(m_es, agg = "att")
att_ct <- as.data.table(att$coeftable, keep.rownames = "term")
setnames(att_ct, 2:5, c("estimate", "se", "stat", "p"))
cat("\n  --- ATT agrege ---\n"); print(att_ct)

# Coefficients event-time.
ct_es <- as.data.table(coeftable(m_es), keep.rownames = "term")
setnames(ct_es, 2:5, c("estimate", "se", "stat", "p"))
es_dyn <- ct_es[grepl("year::", term)]
es_dyn[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
es_dyn[, `:=`(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]
setorder(es_dyn, rel_time)
cat("\n  --- Coefficients event-time ---\n")
print(es_dyn[, .(rel_time, estimate = round(estimate, 4), se = round(se, 4),
                 ci_lo = round(ci_lo, 4), ci_hi = round(ci_hi, 4))])

# Sortie combinee : event-time + ligne ATT.
out_es <- rbindlist(list(
  es_dyn[, .(term = "event_time", rel_time, estimate, se, ci_lo, ci_hi)],
  att_ct[, .(term = "ATT", rel_time = NA_integer_, estimate, se,
             ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)]),
  use.names = TRUE)
fwrite(out_es, file.path(PATH_TAB, "tab_eventstudy_sunab.csv"))

# --- Figure maitresse : event-time (ggplot, fenetre lisible) ----------------
p_es <- ggplot(es_dyn, aes(rel_time, estimate)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = -0.5, lty = 3, color = "grey60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166AC") +
  geom_line(color = "#2166AC", linewidth = 0.6) +
  geom_point(color = "#2166AC", size = 2) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Effet des sanctions non-commerciales sur le commerce bilateral",
       subtitle = "Event study Sun & Abraham. PPML 3-way FE. IC 95% (cluster paire). k=0 = transition.",
       x = "Temps relatif a l'onset de la sanction (annees)",
       y = "Semi-elasticite (effet sur log trade)",
       caption = "Source : BACI-CEPII, GSDB-R4 (Yalcin et al. 2025). Fenetre 2008-2023, bornes binnees a +/-5.")
ggsave(file.path(PATH_FIG, "es_fig01_sunab_2014.png"),
       p_es, width = 10, height = 6, dpi = 300)


# =============================================================================
# Resume console
# =============================================================================

log_step("Termine. Sorties :")
cat("  Tables  :", PATH_TAB, "\n");  print(list.files(PATH_TAB))
cat("  Figures :", PATH_FIG, "\n");  print(list.files(PATH_FIG))
