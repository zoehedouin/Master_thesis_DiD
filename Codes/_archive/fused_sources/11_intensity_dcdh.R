# =============================================================================
# 11_intensity_dcdh.R
# -----------------------------------------------------------------------------
# PHASE 3 : effet de l'INTENSITE des sanctions (dose) sur le commerce bilateral,
# via de Chaisemartin & D'Haultfoeuille (did_multiplegt_dyn), traitement
# non-binaire NON-ABSORBANT -> capte l'escalade 2014->2022 et la reversibilite,
# ce que l'event study sunab absorbant (binaire/onset) ne peut pas.
#
# Decouverte motivante (cf. report Partie 1) : pour la Russie tous les types de
# sanctions s'allument en 2014 ; 2022 n'est PAS un nouvel onset mais une
# INTENSIFICATION (n_active_core 8 -> 38 -> 46). Le binaire/type/onset sature ;
# seule la dose distingue 2022 de 2014.
#
# Dose = sanc_n_active_core (nb de cases de sanctions commercialement pertinents
# par paire-annee), DISCRETISEE EN PALIERS (GSDB-R4 note 1 : + de sanctions !=
# impact proportionnel -> ordinaliser plutot que lineaire).
#
# Estimateur en LOGS (dCDH est lineaire, hors PPML) : log(trade_value + 1).
# Groupe = paire non ordonnee (pkey) ; temps = year ; cluster = pkey.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(DIDmultiplegtDYN); library(ggplot2)
})

PATH_ROOT <- "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis"
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "EventStudy")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "EventStudy")
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)

log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))

read_parquet_safe <- function(path, ...) {
  tmp <- tempfile(fileext = ".parquet")
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(arrow::read_parquet(tmp, ...))
}

# ---- Donnees : panel paire non ordonnee (pkey) -----------------------------
log_step("Chargement iv_panel + construction panel pkey.")
d <- read_parquet_safe(file.path(PATH_ROOT, "Data/Clean/iv_panel.parquet"))
d[, pkey := ifelse(exp_iso3 < imp_iso3,
                   paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]

# Fenetre : 2008-2023. Couvre une base pre-2014 (6 ans, pour les placebos),
# la vague d'onset 2014, et l'escalade 2022-2023. Restreindre la fenetre est
# rendu necessaire par les 8 Go de RAM (dCDH alloue des matrices ~ N).
YR_MIN <- 2008L; YR_MAX <- 2023L

# Agregation a la paire non ordonnee : commerce = somme des deux directions ;
# dose symetrique -> max (identique dans les deux directions par construction).
pk <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot     = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core),
  n_active_all  = max(sanc_n_active_all)),
  by = .(pkey, year)]
pk[, log_trade := log(trade_tot + 1)]

# Paliers d'intensite (cale sur la distribution : median 1, p90 3, p95 4, p99 5)
pk[, tier := fcase(n_active_core == 0L, 0L,
                   n_active_core == 1L, 1L,
                   n_active_core <= 5L, 2L,
                   default = 3L)]

# CONTRAINTE MEMOIRE (8 Go) : dCDH alloue des matrices ~ nb de groupes (26 565
# paires -> OOM). On garde TOUTES les paires jamais-traitees ? non : on garde
# toutes les paires EVER-traitees (dose>0 au moins une annee) + un ECHANTILLON
# ALEATOIRE de paires jamais-traitees comme controles. L'echantillonnage des
# controles laisse l'ATE non biaise (juste moins precis) ; documente.
N_CTRL <- 4000L
ever_treated <- pk[tier > 0L, unique(pkey)]
never_treated <- setdiff(unique(pk$pkey), ever_treated)
set.seed(1234)
ctrl_keep <- sample(never_treated, min(N_CTRL, length(never_treated)))
pk <- pk[pkey %in% c(ever_treated, ctrl_keep)]
pk[, gid := .GRP, by = pkey]  # id numerique pour dCDH
cat("  - paires ever-traitees :", length(ever_treated),
    "| controles jamais-traites gardes :", length(ctrl_keep),
    "| total groupes :", uniqueN(pk$pkey), "\n")
cat("  - pkey-year obs :", nrow(pk), "\n")
cat("  - repartition paliers (year<=2023) :\n"); print(pk[, .N, by = tier][order(tier)])

# ---- dCDH principal : dose en paliers --------------------------------------
log_step("dCDH (paliers) : effects=4, placebo=2. Peut etre long (single-thread).")
set.seed(1234)
m_tier <- did_multiplegt_dyn(
  df = pk, outcome = "log_trade", group = "gid", time = "year",
  treatment = "tier", effects = 4, placebo = 2,
  cluster = "gid", graph_off = TRUE)  # gid <-> pkey 1:1 => cluster par paire

saveRDS(m_tier, file.path(PATH_TAB, "_dcdh_tier_obj.rds"))
log_step("dCDH paliers termine. Structure de l'objet :")
str(m_tier, max.level = 2)
cat("\n=== results$ATE / Effects / Placebos ===\n")
print(m_tier$results$Effects)
print(m_tier$results$Placebos)
print(m_tier$results$ATE)
