# =============================================================================
# 07_validity.R   (etape 2 — validite du DiD : balance + tendances paralleles)
# -----------------------------------------------------------------------------
# Decide si le design est credible AVANT toute estimation d'effet. Trois volets
# (feuille de route §2) :
#   (i)   BALANCE DES COVARIABLES (SMD) entre traites/controles
#         (sanctionneurs vs non ; condamneurs vs non), surtout exposition
#         pre-2014 et dependance energetique. OTAN/UE = DESCRIPTIF (sorting),
#         pas un controle.
#   (ii)  TENDANCES PARALLELES (pre-tendances) : leads/placebos plats et proches
#         de zero aux temps relatifs negatifs ; absence d'anticipation.
#   (iii) HonestDiD (Rambachan & Roth 2023) : borner la violation possible des
#         tendances paralleles et montrer la survie de l'effet.
#
# Output : Output/Tables/Validity/ , Output/Figures/Validity/
# Entrees : master_panel_with_strategic.parquet, iv_panel.parquet,
#           covariates.parquet (cf. 05). Coefs d'event study repris de 08_ppml.R.
# Chemins / wrappers I/O / helpers : 00_setup.R.
# =============================================================================

# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "ggplot2", "HonestDiD")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) message("Packages manquants (a installer) : ",
                          paste(miss, collapse = ", "))

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
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})  # PATH_*, wrappers, out_tab/out_fig, log_step/tic/toc
PART <- "07_validity"   # co-localisation des sorties de cette partie (out_*)

PATH_TAB <- out_tab("Validity")
PATH_FIG <- out_fig("Validity")
PATH_COV <- file.path(PATH_CLEAN, "covariates.parquet")  # produit par 05

log_step("07_validity : setup OK.")

df <- read_parquet_safe(PATH_STRATEGIC)
log_step(sprintf("panel : %d lignes.", nrow(df)))


## TODO (feuille de route §2 — Validite du DiD)
## ---------------------------------------------------------------------------
## (i) BALANCE / SMD (le vrai talon d'Achille : le sorting) :
##     - Definir traites/controles selon DEUX partitions :
##         * sanctionneurs vs non-sanctionneurs (vs la Russie) ;
##         * condamneurs ONU vs non (cf. un_votes / covariates).
##     - Calculer l'ecart standardise (Standardized Mean Difference) sur les
##       covariables, en priorite : exposition pre-2014, dependance energetique,
##       distance, regime (polyarchy), PIB/pop, region.
##         smd = (mean_treated - mean_control) / sqrt((var_t + var_c)/2)
##     - AFFICHER OTAN/UE dans la table de balance pour DOCUMENTER le sorting
##       (sanctionneurs ~ membres UE/OTAN), mais NE PAS s'en servir comme
##       covariable de conditionnement (bad control colineaire au traitement).
##     - Sortie : Output/Tables/Validity/tab_balance_smd.{csv,tex}.
##
## (ii) PRE-TENDANCES (tendances paralleles) :
##     - Reprendre l'event study Sun & Abraham de 08_ppml.R ; lire les leads
##       (k = -2,-3,-4,...) : doivent etre plats et ~0. C'est le juge de paix
##       n°1, lu AVANT le signe de l'effet post-traitement.
##     - Tester l'anticipation (pas de reaction avant le choc : accumulation
##       militaire fin 2021 avant 2022).
##     - Sortie : Output/Figures/Validity/fig_pretrends.png + table des leads.
##
## (iii) HonestDiD (Rambachan & Roth 2023) :
##     - A partir de l'objet event-study (fixest -> coefs + vcov), utiliser
##       HonestDiD::createSensitivityResults_relativeMagnitudes() (et/ou la
##       restriction "smoothness" Delta^SD) pour borner la violation et montrer
##       que l'effet survit tant que la tendance differentielle <= Mbar fois la
##       pre-tendance observee.
##     - Sortie : Output/Figures/Validity/fig_honestdid_sensitivity.png +
##       Output/Tables/Validity/tab_honestdid_bounds.csv.
##
## Packages : HonestDiD (install : install.packages("HonestDiD")), fixest,
##            ggplot2, data.table, arrow.
## NE RIEN INVENTER : squelette uniquement.

log_step("07_validity : squelette charge. Implementer les volets (i)-(iii).")
