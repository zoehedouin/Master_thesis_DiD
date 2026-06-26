# =============================================================================
# 05_build_covariates.R   (etape prealable — covariables du design)
# -----------------------------------------------------------------------------
# Construit les covariables de conditionnement/balance de la feuille de route
# (section "Covariables") et les quatre cellules du 2x2 condamne x sanctionne :
#   * dependance energetique russe  : part des hydrocarbures russes (HS27) dans
#     le commerce du pays — version BACI (defendable pour un memoire). C'EST la
#     covariable de conditionnement qui compte (varie dans le temps, non absorbee
#     par les FE paire).
#   * exposition commerciale pre-2014 a la Russie : part de la Russie dans le
#     commerce total du pays, moyennee 2008-2013.
#   * 2x2 condamne x sanctionne : croisement votes ONU (04) x sanctions (03).
#
# Output : Data/Clean/covariates.parquet (+ .csv) — pays/dyade x annee
# Entrees : BACI HS6 (Data/Raw/BACI_HS92_V202601) pour HS27 et parts Russie ;
#           iv_panel.parquet (sanctions) ; un_votes.parquet (condamne, cf. 04).
# Chemins / wrappers I/O / helpers : 00_setup.R.
#
# NOTE design (feuille de route) : OTAN/UE = variable DESCRIPTIVE de balance,
# PAS une covariable de conditionnement (bad control quasi colineaire au
# traitement, deja absorbe par les FE paire). On la garde pour documenter le
# sorting (cf. 07_validity.R), jamais comme controle.
# =============================================================================

# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

source("00_setup.R")  # PATH_BACI, PATH_CLEAN, PATH_IV_PANEL, wrappers, out_*

log_step("05_build_covariates : setup OK.")

PATH_VOTES <- file.path(PATH_CLEAN, "un_votes.parquet")  # produit par 04

# Frame de reference : panel de traitement (sanctions deja construites en 03).
panel <- read_parquet_safe(PATH_IV_PANEL)
log_step(sprintf("iv_panel : %d lignes, %d colonnes.", nrow(panel), ncol(panel)))


## TODO (feuille de route — Etape prealable "Covariables" + 2x2)
## ---------------------------------------------------------------------------
## A. DEPENDANCE ENERGETIQUE RUSSE (version BACI) :
##    - Pour chaque (pays p, annee t) : part des imports d'hydrocarbures russes
##      dans le commerce total du pays. Filtrer BACI sur HS2 == "27"
##      (combustibles mineraux), exportateur = RUS, importateur = p.
##      energy_dep[p,t] = sum(trade HS27, RUS->p) / sum(trade total, *->p).
##    - C'est la covariable de conditionnement clef (varie dans le temps
##      differemment selon les pays -> non absorbee par les FE paire).
##    - Boucler sur les fichiers annuels BACI_HS92_Y####_V202601.csv
##      (fread + filtre k commencant par "27"), pas de chargement global.
##
## B. EXPOSITION COMMERCIALE PRE-2014 A LA RUSSIE :
##    - part de la Russie dans le commerce total de p, moyennee sur 2008-2013 :
##      exposure_pre2014[p] = mean_{t in 2008..2013} (
##         (trade RUS<->p, somme deux sens) / (trade total de p) ).
##    - Variable invariante (un scalaire par pays) -> sert a la BALANCE/SMD et
##      a restreindre/ponderer les controles, pas comme controle time-varying.
##
## C. 2x2 CONDAMNE x SANCTIONNE (quatre cellules) :
##    votes <- read_parquet_safe(PATH_VOTES)            # cf. 04, colonne `condamne`
##    - sanctionne[p,t] : derive de iv_panel (sanction_any / sanction_nontrade
##      cote partenaire vs Russie ; choisir la def alignee sur 08_ppml.R).
##    - cell := fcase(
##        condamne==1 & sanctionne==1, "a_both",        # coalition occidentale
##        condamne==1 & sanctionne==0, "b_condemn_only",# canal expressif (coeur)
##        condamne==0 & sanctionne==1, "c_sanction_only",# quasi vide
##        default,                     "d_neither")      # Chine, Inde, abstenus
##    - GARDER LES QUATRE cellules (c'est la comparaison inter-cellules qui
##      identifie, cf. feuille de route ; (d) est le contrefactuel de (b)).
##
## D. ECRIRE :
##    write_parquet_safe(cov, file.path(PATH_CLEAN, "covariates.parquet"))
##    fwrite(cov,            file.path(PATH_CLEAN, "covariates.csv"))
##    -> consomme par 07_validity.R (balance/SMD), 08_ppml.R (2x2, conditionnement
##       energie), 09_dcdh.R (eventuel conditionnement).
##
## Packages : data.table, arrow.
## NE RIEN INVENTER : squelette uniquement (lectures reelles + TODO).

if (!file.exists(PATH_VOTES)) {
  log_step(paste0("NOTE : ", PATH_VOTES, " absent — lancer 04_build_un_votes.R ",
                  "avant la cellule C (2x2). Etapes A/B (BACI) implementables sans."))
}
log_step("05_build_covariates : squelette charge. Implementer le bloc TODO.")
