# =============================================================================
# 04_build_un_votes.R   (etape preliminaire — collecte externe, PRIORITE N°1)
# -----------------------------------------------------------------------------
# Code A LA MAIN la position de chaque pays sur les DEUX resolutions de
# l'Assemblee generale de l'ONU qui structurent le bras "votes" du design :
#   * ES-11/1   (2 mars 2022)   — condamnation de l'invasion de l'Ukraine
#   * Res. 68/262 (27 mars 2014) — integrite territoriale (Crimee)
# Pour chaque pays x resolution : pour / contre / abstention / absent.
# Derive la binaire `condamne` (1 si "pour"), puis fusionne sur les dyades
# centrees Russie (Russie<->partenaire).
#
# Source des votes : UN Digital Library, votes nominaux (roll-call).
#   ES-11/1   : https://digitallibrary.un.org/record/3965290  (A/RES/ES-11/1)
#   Res 68/262: https://digitallibrary.un.org/record/767565   (A/RES/68/262)
# Petit fichier : ~193 pays x 2 votes -> a saisir dans un CSV puis charger ici.
#
# Output : Data/Clean/un_votes.parquet (+ .csv)  — table pays x resolution + condamne
# Entrees : iv_panel.parquet (pour la liste ISO3 et le cadrage dyadique Russie)
# Chemins / wrappers I/O / helpers : 00_setup.R.
# =============================================================================

# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(countrycode)
})

source("00_setup.R")  # PATH_CLEAN, PATH_IV_PANEL, wrappers I/O, log_step, out_*

log_step("04_build_un_votes : setup OK.")

# Fichier de saisie manuelle attendu (a creer si absent). Une ligne par pays,
# colonnes : iso3, vote_2022, vote_2014 dans {for, against, abstain, absent}.
PATH_VOTES_RAW <- file.path(PATH_RAW, "UN_votes", "un_votes_roll_call.csv")

# Frame de reference : ISO3 presents dans le panel de traitement.
panel <- read_parquet_safe(PATH_IV_PANEL,
                           col_select = c("exp_iso3", "imp_iso3", "year"))
iso3_panel <- sort(unique(c(panel$exp_iso3, panel$imp_iso3)))
log_step(sprintf("ISO3 dans le panel : %d.", length(iso3_panel)))


## TODO (feuille de route — Etape preliminaire, "Votes ONU : la donnee nouvelle")
## ---------------------------------------------------------------------------
## 1. SAISIR les votes (collecte externe, a la main) :
##    - Telecharger les roll-calls ES-11/1 (2022) et Res. 68/262 (2014) depuis
##      la UN Digital Library (liens en en-tete).
##    - Creer Data/Raw/UN_votes/un_votes_roll_call.csv avec colonnes :
##        iso3, country, vote_2022, vote_2014
##      ou vote_* in {"for","against","abstain","absent"}.
##      Mapper les noms de pays -> ISO3 via countrycode() ; verifier les cas
##      delicats (Russie = RUS exclue du cote "partenaire", ex-URSS, micro-Etats).
##
## 2. CHARGER + VALIDER :
##    votes <- read_excel_safe()/fread(PATH_VOTES_RAW)
##    - Verifier couverture vs iso3_panel (lister les manquants, justifier).
##    - Verifier les totaux contre le compte officiel (ES-11/1 : 141 pour,
##      5 contre, 35 abstentions ; 68/262 : 100 pour, 11 contre, 58 abstentions)
##      -> garde-fou de saisie, NE PAS coder ces totaux en dur comme resultat.
##
## 3. DERIVER la binaire de traitement-vote :
##    votes[, condamne_2022 := as.integer(vote_2022 == "for")]
##    votes[, condamne_2014 := as.integer(vote_2014 == "for")]
##    votes[, condamne      := condamne_2022]   # bras principal (choc 2022)
##    (garder aussi le codage 2014 pour le volet Crimee.)
##
## 4. FUSIONNER sur les dyades Russie : pour chaque partenaire p, joindre
##    condamne(p) aux dyades RUS<->p de iv_panel (le statut vote est porte par
##    le PARTENAIRE, la Russie est la cible commune).
##
## 5. ECRIRE :
##    write_parquet_safe(votes, file.path(PATH_CLEAN, "un_votes.parquet"))
##    fwrite(votes,            file.path(PATH_CLEAN, "un_votes.csv"))
##    -> consomme par 05_build_covariates.R (cellule "condamne" du 2x2) et par
##       08_ppml.R (bras vote + 2x2 en interaction).
##
## Packages : data.table, arrow, countrycode.
## NE RIEN INVENTER : tant que le CSV de saisie n'existe pas, ce script s'arrete
## proprement ci-dessous.

if (!file.exists(PATH_VOTES_RAW)) {
  log_step(paste0("STOP : fichier de votes a saisir absent (", PATH_VOTES_RAW,
                  "). Voir le bloc TODO ci-dessus."))
} else {
  log_step("Fichier de votes detecte — implementer les etapes 2-5 du TODO.")
}
