# =============================================================================
# 04_build_un_votes.R   (etape prealable — variables de traitement, bras VOTES ONU)
# -----------------------------------------------------------------------------
# Pendant "expressif" du bras sanctions (03) : code la position de chaque Etat
# membre de l'ONU sur les DEUX resolutions qui structurent le design, derive la
# binaire de condamnation, valide contre les decomptes officiels, et ecrit une
# table MONADIQUE propre (une ligne par pays). Aucune estimation, aucune figure.
#   * Res. 68/262 (27 mars 2014) — integrite territoriale de l'Ukraine (Crimee)
#   * ES-11/1     ( 2 mars 2022) — condamnation de l'invasion de l'Ukraine
#
# Entrees : Data/Raw/UN_votes/un_votes_raw.csv  (artefact humain : pays x vote).
# Output  : Data/Clean/un_votes.parquet  (PATH_UN_VOTES) — lu par 05 (2x2,
#           covariables) et 07 (balance des condamneurs).
# Chemins / wrappers I/O / helpers : 00_setup.R.
#
# Sources nominales officielles (recorded votes, UN Digital Library) :
#   Res 68/262 : https://digitallibrary.un.org/record/767565
#   ES-11/1    : https://digitallibrary.un.org/record/3959039
#
# NB design : la RUSSIE est la CIBLE du choc, pas un "partenaire condamnateur".
# Sa ligne est conservee (vote_2014 = vote_2022 = "no") mais elle sera traitee a
# part en aval (jamais comptee comme partenaire qui condamne) — cf. 05.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "countrycode", "stringi")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(data.table); library(arrow)
  library(countrycode); library(stringi)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})  # fournit PATH_*, PATH_UN_VOTES, wrappers I/O, log_step

log_step("Setup termine.")

VOTE_LEVELS <- c("yes", "no", "abstain", "absent")
PATH_RAW_VOTES <- file.path(PATH_RAW, "UN_votes", "un_votes_raw.csv")


# ---- Section 1 : lecture du brut --------------------------------------------

log_step("Section 1 : lecture un_votes_raw.csv.")
stopifnot(file.exists(PATH_RAW_VOTES))
raw <- fread(PATH_RAW_VOTES, encoding = "UTF-8",
             colClasses = c(country_name = "character", iso3 = "character",
                            vote_2014 = "character", vote_2022 = "character"))
cat("  - Lignes brutes :", nrow(raw), "| colonnes :",
    paste(names(raw), collapse = ", "), "\n")

# Hygiene : trim + lower-case des positions de vote
for (v in c("vote_2014", "vote_2022"))
  raw[, (v) := tolower(trimws(get(v)))]


# ---- Section 2 : harmonisation ISO3 (countrycode + overrides) ---------------

log_step("Section 2 : harmonisation ISO3 (vocabulaire master panel).")

# Overrides manuels pour les libelles que countrycode rate ou que l'on veut
# forcer sur le vocabulaire du master panel (BACI/CEPII). A etendre si besoin.
iso_overrides <- c(
  "Micronesia" = "FSM"   # Federated States of Micronesia (countrycode -> NA)
)

iso_cc <- countrycode(raw$country_name, origin = "country.name",
                      destination = "iso3c", custom_match = iso_overrides,
                      warn = FALSE)

# Logue tout libelle non resolu (ne PAS laisser passer en NA silencieux)
unresolved <- raw$country_name[is.na(iso_cc)]
if (length(unresolved)) {
  cat("  !! Libelles NON resolus en ISO3 (a corriger via iso_overrides) :\n")
  cat("     ", paste(unresolved, collapse = " | "), "\n")
}
stopifnot(length(unresolved) == 0L)

# Croise avec l'ISO3 saisi a la main (garde-fou : signaler les divergences)
mism <- which(!is.na(raw$iso3) & raw$iso3 != "" & raw$iso3 != iso_cc)
if (length(mism)) {
  cat("  !! Divergence iso3 saisi vs countrycode (countrycode fait foi) :\n")
  for (i in mism) cat(sprintf("     %-28s saisi=%s  cc=%s\n",
                              raw$country_name[i], raw$iso3[i], iso_cc[i]))
}
raw[, iso3 := iso_cc]   # countrycode fait foi (aligne master panel)

stopifnot(!any(duplicated(raw$iso3)))


# ---- Section 3 : derivation des variables -----------------------------------

log_step("Section 3 : derivation condemn_* / align_2022.")

votes <- raw[, .(
  country_name,
  iso3,
  un_member = TRUE,   # les 193 lignes sont des membres de l'AGNU au vote
  vote_2014 = factor(vote_2014, levels = VOTE_LEVELS),
  vote_2022 = factor(vote_2022, levels = VOTE_LEVELS)
)]

# Toute position hors {yes,no,abstain,absent} -> NA de facteur (a attraper)
stopifnot(!anyNA(votes$vote_2014), !anyNA(votes$vote_2022))

# Binaire de condamnation (mesure principale, feuille de route) : 1 ssi "yes".
votes[, condemn_2014 := as.integer(vote_2014 == "yes")]
votes[, condemn_2022 := as.integer(vote_2022 == "yes")]

# Score ordinal optionnel (robustesses) : yes=2, abstain=1, no/absent=0.
# Teste l'abstention comme condamnation "partielle". EN PLUS du binaire.
votes[, align_2022 := fcase(vote_2022 == "yes", 2L,
                            vote_2022 == "abstain", 1L,
                            default = 0L)]


# ---- Section 4 : validation (garde-fous durs) -------------------------------

log_step("Section 4 : validation vs decomptes officiels.")

t14 <- table(votes$vote_2014); t22 <- table(votes$vote_2022)
cat("  Res 68/262 (2014) :", paste(names(t14), as.integer(t14), sep = "=", collapse = "  "), "\n")
cat("  ES-11/1    (2022) :", paste(names(t22), as.integer(t22), sep = "=", collapse = "  "), "\n")

# Totaux officiels (193 membres). Echec BRUYANT si la saisie ne reproduit pas.
stopifnot(nrow(votes) == 193L)
stopifnot(t14["yes"] == 100L, t14["no"] == 11L,
          t14["abstain"] == 58L, t14["absent"] == 24L)
stopifnot(t22["yes"] == 141L, t22["no"] == 5L,
          t22["abstain"] == 35L, t22["absent"] == 12L)

# Spot-checks nominaux fiables.
no_2022 <- sort(votes[vote_2022 == "no", iso3])
stopifnot(identical(no_2022, sort(c("BLR", "PRK", "ERI", "RUS", "SYR"))))
stopifnot(votes[iso3 == "RUS", vote_2014 == "no" & vote_2022 == "no"])
cat("  Spot-checks (5 'contre' 2022 ; Russie no/no) : OK\n")


# ---- Section 5 : alignement au master panel (anti-join, deux sens) ----------

log_step("Section 5 : anti-join au master panel (non corrige en silence).")

mp <- read_parquet_safe(PATH_MASTER, col_select = c("exp_iso3", "imp_iso3"))
master_iso3 <- sort(unique(c(mp$exp_iso3, mp$imp_iso3)))

votes_not_in_master <- sort(setdiff(votes$iso3, master_iso3))
master_not_voting   <- sort(setdiff(master_iso3, votes$iso3))

cat("  - Pays qui votent a l'ONU MAIS absents du master panel (",
    length(votes_not_in_master), ") :\n     ",
    if (length(votes_not_in_master)) paste(votes_not_in_master, collapse = " ") else "(aucun)", "\n")
cat("  - Pays du master panel SANS ligne de vote (",
    length(master_not_voting), ", non-membres ONU / territoires attendus) :\n     ",
    paste(master_not_voting, collapse = " "), "\n")
# Aucun pays-ONU ne devrait manquer cote master (BACI couvre tous les membres).
if (length(votes_not_in_master))
  cat("  !! A TRANCHER : des membres ONU ne matchent pas le master panel.\n")


# ---- Section 6 : sauvegarde -------------------------------------------------

log_step("Section 6 : sauvegarde.")

setorder(votes, country_name)
setcolorder(votes, c("country_name", "iso3", "un_member",
                     "vote_2014", "vote_2022",
                     "condemn_2014", "condemn_2022", "align_2022"))
write_parquet_safe(votes, PATH_UN_VOTES)
cat("  - Ecrit :", PATH_UN_VOTES, "\n")
cat("  -", nrow(votes), "lignes,", ncol(votes), "colonnes\n")
cat(sprintf("  - Condamneurs : 2014 = %d | 2022 = %d (sur 193 membres)\n",
            sum(votes$condemn_2014), sum(votes$condemn_2022)))

log_step("Termine.")
