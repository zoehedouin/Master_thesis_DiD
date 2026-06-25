# =============================================================================
# 06_build_geopol_measures.R
# -----------------------------------------------------------------------------
# Construit un panel de "mesures geopolitiques alternatives" (regroupant ce
# qui etait initialement des "instruments alternatifs", + les sanctions GSDB).
# Lit Data/Raw/IV/, harmonise en ISO3, ecrit Data/Clean/iv_panel.parquet
# (nom conserve pour compatibilite avec scripts 07/07b/07c).
#
# Famille "institutional"      (sources : V-Dem v16, DPI 2023, Polity5)
#   - polyarchy_dist           = |v2x_polyarchy_i - v2x_polyarchy_j|
#   - joint_dem_vdem           = min(v2x_polyarchy_i, v2x_polyarchy_j)
#   - ideol_dist               = |execrlc_i - execrlc_j| (DPI, recode 1-3)
#   - polity_dist              = |polity2_i - polity2_j|  (Polity5, robustness)
#
# Famille "strategic_relations" (sources : ATOP v5.1, dyadic_mid v4.03)
#   - allied_atop              = 1 si alliance active i-j (instrument)
#   - shared_ally_atop         = nb. de pays k allies a i ET a j
#   - shared_rival_mid         = nb. de pays k en MID actif avec i ET avec j
#   - mid_direct               = 1 si i et j ont un MID direct (CONTROL)
#
# Famille "sanctions"           (source : GSDB v4 dyadic, 1950-2023)
#   - sanction_any             = 1 si sanction active entre i et j (undirected,
#                                 tous types confondus)
#   - sanction_trade           = 1 si sanction de TYPE trade en vigueur
#                                 (! tautologique en equation de gravity car
#                                 elle coupe le commerce mecaniquement)
#   - sanction_nontrade        = 1 si sanction non-trade (financial, travel,
#                                 arms, military, other). Mesure "propre"
#                                 d'hostilite geopolitique, recommandee.
#   - n_common_sanctioners     = nb. de pays tiers k sanctionnant a la fois
#                                 i ET j la meme annee t
#
# Coverage temporelle effective :
#   - institutional family     : 1995-2023 (DPI = binding) ; Polity 1995-2018
#   - strategic_relations      : 1995-2014 (MID = binding) ; ATOP 1995-2018
#   - sanctions                : 1995-2023 (GSDB v4)
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "readxl", "countrycode", "stringi", "haven")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(readxl)
  library(countrycode); library(stringi)
})

PATH_ROOT  <- "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis"
PATH_RAW   <- file.path(PATH_ROOT, "Data", "Raw", "IV")
PATH_CLEAN <- file.path(PATH_ROOT, "Data", "Clean")
PATH_PANEL <- file.path(PATH_CLEAN, "master_panel_with_strategic.parquet")

# ---- I/O robuste au chemin accentue NFD (OneDrive) -------------------------
# Les lecteurs C++ (arrow / haven / readxl) n'ouvrent pas un chemin contenant
# un 'e' accentue normalise NFD. On copie d'abord vers un tempfile ASCII (la
# copie de base R gere le NFD), puis on lit/ecrit. fread/fwrite/ggsave de base
# fonctionnent directement et ne sont pas wrappes.
read_parquet_safe <- function(path, ...) {
  tmp <- tempfile(fileext = ".parquet")
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(arrow::read_parquet(tmp, ...))
}
write_parquet_safe <- function(x, path, ...) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(x, tmp, ...)
  stopifnot(file.copy(tmp, path, overwrite = TRUE)); unlink(tmp)
}
read_dta_safe <- function(path, ...) {
  tmp <- tempfile(fileext = ".dta")
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(haven::read_dta(tmp, ...))
}
read_excel_safe <- function(path, ...) {
  ext <- paste0(".", tools::file_ext(path))
  tmp <- tempfile(fileext = ext)
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(readxl::read_excel(tmp, ...))
}

log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

# Bornes temporelles du panel master
YEAR_MIN <- 1995L
YEAR_MAX <- 2024L

log_step("Setup termine.")


# ---- Section 1 : ISO3 du panel master (frame de reference) ------------------

log_step("Section 1 : ISO3 panel master + colonnes a importer.")
panel <- read_parquet_safe(PATH_PANEL,
                       col_select = c("exp_iso3", "imp_iso3", "year",
                                      "trade_value", "ipd", "rta",
                                      "dist", "contig",
                                      "comlang_off", "colony"))
master_iso3 <- sort(unique(c(panel$exp_iso3, panel$imp_iso3)))
cat("  - Pays uniques panel master :", length(master_iso3), "\n")

# Helper : convertir COW -> ISO3 avec custom_match pour cas frequents post-1991
cow_to_iso3 <- function(cow) {
  custom <- c("260" = "DEU", "265" = "DEU",     # RFA / RDA -> DEU
              "678" = "YEM",                      # Yemen Arab Republic
              "680" = "YEM",                      # Yemen PDR
              "315" = "CZE",                      # Czechoslovakia
              "345" = "SRB",                      # Yugoslavia / FRY / Serbia
              "347" = "SRB",                      # Kosovo (mapped to SRB best effort)
              "364" = "RUS",                      # USSR
              "365" = "RUS",                      # USSR alt
              "625" = "SDN",                      # Sudan pre-2011 split
              "626" = "SSD",                      # South Sudan
              "713" = "TWN")                      # Taiwan
  iso <- countrycode(cow, origin = "cown", destination = "iso3c",
                     custom_match = custom, warn = FALSE)
  iso
}

# Helper : country name -> ISO3
name_to_iso3 <- function(nm) {
  countrycode(nm, origin = "country.name", destination = "iso3c",
              warn = FALSE)
}


# ---- Section 2 : V-Dem v16 (institutional) ----------------------------------

log_step("Section 2 : V-Dem.")
tic()
vdem <- fread(file.path(PATH_RAW, "V-Dem", "V-Dem-CY-Full+Others-v16.csv"),
              select = c("country_text_id", "country_name", "year",
                         "v2x_polyarchy"))
cat("  - V-Dem lignes brutes :", nrow(vdem), "(", toc(), "s)\n")

vdem <- vdem[year >= YEAR_MIN - 1L & year <= YEAR_MAX]
vdem[, iso3 := country_text_id]
# country_text_id deja en ISO3 dans V-Dem - verification rapide
n_bad <- vdem[!iso3 %in% master_iso3, uniqueN(country_text_id)]
cat("  - V-Dem codes non-mappes :", n_bad, "\n")
vdem <- vdem[!is.na(v2x_polyarchy),
             .(iso3, year, v2x_polyarchy)]
# Dedup (iso3, year) : moyenne en cas d'historical entities pour meme code
vdem <- vdem[, .(v2x_polyarchy = mean(v2x_polyarchy)), by = .(iso3, year)]
cat("  - V-Dem obs filtrees    :", nrow(vdem),
    "| annees :", min(vdem$year), "-", max(vdem$year),
    "| pays :", uniqueN(vdem$iso3), "\n")


# ---- Section 3 : DPI 2023 (institutional - ideologie executif) -------------

log_step("Section 3 : DPI execrlc.")
tic()
dpi_path <- file.path(PATH_RAW, "the-database-of-political-institutions",
                     "DPI 2023 (CSV Version)", "DPI 2023 CSV Version.csv")
# fread gere BOM via encoding=UTF-8
dpi <- fread(dpi_path, encoding = "UTF-8",
             select = c(1, 2, 3, 15))  # countryname, ifs, year, execrlc
setnames(dpi, c("countryname", "ifs", "year_raw", "execrlc"))
cat("  - DPI lignes brutes :", nrow(dpi), "(", toc(), "s)\n")

# year : IDate dans v2023 -> extraction annee
dpi[, year := as.integer(format(year_raw, "%Y"))]
dpi <- dpi[year >= YEAR_MIN & year <= YEAR_MAX]

# execrlc est CHARACTER dans DPI 2023 :
#   "Right" / "Center" / "Left"      = valeurs ordinales
#   "0"                              = pas de parti / militaire
#   "-999" / "NA"                    = missing
# Recodage ordinal 1-3
dpi[, execrlc := fcase(
  execrlc == "Right",  1L,
  execrlc == "Center", 2L,
  execrlc == "Left",   3L,
  default = NA_integer_
)]

# DPI : ifs est ~ISO3 (codes IMF) mais quelques exceptions
dpi[, iso3 := ifs]
# Cas connus de divergence IFS vs ISO3
dpi[ifs == "ROM",            iso3 := "ROU"]   # Roumanie
dpi[ifs == "TMP" | ifs == "TLS", iso3 := "TLS"]
dpi[ifs == "WBG",            iso3 := "PSE"]
dpi[ifs == "ZAR",            iso3 := "COD"]
dpi[ifs == "GER",            iso3 := "DEU"]
dpi[ifs == "RSS" | ifs == "USR", iso3 := "RUS"]

# Si ifs ne mappe pas, fallback sur countryname
dpi[!iso3 %in% master_iso3, iso3 := name_to_iso3(countryname)]
n_bad <- dpi[is.na(iso3) | !iso3 %in% master_iso3, uniqueN(countryname)]
cat("  - DPI pays non-mappes :", n_bad, "\n")

dpi <- dpi[!is.na(execrlc) & !is.na(iso3) & iso3 %in% master_iso3,
           .(iso3, year, execrlc)]
dpi <- dpi[, .(execrlc = first(execrlc)), by = .(iso3, year)]
cat("  - DPI obs filtrees :", nrow(dpi),
    "| annees :", min(dpi$year), "-", max(dpi$year),
    "| pays :", uniqueN(dpi$iso3), "\n")


# ---- Section 4 : Polity5 (robustness institutional) ------------------------

log_step("Section 4 : Polity5 polity2.")
tic()
polity <- read_excel_safe(file.path(PATH_RAW, "Polity5.xls"),
                          sheet = "p5v2018")
cat("  - Polity5 lignes brutes :", nrow(polity), "(", toc(), "s)\n")

polity <- polity[year >= YEAR_MIN & year <= YEAR_MAX]
# polity2 : -10 a +10 ; flag != 0 indique transition/foreign occupation -> NA
polity[flag != 0 | is.na(polity2), polity2 := NA_real_]
polity[, polity2 := as.numeric(polity2)]

# ccode = COW
polity[, iso3 := cow_to_iso3(ccode)]
n_bad <- polity[is.na(iso3) | !iso3 %in% master_iso3, uniqueN(ccode)]
cat("  - Polity codes COW non-mappes :", n_bad, "\n")

polity <- polity[!is.na(polity2) & !is.na(iso3) & iso3 %in% master_iso3,
                 .(iso3, year, polity2)]
# Dedup en cas de doublons (transitions intra-annee)
polity <- polity[, .(polity2 = mean(polity2)), by = .(iso3, year)]
cat("  - Polity obs filtrees :", nrow(polity),
    "| annees :", min(polity$year), "-", max(polity$year),
    "| pays :", uniqueN(polity$iso3), "\n")


# ---- Section 5 : ATOP - alliances (strategic_relations) --------------------

log_step("Section 5 : ATOP alliances.")
tic()
atop <- fread(file.path(PATH_RAW, "ATOP", "atop5_1ddyr.csv"),
              select = c("ddyad", "year", "atopally", "defense", "offense",
                         "nonagg", "stateA", "stateB"))
cat("  - ATOP lignes brutes :", nrow(atop), "(", toc(), "s)\n")

atop <- atop[year >= YEAR_MIN & year <= YEAR_MAX]
# stateA, stateB sont des codes COW
atop[, iso_a := cow_to_iso3(stateA)]
atop[, iso_b := cow_to_iso3(stateB)]
n_bad <- atop[(is.na(iso_a) | is.na(iso_b)),
              uniqueN(c(stateA, stateB))]
cat("  - ATOP codes COW non-mappes :", n_bad, "\n")

atop <- atop[!is.na(iso_a) & !is.na(iso_b) &
             iso_a %in% master_iso3 & iso_b %in% master_iso3]

# allied (instrument direct)
allied_atop <- atop[atopally == 1L, .(iso_a, iso_b, year, allied_atop = 1L)]
cat("  - Allied dyad-years :", nrow(allied_atop),
    "| annees :", min(allied_atop$year), "-", max(allied_atop$year), "\n")

# Construction shared_ally : nombre de pays k tels que i-k et j-k allies
# Pour chaque (k, year), liste des allies de k
allies_of <- atop[atopally == 1L, .(iso_a, iso_b, year)]
# Symetrisation : alliance is undirected. ATOP ddyad est deja directional
# (deux lignes par paire). On dedoublonne en undirected pair pour faire la
# liste d'allies.
allies_und <- unique(rbind(
  allies_of[, .(focal = iso_a, ally = iso_b, year)],
  allies_of[, .(focal = iso_b, ally = iso_a, year)]
))
cat("  - Allies (focal, ally, year) dedoublonnes :", nrow(allies_und), "\n")


# ---- Section 6 : dyadic_mid (strategic_relations) ---------------------------

log_step("Section 6 : dyadic MID.")
tic()
mid <- fread(file.path(PATH_RAW, "dyadic_mid", "dyadic_mid_4.03.csv"),
             select = c("disno", "statea", "stateb", "year", "strtyr", "endyear"))
cat("  - MID lignes brutes :", nrow(mid), "(", toc(), "s)\n")

# Expansion strtyr-endyear (un MID-dyad peut s'etendre sur plusieurs annees)
# data.table : seq.int doit etre appele sur scalaires, donc row_id-by-row
mid[, row_id := .I]
mid_expanded <- mid[, .(year = seq.int(strtyr, pmin(endyear, YEAR_MAX))),
                    by = row_id]
mid_expanded <- merge(mid_expanded,
                      mid[, .(row_id, statea, stateb, disno)],
                      by = "row_id")
mid_expanded <- mid_expanded[year >= YEAR_MIN & year <= YEAR_MAX]
mid_expanded[, iso_a := cow_to_iso3(statea)]
mid_expanded[, iso_b := cow_to_iso3(stateb)]
mid_expanded <- mid_expanded[!is.na(iso_a) & !is.na(iso_b) &
                              iso_a %in% master_iso3 &
                              iso_b %in% master_iso3]
cat("  - MID expanded (annees actives) :", nrow(mid_expanded),
    "| annees :", min(mid_expanded$year), "-", max(mid_expanded$year), "\n")

# MID direct (control) : 0/1 par paire-annee, undirected
mid_direct <- unique(rbind(
  mid_expanded[, .(iso_a, iso_b, year)],
  mid_expanded[, .(iso_a = iso_b, iso_b = iso_a, year)]
))[, mid_direct := 1L]
cat("  - MID direct (dyad-years undirected) :", nrow(mid_direct), "\n")

# Liste des adversaires de chaque pays par annee (pour shared_rival)
rivals_of <- unique(rbind(
  mid_expanded[, .(focal = iso_a, rival = iso_b, year)],
  mid_expanded[, .(focal = iso_b, rival = iso_a, year)]
))
cat("  - Rivals (focal, rival, year) dedoublonnes :", nrow(rivals_of), "\n")


# ---- Section 6bis : GSDB v4 - sanctions ------------------------------------

log_step("Section 6bis : GSDB sanctions.")
tic()
gsdb <- read_dta_safe(
  file.path(PATH_RAW, "gsdb_v4", "GSDB_V4_dyadic.dta"))
cat("  - GSDB lignes brutes :", nrow(gsdb), "(", toc(), "s)\n")

# Format dyad-year deja etendu (sender, target, year, types). ISO3 natifs.
# Filtre fenetre temporelle.
gsdb <- gsdb[year >= YEAR_MIN & year <= YEAR_MAX]

# Mappe sur master_iso3
gsdb <- gsdb[sanctioning_state_iso3 %in% master_iso3 &
             sanctioned_state_iso3  %in% master_iso3]
cat("  - GSDB filtres ISO3 master :", nrow(gsdb),
    "| annees :", min(gsdb$year), "-", max(gsdb$year), "\n")

# Types : trade, arms, military, financial, travel, other. On reconstruit :
#   sanction_trade    : la ligne touche le commerce
#   sanction_nontrade : au moins un type non-trade (financial OR travel OR arms
#                        OR military OR other)
gsdb[, type_trade    := as.integer(trade == 1L)]
gsdb[, type_nontrade := as.integer(arms == 1L | military == 1L |
                                    financial == 1L | travel == 1L |
                                    other == 1L)]

# Construction directional : pour chaque (sender, target, year) on agrege
# les flags max sur les cas qui coexistent
sanc_dir <- gsdb[, .(
  sanction_any_d      = 1L,
  sanction_trade_d    = max(type_trade,    na.rm = TRUE),
  sanction_nontrade_d = max(type_nontrade, na.rm = TRUE)),
  by = .(sender = sanctioning_state_iso3,
         target = sanctioned_state_iso3, year)]

# Construction undirected : OR sur les deux directions
sanc_und_a <- sanc_dir[, .(iso_a = sender, iso_b = target, year,
                            sanction_any_d, sanction_trade_d,
                            sanction_nontrade_d)]
sanc_und_b <- sanc_dir[, .(iso_a = target, iso_b = sender, year,
                            sanction_any_d, sanction_trade_d,
                            sanction_nontrade_d)]
sanc_und <- rbind(sanc_und_a, sanc_und_b)
sanc_und <- sanc_und[, .(
  sanction_any      = max(sanction_any_d,      na.rm = TRUE),
  sanction_trade    = max(sanction_trade_d,    na.rm = TRUE),
  sanction_nontrade = max(sanction_nontrade_d, na.rm = TRUE)),
  by = .(iso_a, iso_b, year)]
cat("  - Sanctions undirected (pair-years) :", nrow(sanc_und), "\n")

# ---- Section 6ter : indicatrices par TYPE + doses d'intensite (ADDITIF) -----
# N'altere PAS les 4 colonnes historiques ci-dessus. Memes conventions :
#   - agreger par max sur les cas coexistants par (sender, target, year) ;
#   - symetriser en empilant les deux directions et en prenant le max ;
#   - fenetre NA-apres-2023 appliquee a l'assemblage (Section 8).

# Flags par type au niveau ligne. descr_trade subdivise le commerce :
#   "exp_compl"/"imp_compl" -> complet ; "exp_part"/"imp_part" -> partiel ;
#   "" si trade == 0. (cf. codebook Var_Description.xlsx)
gsdb[, dtr := fifelse(is.na(descr_trade), "", descr_trade)]
gsdb[, `:=`(
  ft_arms           = as.integer(arms == 1L),
  ft_military       = as.integer(military == 1L),
  ft_financial      = as.integer(financial == 1L),
  ft_travel         = as.integer(travel == 1L),
  ft_other          = as.integer(other == 1L),
  ft_trade_complete = as.integer(grepl("compl", dtr)),
  ft_trade_partial  = as.integer(grepl("part",  dtr)))]

type_cols <- c("sanc_arms", "sanc_military", "sanc_financial", "sanc_travel",
               "sanc_other", "sanc_trade_complete", "sanc_trade_partial")

sanc_type_dir <- gsdb[, .(
  sanc_arms           = max(ft_arms),
  sanc_military       = max(ft_military),
  sanc_financial      = max(ft_financial),
  sanc_travel         = max(ft_travel),
  sanc_other          = max(ft_other),
  sanc_trade_complete = max(ft_trade_complete),
  sanc_trade_partial  = max(ft_trade_partial)),
  by = .(sender = sanctioning_state_iso3,
         target = sanctioned_state_iso3, year)]

st_a <- sanc_type_dir[, c(list(iso_a = sender, iso_b = target, year = year),
                          .SD), .SDcols = type_cols]
st_b <- sanc_type_dir[, c(list(iso_a = target, iso_b = sender, year = year),
                          .SD), .SDcols = type_cols]
sanc_type_und <- rbind(st_a, st_b)[, lapply(.SD, max),
                                   by = .(iso_a, iso_b, year),
                                   .SDcols = type_cols]
cat("  - Indicatrices par type (pair-years) :", nrow(sanc_type_und), "\n")

# DOSE D'INTENSITE : nb de cases de sanctions actifs par dyade-annee.
# case_id est une chaine comma-separee de cas atomiques -> on explose et on
# compte les cases distincts. C'est ce qui capte l'escalade (2014->2022) la ou
# binaire/type/onset saturent.
gsdb_dose <- gsdb[, .(case_atomic = trimws(unlist(strsplit(case_id, ",")))),
                  by = .(sender = sanctioning_state_iso3,
                         target = sanctioned_state_iso3, year)]
gsdb_dose <- gsdb_dose[nchar(case_atomic) > 0]

# Classification core/peripherique via le fichier case-level (un case = 1 ligne,
# avec ses types). "core" = commercialement pertinent (trade|financial|arms|
# military) ; on exclut les cases purement travel/other (mesures individuelles
# ciblees, nombreuses mais quasi inertes commercialement, cf. GSDB-R4 note 1).
gsdb_cases <- fread(file.path(PATH_RAW, "gsdb_v4", "GSDB_V4.csv"))
gsdb_cases[, is_core := as.integer(trade == 1L | financial == 1L |
                                   arms == 1L | military == 1L)]
core_ids <- gsdb_cases[is_core == 1L, as.character(case_id)]
gsdb_dose[, core := as.integer(case_atomic %in% core_ids)]

dose_dir <- gsdb_dose[, .(n_all  = uniqueN(case_atomic),
                          n_core = uniqueN(case_atomic[core == 1L])),
                      by = .(sender, target, year)]
da <- dose_dir[, .(iso_a = sender, iso_b = target, year, n_all, n_core)]
db <- dose_dir[, .(iso_a = target, iso_b = sender, year, n_all, n_core)]
dose_und <- rbind(da, db)[, .(sanc_n_active_all  = max(n_all),
                              sanc_n_active_core = max(n_core)),
                          by = .(iso_a, iso_b, year)]
cat("  - Doses d'intensite (pair-years)     :", nrow(dose_und),
    "| max n_all :", max(dose_und$sanc_n_active_all),
    "| max n_core :", max(dose_und$sanc_n_active_core), "\n")

rm(sanc_type_dir, st_a, st_b, gsdb_dose, gsdb_cases, dose_dir, da, db)

# Common sanctioner : pour chaque (i, j, year), nb. de COALITIONS DISTINCTES
# qui sanctionnent a la fois i ET j la meme annee.
#
# DECISION DE CONSTRUCTION (cf. VARIABLES.md) :
# GSDB v4 a deux subtilites :
#   1. Les cas multilateraux (ONU, UE...) sont decomposes en une ligne par
#      Etat membre. Un embargo ONU = 190+ "senders" si on compte naivement.
#   2. case_id est une CHAINE comma-separee de cas atomiques (ex. "1057,1354"
#      = ce sender participe a 2 cases). Et un meme cas atomique peut etre
#      reutilise pour cibler differentes victimes a differentes annees.
#
# REGLE : on identifie chaque COALITION par l'ENSEMBLE (set) de ses senders
# observe dans le data. Deux cases distincts mais avec le meme set de
# senders = meme coalition. Cela capture correctement :
#   - EU sanctionne Iran (case A) et Russie (case B) -> meme coalition_id
#   - US tout seul sanctionne Iran (case C) -> coalition_id distinct (1 member)
#   - ONU sanctionne X et Y -> meme coalition_id (set commun = ~190 pays)

# Etape 1 : exploser case_id (chaine comma-separee) en cas atomiques
gsdb[, row_id := .I]
gsdb_exp <- gsdb[, .(case_atomic = trimws(unlist(strsplit(case_id, ",")))),
                  by = .(row_id, sanctioning_state_iso3,
                         sanctioned_state_iso3, year)]
gsdb_exp <- gsdb_exp[nchar(case_atomic) > 0]

# Etape 2 : pour chaque case atomique, recuperer le SET de ses senders
# (concatenes tries) -> chaque coalition identifiee par signature de
# membership.
case_set <- unique(gsdb_exp[, .(case_atomic, sanctioning_state_iso3)])
case_signature <- case_set[, .(set_str = paste(sort(sanctioning_state_iso3),
                                                collapse = "|")),
                            by = case_atomic]
case_signature[, coalition_id := paste0(
  "COAL_", as.integer(factor(set_str)))]
cat("  - Cas atomiques distincts          :",
    uniqueN(case_signature$case_atomic), "\n")
cat("  - Coalitions distinctes (signatures):",
    uniqueN(case_signature$coalition_id), "\n")

# Etape 3 : remapper cas -> coalition_id
gsdb_exp <- merge(gsdb_exp,
                  case_signature[, .(case_atomic, coalition_id)],
                  by = "case_atomic")

# Etape 4 : (coalition, target, year) dedoublonne -> set actif annuel
coal_targets <- unique(gsdb_exp[, .(coalition_id,
                                     target = sanctioned_state_iso3,
                                     year)])

# Etape 5 : self-join : pour chaque (coalition, year), produire toutes les
# paires de targets, compter par (i, j, year)
sj_a <- coal_targets[, .(coalition_id, i = target, year)]
sj_b <- coal_targets[, .(coalition_id, j = target, year)]
setkey(sj_a, coalition_id, year); setkey(sj_b, coalition_id, year)
common_snc <- merge(sj_a, sj_b, by = c("coalition_id", "year"),
                    allow.cartesian = TRUE)
common_snc <- common_snc[i != j, .N, by = .(iso_a = i, iso_b = j, year)]
setnames(common_snc, "N", "n_common_sanctioners")
cat("  - Common sanctioners (pair-years > 0) :", nrow(common_snc), "\n")
cat("  - Max n_common_sanctioners (apres regroupement coalitions) :",
    max(common_snc$n_common_sanctioners), "\n")

rm(gsdb, sanc_dir, sanc_und_a, sanc_und_b,
   gsdb_exp, case_set, case_signature, coal_targets, sj_a, sj_b)
gc(verbose = FALSE)


# ---- Section 7 : Construction des instruments dyadiques --------------------
#
# On itere annee par annee pour gerer la complexite de la jointure
# "shared third party". L'espace des paires est borne par master_iso3.

log_step("Section 7 : agregation dyadique.")

# Frame de pairs (toutes paires directionnelles) annee par annee
# On le construit pour les annees couvertes par chaque famille separement.

# Helper : compte de tiers communs (k tels que (i,k) ET (j,k) sont dans 'set')
compute_shared <- function(set_table, value_name) {
  # set_table : (focal, partner, year). On veut, pour chaque (i,j,year),
  # |{k : (i,k,year) in set AND (j,k,year) in set}|
  # Methode : self-join sur (partner, year) avec deux copies de set,
  # filtree pour i != j.
  log_step(paste("  Computing shared count :", value_name))
  tic()
  set_a <- set_table[, .(i = focal, k = partner, year)]
  set_b <- set_table[, .(j = focal, k = partner, year)]
  setkey(set_b, k, year)
  setkey(set_a, k, year)
  joined <- merge(set_a, set_b, by = c("k", "year"),
                  allow.cartesian = TRUE)
  joined <- joined[i != j]
  out <- joined[, .N, by = .(i, j, year)]
  setnames(out, c("i", "j", "N"), c("iso_a", "iso_b", value_name))
  cat("    ->", value_name, ":", nrow(out), "rows in", toc(), "s\n")
  out[]
}

# Allies & rivals : on standardise les noms de colonnes
allies_und2 <- allies_und[, .(focal, partner = ally, year)]
shared_ally <- compute_shared(allies_und2, "shared_ally_atop")

rivals_of2 <- rivals_of[, .(focal, partner = rival, year)]
shared_rival <- compute_shared(rivals_of2, "shared_rival_mid")


# ---- Section 8 : Panel par paire-annee --------------------------------------

log_step("Section 8 : assemblage panel IV (paire-annee).")

# Squelette : on part du panel master (deja en memoire), avec ses colonnes
# trade_value, ipd, rta, dist, contig, comlang_off, colony. iv_panel sera
# self-contained pour faciliter les diagnostics downstream.
panel_pairs <- copy(panel)
setnames(panel_pairs, c("exp_iso3", "imp_iso3"), c("iso_a", "iso_b"))
panel_pairs[, log_dist := log(dist)]
cat("  - Squelette pairs-years (master_panel) :", nrow(panel_pairs), "\n")

iv_panel <- copy(panel_pairs)
rm(panel, panel_pairs); gc(verbose = FALSE)

# Merger V-Dem (exp puis imp)
vdem_exp <- vdem[, .(iso_a = iso3, year, vdem_a = v2x_polyarchy)]
vdem_imp <- vdem[, .(iso_b = iso3, year, vdem_b = v2x_polyarchy)]
iv_panel <- merge(iv_panel, vdem_exp, by = c("iso_a", "year"), all.x = TRUE)
iv_panel <- merge(iv_panel, vdem_imp, by = c("iso_b", "year"), all.x = TRUE)
iv_panel[, polyarchy_dist := abs(vdem_a - vdem_b)]
iv_panel[, joint_dem_vdem := pmin(vdem_a, vdem_b)]
iv_panel[, c("vdem_a", "vdem_b") := NULL]

# DPI execrlc
dpi_exp <- dpi[, .(iso_a = iso3, year, dpi_a = execrlc)]
dpi_imp <- dpi[, .(iso_b = iso3, year, dpi_b = execrlc)]
iv_panel <- merge(iv_panel, dpi_exp, by = c("iso_a", "year"), all.x = TRUE)
iv_panel <- merge(iv_panel, dpi_imp, by = c("iso_b", "year"), all.x = TRUE)
iv_panel[, ideol_dist := abs(dpi_a - dpi_b)]
iv_panel[, c("dpi_a", "dpi_b") := NULL]

# Polity (robustness)
pol_exp <- polity[, .(iso_a = iso3, year, pol_a = polity2)]
pol_imp <- polity[, .(iso_b = iso3, year, pol_b = polity2)]
iv_panel <- merge(iv_panel, pol_exp, by = c("iso_a", "year"), all.x = TRUE)
iv_panel <- merge(iv_panel, pol_imp, by = c("iso_b", "year"), all.x = TRUE)
iv_panel[, polity_dist := abs(pol_a - pol_b)]
iv_panel[, c("pol_a", "pol_b") := NULL]

# ATOP allied (direct) - undirected, on symetrise
allied_und <- unique(rbind(
  allied_atop[, .(iso_a, iso_b, year, allied_atop)],
  allied_atop[, .(iso_a = iso_b, iso_b = iso_a, year, allied_atop)]
))
iv_panel <- merge(iv_panel, allied_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(allied_atop), allied_atop := 0L]
# allied_atop n'est valide que pour annees ATOP couvertes : mettre NA hors
# fenetre
iv_panel[year > 2018L, allied_atop := NA_integer_]

# Shared ally
iv_panel <- merge(iv_panel, shared_ally,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(shared_ally_atop) & year <= 2018L, shared_ally_atop := 0L]
iv_panel[year > 2018L, shared_ally_atop := NA_integer_]

# MID direct (control)
iv_panel <- merge(iv_panel, mid_direct,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(mid_direct) & year <= 2014L, mid_direct := 0L]
iv_panel[year > 2014L, mid_direct := NA_integer_]

# Shared rival
iv_panel <- merge(iv_panel, shared_rival,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(shared_rival_mid) & year <= 2014L, shared_rival_mid := 0L]
iv_panel[year > 2014L, shared_rival_mid := NA_integer_]

# Sanctions undirected (any/trade/nontrade)
iv_panel <- merge(iv_panel, sanc_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(sanction_any)      & year <= 2023L, sanction_any      := 0L]
iv_panel[is.na(sanction_trade)    & year <= 2023L, sanction_trade    := 0L]
iv_panel[is.na(sanction_nontrade) & year <= 2023L, sanction_nontrade := 0L]
iv_panel[year > 2023L, c("sanction_any", "sanction_trade",
                          "sanction_nontrade") := NA_integer_]

# Indicatrices par type (undirected) + score de canal sanc_n_types
iv_panel <- merge(iv_panel, sanc_type_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
for (v in type_cols) iv_panel[is.na(get(v)) & year <= 2023L, (v) := 0L]
iv_panel[year > 2023L, (type_cols) := NA_integer_]
iv_panel[, sanc_n_types := sanc_arms + sanc_military + sanc_financial +
           sanc_travel + sanc_other + sanc_trade_complete + sanc_trade_partial]

# Doses d'intensite (undirected)
iv_panel <- merge(iv_panel, dose_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(sanc_n_active_all)  & year <= 2023L, sanc_n_active_all  := 0L]
iv_panel[is.na(sanc_n_active_core) & year <= 2023L, sanc_n_active_core := 0L]
iv_panel[year > 2023L, c("sanc_n_active_all", "sanc_n_active_core") := NA_integer_]

# Common sanctioners (count)
iv_panel <- merge(iv_panel, common_snc,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(n_common_sanctioners) & year <= 2023L,
         n_common_sanctioners := 0L]
iv_panel[year > 2023L, n_common_sanctioners := NA_integer_]


# ---- Section 9 : Diagnostics ------------------------------------------------

log_step("Section 9 : diagnostics couverture.")

setnames(iv_panel, c("iso_a", "iso_b"), c("exp_iso3", "imp_iso3"))

cat("\n=== Panel IV alternative ===\n")
cat("Lignes (paires-annees)              :", nrow(iv_panel), "\n")
cat("Annees                              :",
    min(iv_panel$year), "-", max(iv_panel$year), "\n\n")

cat("Couverture par variable (non-NA en % du panel) :\n")
for (v in c("polyarchy_dist", "joint_dem_vdem", "ideol_dist", "polity_dist",
            "allied_atop", "shared_ally_atop", "mid_direct", "shared_rival_mid",
            "sanction_any", "sanction_trade", "sanction_nontrade",
            "n_common_sanctioners")) {
  pct <- 100 * mean(!is.na(iv_panel[[v]]))
  cat(sprintf("  %-22s : %.1f%%\n", v, pct))
}
cat("\nStats sanctions (sur obs avec sanction_any non-NA) :\n")
snc <- iv_panel[!is.na(sanction_any)]
cat(sprintf("  Sanction_any      = 1 : %d (%.2f%%)\n",
            snc[sanction_any == 1L, .N],
            100 * mean(snc$sanction_any)))
cat(sprintf("  Sanction_trade    = 1 : %d (%.2f%%)\n",
            snc[sanction_trade == 1L, .N],
            100 * mean(snc$sanction_trade)))
cat(sprintf("  Sanction_nontrade = 1 : %d (%.2f%%)\n",
            snc[sanction_nontrade == 1L, .N],
            100 * mean(snc$sanction_nontrade)))
cat(sprintf("  n_common_sanctioners > 0 : %d (%.2f%%, max = %d)\n",
            snc[n_common_sanctioners > 0L, .N],
            100 * mean(snc$n_common_sanctioners > 0),
            max(snc$n_common_sanctioners, na.rm = TRUE)))

cat("\nDistribution par famille :\n")
cat("  institutional (V-Dem + DPI joint non-NA)   :",
    iv_panel[!is.na(polyarchy_dist) & !is.na(ideol_dist), .N], "\n")
cat("  strategic_relations (ATOP + MID joint)     :",
    iv_panel[!is.na(allied_atop) & !is.na(shared_rival_mid), .N], "\n")
cat("  Polity (separate, robustness)              :",
    iv_panel[!is.na(polity_dist), .N], "\n")


# ---- Section 10 : Sauvegarde -----------------------------------------------

log_step("Section 10 : sauvegarde.")

out <- file.path(PATH_CLEAN, "iv_panel.parquet")
write_parquet_safe(iv_panel, out)
cat("  - Ecrit :", out, "\n")
cat("  -", nrow(iv_panel), "lignes,", ncol(iv_panel), "colonnes\n")

log_step("Termine.")
