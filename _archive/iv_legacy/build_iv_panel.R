# =============================================================================
# build_iv_panel.R   (LEGACY — hors pipeline actif, lu par PERSONNE)
# -----------------------------------------------------------------------------
# Reproduit l'ancien iv_panel.parquet (8 distances "instruments alternatifs")
# pour l'historique / la reproduction des diagnostics IV archives (07/08/09-IV).
# N'est PAS dans la chaine active : le pipeline lit sanctions_panel.parquet.
#
# Base = sanctions_panel.parquet (produit par 03_sanctions/03_build_sanctions.R),
# qui porte deja le squelette gravite, les sanctions, et les NIVEAUX
# exp/imp_polyarchy. Ce script n'AJOUTE que les 8 distances IV :
#   - derivees des niveaux deja presents (pas de relecture V-Dem) :
#       polyarchy_dist = |exp_polyarchy - imp_polyarchy|
#       joint_dem_vdem = pmin(exp_polyarchy, imp_polyarchy)
#   - reconstruites depuis les sources brutes (code deplace depuis l'ancien 03,
#     logique inchangee) :
#       ideol_dist        (DPI 2023)
#       polity_dist       (Polity5)
#       allied_atop, shared_ally_atop   (ATOP v5.1)
#       mid_direct,  shared_rival_mid   (dyadic MID v4.03)
#
# Output : Data/Clean/_archive/iv_panel.parquet  (= colonnes de sanctions_panel
#          + 8 distances IV). Le backup fige iv_panel_backup_20260624.parquet
#          n'est pas touche.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "readxl", "countrycode", "stringi", "haven")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(readxl)
  library(countrycode); library(stringi)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})

PATH_RAW <- PATH_IV   # sources brutes des familles (Data/Raw/IV)
log_step("Setup termine (build_iv_panel LEGACY).")


# ---- Section 1 : base = sanctions_panel + helpers ---------------------------

log_step("Section 1 : lecture sanctions_panel (base).")
base <- read_parquet_safe(PATH_SANCTIONS_PANEL)
stopifnot(all(c("exp_polyarchy", "imp_polyarchy") %in% names(base)))
setnames(base, c("exp_iso3", "imp_iso3"), c("iso_a", "iso_b"))
master_iso3 <- sort(unique(c(base$iso_a, base$iso_b)))
cat("  - sanctions_panel :", nrow(base), "lignes,", ncol(base), "colonnes\n")

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


# ---- Section 7 : tiers communs (shared_ally_atop / shared_rival_mid) --------

log_step("Section 7 : agregation dyadique (shared counts).")

# Helper : compte de tiers communs (k tels que (i,k) ET (j,k) sont dans 'set')
compute_shared <- function(set_table, value_name) {
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

allies_und2 <- allies_und[, .(focal, partner = ally, year)]
shared_ally <- compute_shared(allies_und2, "shared_ally_atop")

rivals_of2 <- rivals_of[, .(focal, partner = rival, year)]
shared_rival <- compute_shared(rivals_of2, "shared_rival_mid")


# ---- Section 8 : ajout des 8 distances IV a la base -------------------------

log_step("Section 8 : merge des distances IV.")

iv_panel <- copy(base); rm(base); gc(verbose = FALSE)

# (1) Distances V-Dem derivees des NIVEAUX deja presents (pas de relecture).
iv_panel[, polyarchy_dist := abs(exp_polyarchy - imp_polyarchy)]
iv_panel[, joint_dem_vdem := pmin(exp_polyarchy, imp_polyarchy)]

# (2) DPI execrlc -> ideol_dist
dpi_exp <- dpi[, .(iso_a = iso3, year, dpi_a = execrlc)]
dpi_imp <- dpi[, .(iso_b = iso3, year, dpi_b = execrlc)]
iv_panel <- merge(iv_panel, dpi_exp, by = c("iso_a", "year"), all.x = TRUE)
iv_panel <- merge(iv_panel, dpi_imp, by = c("iso_b", "year"), all.x = TRUE)
iv_panel[, ideol_dist := abs(dpi_a - dpi_b)]
iv_panel[, c("dpi_a", "dpi_b") := NULL]

# (3) Polity (robustness) -> polity_dist
pol_exp <- polity[, .(iso_a = iso3, year, pol_a = polity2)]
pol_imp <- polity[, .(iso_b = iso3, year, pol_b = polity2)]
iv_panel <- merge(iv_panel, pol_exp, by = c("iso_a", "year"), all.x = TRUE)
iv_panel <- merge(iv_panel, pol_imp, by = c("iso_b", "year"), all.x = TRUE)
iv_panel[, polity_dist := abs(pol_a - pol_b)]
iv_panel[, c("pol_a", "pol_b") := NULL]

# (4) ATOP allied (direct) - undirected, on symetrise
allied_und <- unique(rbind(
  allied_atop[, .(iso_a, iso_b, year, allied_atop)],
  allied_atop[, .(iso_a = iso_b, iso_b = iso_a, year, allied_atop)]
))
iv_panel <- merge(iv_panel, allied_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(allied_atop), allied_atop := 0L]
iv_panel[year > 2018L, allied_atop := NA_integer_]

# (5) Shared ally
iv_panel <- merge(iv_panel, shared_ally,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(shared_ally_atop) & year <= 2018L, shared_ally_atop := 0L]
iv_panel[year > 2018L, shared_ally_atop := NA_integer_]

# (6) MID direct (control)
iv_panel <- merge(iv_panel, mid_direct,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(mid_direct) & year <= 2014L, mid_direct := 0L]
iv_panel[year > 2014L, mid_direct := NA_integer_]

# (7) Shared rival
iv_panel <- merge(iv_panel, shared_rival,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
iv_panel[is.na(shared_rival_mid) & year <= 2014L, shared_rival_mid := 0L]
iv_panel[year > 2014L, shared_rival_mid := NA_integer_]


# ---- Section 9 : diagnostics + sauvegarde -----------------------------------

setnames(iv_panel, c("iso_a", "iso_b"), c("exp_iso3", "imp_iso3"))

cat("\n=== iv_panel LEGACY : couverture distances IV (non-NA %) ===\n")
for (v in c("polyarchy_dist", "joint_dem_vdem", "ideol_dist", "polity_dist",
            "allied_atop", "shared_ally_atop", "mid_direct", "shared_rival_mid")) {
  cat(sprintf("  %-22s : %.1f%%\n", v, 100 * mean(!is.na(iv_panel[[v]]))))
}

out_dir <- file.path(PATH_CLEAN, "_archive")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(out_dir, "iv_panel.parquet")
write_parquet_safe(iv_panel, out)
cat("  - Ecrit :", out, "\n")
cat("  -", nrow(iv_panel), "lignes,", ncol(iv_panel), "colonnes\n")

log_step("Termine (build_iv_panel LEGACY).")
