# =============================================================================
# 01_build_master_panel.R   (etape prealable — construction des donnees)
# -----------------------------------------------------------------------------
# Construction de la base master dyadique directionnelle pour le memoire M2.
# Sources : BACI HS92 V202601, Gravity CEPII V202211 (time-invariant only),
#           IPD Bailey-Strezhnev-Voeten 1946-2025, World Bank WDI.
# Output  : Data/Clean/master_panel.parquet et .csv
# Chemins / wrappers I/O / helpers (log_step, YEAR_MIN/MAX) : 00_setup.R.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(WDI)
  library(readxl)
})

source("00_setup.R")  # PATH_RAW/CLEAN/BACI/GRAV/IPD, wrappers, log_step, YEAR_*

stopifnot(dir.exists(PATH_RAW), dir.exists(PATH_BACI),
          dir.exists(PATH_GRAV), dir.exists(PATH_IPD))

log_step("Setup termine. Periode cible : 1995-2024.")


# ---- Section 1 : Crosswalk pays --------------------------------------------

log_step("Section 1 : construction du crosswalk pays.")

baci_codes <- fread(file.path(PATH_BACI, "country_codes_V202601.csv"),
                    encoding = "UTF-8")
setnames(baci_codes, c("country_code", "country_name", "iso2", "iso3"))

# Cas Belgium-Luxembourg (code 58, valide jusqu'a 1998) -> mappe deja a BEL
# par CEPII. Pas de remediation supplementaire.
iso3_map <- baci_codes[!is.na(iso3) & iso3 != "N/A" & nchar(iso3) == 3,
                       .(country_code, iso3)]

# Quelques codes BACI peuvent avoir iso3 manquant (zones non-attribuees).
n_missing_iso3 <- baci_codes[is.na(iso3) | nchar(iso3) != 3, .N]
cat("  - Pays BACI total          :", nrow(baci_codes), "\n")
cat("  - Pays avec ISO3 valide    :", nrow(iso3_map), "\n")
cat("  - Pays sans ISO3 (drop)    :", n_missing_iso3, "\n")
cat("  - Doublons ISO3 (Bel-Lux)  :", iso3_map[, .N, iso3][N > 1, .N], "\n")


# ---- Section 2 : BACI - agregation par pair-annee --------------------------

log_step("Section 2 : agregation BACI par (exp,imp,year).")

baci_cache <- file.path(PATH_CLEAN, "_baci_agg_cache.parquet")
if (file.exists(baci_cache)) {
  log_step(paste("  Cache trouve, lecture :", baci_cache))
  baci <- as.data.table(read_parquet(baci_cache))
} else {

baci_files <- list.files(PATH_BACI, pattern = "^BACI_HS92_Y[0-9]+_V202601\\.csv$",
                         full.names = TRUE)
baci_files <- baci_files[order(baci_files)]
cat("  -", length(baci_files), "fichiers annuels detectes.\n")

baci_list <- vector("list", length(baci_files))
for (i in seq_along(baci_files)) {
  f <- baci_files[i]
  yr <- as.integer(sub(".*_Y([0-9]{4})_.*", "\\1", basename(f)))
  dt <- fread(f, select = c("t", "i", "j", "v"),
              colClasses = c(t = "integer", i = "integer",
                             j = "integer", v = "numeric"))
  agg <- dt[, .(trade_value = sum(v, na.rm = TRUE)), by = .(t, i, j)]
  baci_list[[i]] <- agg
  cat(sprintf("    [%d/%d] %d : %d obs HS6 -> %d paires\n",
              i, length(baci_files), yr, nrow(dt), nrow(agg)))
  rm(dt, agg); gc(verbose = FALSE)
}
baci <- rbindlist(baci_list)
rm(baci_list); gc(verbose = FALSE)

setnames(baci, c("t", "i", "j"), c("year", "exp_code", "imp_code"))

# Merge codes BACI -> ISO3 (exporter puis importer)
baci <- merge(baci, iso3_map, by.x = "exp_code", by.y = "country_code",
              all.x = TRUE)
setnames(baci, "iso3", "exp_iso3")
baci <- merge(baci, iso3_map, by.x = "imp_code", by.y = "country_code",
              all.x = TRUE)
setnames(baci, "iso3", "imp_iso3")

n_pre <- nrow(baci)
n_drop_iso <- baci[is.na(exp_iso3) | is.na(imp_iso3), .N]
baci <- baci[!is.na(exp_iso3) & !is.na(imp_iso3)]

# Si Belgium (56) et Belgium-Luxembourg (58) coexistent dans la meme annee,
# on agrege apres mapping ISO3 (ils tombent tous deux sur BEL).
baci <- baci[, .(trade_value = sum(trade_value, na.rm = TRUE)),
             by = .(exp_iso3, imp_iso3, year)]

setkey(baci, exp_iso3, imp_iso3, year)
write_parquet(baci, baci_cache)
cat("  - Cache ecrit :", baci_cache, "\n")
}

if (exists("n_pre")) {
  cat("  - Obs BACI avant drop ISO3 :", n_pre, "\n")
  cat("  - Drop sans ISO3 valide    :", n_drop_iso, "\n")
}
cat("  - Obs BACI finales         :", nrow(baci), "\n")
cat("  - Annees couvertes         :", min(baci$year), "-", max(baci$year), "\n")
cat("  - Exporteurs uniques       :", uniqueN(baci$exp_iso3), "\n")
cat("  - Importateurs uniques     :", uniqueN(baci$imp_iso3), "\n")
cat("  - Paires uniques (dir.)    :", uniqueN(baci[, .(exp_iso3, imp_iso3)]), "\n")


# ---- Section 2bis : Cartesian expansion for PPML zeros ----------------------
#
# BACI ne contient que les flux positifs. Pour le PPML, on a besoin d'observer
# aussi les flux nuls (paire-annee avec trade_value = 0). On densifie donc le
# panel en generant le produit cartesien (exp x imp x year) sur les pays
# suffisamment actifs, puis on impute 0 aux paires absentes de BACI.

log_step("Section 2bis : expansion cartesienne pour zeros PPML.")

MIN_YEARS_ACTIVE <- 5L

# 1. Identifier les pays "actifs" : >= 5 annees, comme exp OU imp
exp_yrs <- baci[, .(yrs_exp = uniqueN(year)), by = exp_iso3]
imp_yrs <- baci[, .(yrs_imp = uniqueN(year)), by = imp_iso3]
setnames(exp_yrs, "exp_iso3", "iso3")
setnames(imp_yrs, "imp_iso3", "iso3")
country_yrs <- merge(exp_yrs, imp_yrs, by = "iso3", all = TRUE)
country_yrs[is.na(yrs_exp), yrs_exp := 0L]
country_yrs[is.na(yrs_imp), yrs_imp := 0L]
country_yrs[, yrs_any := pmax(yrs_exp, yrs_imp)]

active_countries   <- country_yrs[yrs_any >= MIN_YEARS_ACTIVE, iso3]
excluded_countries <- country_yrs[yrs_any <  MIN_YEARS_ACTIVE, iso3]

cat("  - Filtre minimum annees actives :", MIN_YEARS_ACTIVE, "\n")
cat("  - Pays actifs retenus           :", length(active_countries), "\n")
cat("  - Pays exclus (faible activite) :", length(excluded_countries),
    if (length(excluded_countries))
      paste0(" -> ", paste(sort(excluded_countries), collapse = ", ")) else "",
    "\n")

# 2. Generer le cartesien (exp, imp, year) avec exp != imp
years_all <- sort(unique(baci$year))
cart <- CJ(exp_iso3 = active_countries,
           imp_iso3 = active_countries,
           year     = years_all)[exp_iso3 != imp_iso3]
cat("  - Cartesien genere              :", nrow(cart),
    "obs (", length(active_countries), "x", length(active_countries) - 1L,
    "x", length(years_all), "ans)\n")

# 3. Left join : flux BACI sur le cartesien, zeros pour les NA
n_baci_before <- nrow(baci)
baci <- merge(cart, baci, by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
n_pos_after_merge <- baci[!is.na(trade_value), .N]
n_drop_excluded   <- n_baci_before - n_pos_after_merge
baci[is.na(trade_value), trade_value := 0]
setkey(baci, exp_iso3, imp_iso3, year)
rm(cart, country_yrs, exp_yrs, imp_yrs); gc(verbose = FALSE)

n_zero <- baci[trade_value == 0, .N]
n_pos  <- baci[trade_value >  0, .N]
cat("  - Apres expansion : obs totales :", nrow(baci), "\n")
cat("    trade_value > 0               :", n_pos,
    sprintf("(%.1f%%)", 100 * n_pos  / nrow(baci)), "\n")
cat("    trade_value = 0               :", n_zero,
    sprintf("(%.1f%%)", 100 * n_zero / nrow(baci)), "\n")
cat("  - Obs BACI ecartees (pays <5y)  :", n_drop_excluded, "\n")


# ---- Section 3 : Gravity time-invariant -------------------------------------

log_step("Section 3 : extraction des variables structurelles gravity.")

grav_cols <- c("year", "iso3_o", "iso3_d",
               "dist", "distw_harmonic", "distcap",
               "contig", "comlang_off", "comlang_ethno",
               "col45", "comcol", "comrelig")

grav <- fread(file.path(PATH_GRAV, "Gravity_V202211.csv"),
              select = grav_cols)

cat("  - Lignes gravity brutes    :", nrow(grav), "\n")
cat("  - Annees gravity           :", min(grav$year), "-", max(grav$year), "\n")

# Collapse a une ligne par paire en gardant la valeur la plus recente non-NA.
# Pour les vars structurelles (dist, contig, ...) ces valeurs sont
# theoriquement constantes ; ce choix nous protege des NA epars.
setorder(grav, iso3_o, iso3_d, -year)

last_non_na <- function(x) {
  v <- x[!is.na(x)]
  # x[NA_integer_] renvoie un NA du meme type que x : garantit la
  # coherence de type entre groupes (sinon data.table erreur).
  if (length(v) == 0L) x[NA_integer_] else v[1L]
}

grav_inv <- grav[, lapply(.SD, last_non_na),
                 by = .(iso3_o, iso3_d),
                 .SDcols = c("dist", "distw_harmonic", "distcap",
                             "contig", "comlang_off", "comlang_ethno",
                             "col45", "comcol", "comrelig")]

setnames(grav_inv,
         c("iso3_o", "iso3_d", "dist", "distw_harmonic", "distcap",
           "contig", "comlang_off", "comlang_ethno",
           "col45", "comcol", "comrelig"),
         c("exp_iso3", "imp_iso3", "dist", "distw_harm", "dist_cap",
           "contig", "comlang_off", "comlang_eth",
           "colony", "comcol", "comrelig"))

cat("  - Paires gravity uniques   :", nrow(grav_inv), "\n")
cat("  - Paires avec dist non-NA  :", grav_inv[!is.na(dist), .N], "\n")
rm(grav); gc(verbose = FALSE)


# ---- Section 4 : IPD --------------------------------------------------------

log_step("Section 4 : IPD - distance geopolitique dyadique.")

ipd <- fread(file.path(PATH_IPD, "IdealPointDyads1946-2025.csv"),
             select = c("iso3c1", "iso3c2", "year", "AbsIdealDiff"))
setnames(ipd, c("iso3c1", "iso3c2", "AbsIdealDiff"),
              c("exp_iso3", "imp_iso3", "ipd"))

cat("  - Lignes IPD brutes        :", nrow(ipd), "\n")
cat("  - Annees IPD               :", min(ipd$year), "-", max(ipd$year), "\n")

# IPD est undirected : on dupplique avec inversion des roles
# (AbsIdealDiff est symmetrique par construction).
ipd_rev <- copy(ipd)
setnames(ipd_rev, c("exp_iso3", "imp_iso3"), c("tmp", "exp_iso3"))
setnames(ipd_rev, "tmp", "imp_iso3")
setcolorder(ipd_rev, c("exp_iso3", "imp_iso3", "year", "ipd"))

ipd <- rbindlist(list(ipd, ipd_rev))
ipd <- unique(ipd, by = c("exp_iso3", "imp_iso3", "year"))
ipd <- ipd[exp_iso3 != imp_iso3]
setkey(ipd, exp_iso3, imp_iso3, year)

cat("  - Lignes IPD apres sym.    :", nrow(ipd), "\n")
cat("  - Paires dir. IPD          :", uniqueN(ipd[, .(exp_iso3, imp_iso3)]), "\n")
rm(ipd_rev); gc(verbose = FALSE)


# ---- Section 4bis : RTA from DESTA -----------------------------------------
#
# DESTA (Design of Trade Agreements) liste les traites commerciaux dyadiques.
# On garde les traites de base (base_treaty == 1) pour eviter le double-
# comptage avec les amendements. On construit une indicatrice 0/1 par
# (exp, imp, year) = 1 si au moins un traite est en vigueur a cette annee
# (entryforceyear <= year). Pas d'info de termination dans le fichier list-
# of-treaties : hypothese implicite = traite en vigueur reste en vigueur.

log_step("Section 4bis : RTA from DESTA.")

desta_file <- file.path(PATH_RAW, "desta_list_of_treaties_02_03_dyads.xlsx")
desta <- as.data.table(read_excel(desta_file, sheet = "data"))

cat("  - Lignes DESTA brutes        :", nrow(desta), "\n")
# entry_type = "base_treaty" (15166) | "accession" (3438) | "consolidated" (1965)
# On garde base_treaty + accession : les adhesions creent de nouvelles dyades
# en vigueur (ex : HUN rejoint l'UE en 2004). On exclut "consolidated"
# (versions consolidees, deja captures par base_treaty + accession).
desta <- desta[entry_type %in% c("base_treaty", "accession")]
cat("  - Apres filtre entry_type    :", nrow(desta), "\n")
desta <- desta[!is.na(entryforceyear)]
cat("  - Avec entryforceyear        :", nrow(desta), "\n")

# DESTA iso1/iso2 sont des codes ISO 3166-1 numerique (France=250, USA=840).
# Attention : BACI utilise UN COMTRADE qui DIFFERE (France=251, USA=842, etc.)
# On construit donc un crosswalk ISO num -> ISO3 dedie depuis CEPII Countries.
cepii_countries <- fread(file.path(PATH_GRAV, "Countries_V202211.csv"))
isonum_map <- unique(cepii_countries[!is.na(iso3num) & !is.na(iso3) & nchar(iso3) == 3,
                                     .(iso3num, iso3)])

desta <- merge(desta, isonum_map, by.x = "iso1", by.y = "iso3num", all.x = TRUE)
setnames(desta, "iso3", "iso3_1")
desta <- merge(desta, isonum_map, by.x = "iso2", by.y = "iso3num", all.x = TRUE)
setnames(desta, "iso3", "iso3_2")

n_pre <- nrow(desta)
desta <- desta[!is.na(iso3_1) & !is.na(iso3_2)]
cat("  - Apres mapping ISO3         :", nrow(desta),
    "(", n_pre - nrow(desta), "drops sans match)\n")

# Pour chaque paire : annee d'entree en vigueur du PREMIER traite (le plus ancien)
desta_min <- desta[, .(rta_start = min(entryforceyear)),
                   by = .(iso3_1, iso3_2)]

# Symetrisation : DESTA est undirected. Construction explicite des deux sens
# pour eviter le piege de rbindlist par position vs par nom.
desta_sym <- rbindlist(list(
  desta_min,
  data.table(iso3_1    = desta_min$iso3_2,
             iso3_2    = desta_min$iso3_1,
             rta_start = desta_min$rta_start)
))
# Une paire dans les deux sens d'origine : garder le min
desta_sym <- desta_sym[, .(rta_start = min(rta_start)),
                       by = .(iso3_1, iso3_2)]
desta_sym <- desta_sym[iso3_1 != iso3_2]
setnames(desta_sym, c("iso3_1", "iso3_2"), c("exp_iso3", "imp_iso3"))
setkey(desta_sym, exp_iso3, imp_iso3)

cat("  - Paires uniques avec RTA    :", nrow(desta_sym), "\n")
cat("  - Range entryforceyear       :", min(desta_sym$rta_start), "-",
    max(desta_sym$rta_start), "\n")
rm(desta, desta_min, cepii_countries, isonum_map); gc(verbose = FALSE)


# ---- Section 4ter : NATO membership ----------------------------------------
#
# Adhesion NATO hardcodee depuis la liste officielle. Un pays est membre a
# partir de l'annee d'adhesion (incluse). Sert a deriver exp_nato, imp_nato
# et pair_nato (intra/inter/non) en Section 6.

log_step("Section 4ter : NATO membership.")

nato_members <- data.table(
  iso3 = c(
    "USA", "GBR", "FRA", "BEL", "NLD", "LUX", "CAN", "DNK", "ISL",
    "ITA", "NOR", "PRT",
    "GRC", "TUR",
    "DEU",
    "ESP",
    "CZE", "HUN", "POL",
    "BGR", "EST", "LVA", "LTU", "ROU", "SVK", "SVN",
    "ALB", "HRV",
    "MNE",
    "MKD",
    "FIN",
    "SWE"
  ),
  nato_join_year = c(
    rep(1949, 12),
    rep(1952, 2),
    1955,
    1982,
    rep(1999, 3),
    rep(2004, 7),
    rep(2009, 2),
    2017,
    2020,
    2023,
    2024
  )
)

cat("  - Membres NATO definis        :", nrow(nato_members), "\n")
cat("  - Membres en 1995             :",
    nato_members[nato_join_year <= 1995, .N], "\n")
cat("  - Membres en", YEAR_MAX, "             :",
    nato_members[nato_join_year <= YEAR_MAX, .N], "\n")


# ---- Section 5 : World Bank via WDI -----------------------------------------

log_step("Section 5 : telechargement WDI (peut prendre 30-60s).")

wdi_indicators <- c(
  gdp_nominal = "NY.GDP.MKTP.CD",
  gdp_real    = "NY.GDP.MKTP.KD",
  inflation   = "FP.CPI.TOTL.ZG",
  deflator    = "NY.GDP.DEFL.KD.ZG",
  pop         = "SP.POP.TOTL"
)

wdi_raw <- as.data.table(
  WDI(country = "all",
      indicator = wdi_indicators,
      start = 1992, end = YEAR_MAX,
      extra = TRUE)
)

# Garde uniquement les pays (et non les agregats regionaux)
wdi <- wdi_raw[!is.na(iso3c) & region != "Aggregates",
               c("iso3c", "year", names(wdi_indicators)), with = FALSE]
setnames(wdi, "iso3c", "iso3")

cat("  - Pays WDI (hors agreg.)   :", uniqueN(wdi$iso3), "\n")
cat("  - Annees WDI               :", min(wdi$year), "-", max(wdi$year), "\n")
for (v in names(wdi_indicators)) {
  na_rate <- mean(is.na(wdi[[v]])) * 100
  cat(sprintf("  - %% NA %-12s       : %.1f%%\n", v, na_rate))
}
rm(wdi_raw); gc(verbose = FALSE)


# ---- Section 6 : Assemblage du panel master ---------------------------------

log_step("Section 6 : merges sur le squelette BACI.")

panel <- copy(baci)
n0 <- nrow(panel)

# 6.1 IPD : merge dyadique (exp, imp, year)
panel <- merge(panel, ipd, by = c("exp_iso3", "imp_iso3", "year"),
               all.x = TRUE)
n_match_ipd <- panel[!is.na(ipd), .N]
cat(sprintf("  - Match IPD       : %d / %d (%.1f%%)\n",
            n_match_ipd, n0, 100 * n_match_ipd / n0))

# 6.1bis RTA : merge sur (exp, imp), construire rta = 1 si year >= rta_start
panel <- merge(panel, desta_sym, by = c("exp_iso3", "imp_iso3"), all.x = TRUE)
panel[, rta := as.integer(!is.na(rta_start) & year >= rta_start)]
panel[, rta_start := NULL]
n_match_rta <- panel[rta == 1L, .N]
cat(sprintf("  - Match RTA       : %d / %d (%.1f%% du panel = 1)\n",
            n_match_rta, n0, 100 * n_match_rta / n0))

# 6.1ter NATO : merge cote exp et cote imp, derivation pair_nato
panel <- merge(panel, nato_members[, .(iso3, nato_join_exp = nato_join_year)],
               by.x = "exp_iso3", by.y = "iso3", all.x = TRUE)
panel <- merge(panel, nato_members[, .(iso3, nato_join_imp = nato_join_year)],
               by.x = "imp_iso3", by.y = "iso3", all.x = TRUE)
panel[, exp_nato := as.integer(!is.na(nato_join_exp) & year >= nato_join_exp)]
panel[, imp_nato := as.integer(!is.na(nato_join_imp) & year >= nato_join_imp)]
panel[, pair_nato := fcase(
  exp_nato == 1L & imp_nato == 1L, "intra",
  exp_nato + imp_nato == 1L,       "inter",
  exp_nato == 0L & imp_nato == 0L, "non"
)]
panel[, c("nato_join_exp", "nato_join_imp") := NULL]
cat(sprintf("  - NATO : exp_nato==1 : %d  imp_nato==1 : %d  intra : %d\n",
            panel[exp_nato == 1L, .N], panel[imp_nato == 1L, .N],
            panel[pair_nato == "intra", .N]))

# 6.2 Gravity time-invariant : merge dyadique (exp, imp)
panel <- merge(panel, grav_inv, by = c("exp_iso3", "imp_iso3"),
               all.x = TRUE)
n_match_grav <- panel[!is.na(dist), .N]
cat(sprintf("  - Match Gravity   : %d / %d (%.1f%%)\n",
            n_match_grav, n0, 100 * n_match_grav / n0))

# 6.3 WDI cote exporter
wdi_exp <- copy(wdi)
setnames(wdi_exp,
         c("iso3", names(wdi_indicators)),
         c("exp_iso3", paste0("exp_", names(wdi_indicators))))
panel <- merge(panel, wdi_exp, by = c("exp_iso3", "year"), all.x = TRUE)

# 6.4 WDI cote importer
wdi_imp <- copy(wdi)
setnames(wdi_imp,
         c("iso3", names(wdi_indicators)),
         c("imp_iso3", paste0("imp_", names(wdi_indicators))))
panel <- merge(panel, wdi_imp, by = c("imp_iso3", "year"), all.x = TRUE)

n_match_wdi_exp <- panel[!is.na(exp_gdp_nominal), .N]
n_match_wdi_imp <- panel[!is.na(imp_gdp_nominal), .N]
cat(sprintf("  - Match WDI exp   : %d / %d (%.1f%%)\n",
            n_match_wdi_exp, n0, 100 * n_match_wdi_exp / n0))
cat(sprintf("  - Match WDI imp   : %d / %d (%.1f%%)\n",
            n_match_wdi_imp, n0, 100 * n_match_wdi_imp / n0))

# Ordre des colonnes final
setcolorder(panel, c(
  "exp_iso3", "imp_iso3", "year",
  "trade_value",
  "ipd",
  "rta",
  "exp_nato", "imp_nato", "pair_nato",
  "dist", "distw_harm", "dist_cap", "contig",
  "comlang_off", "comlang_eth",
  "colony", "comcol", "comrelig",
  "exp_gdp_nominal", "imp_gdp_nominal",
  "exp_gdp_real",    "imp_gdp_real",
  "exp_inflation",   "imp_inflation",
  "exp_deflator",    "imp_deflator",
  "exp_pop",         "imp_pop"
))
setkey(panel, exp_iso3, imp_iso3, year)


# ---- Section 7 : Diagnostics finaux -----------------------------------------

log_step("Section 7 : diagnostics finaux.")

cat("\n========================================================\n")
cat("PANEL MASTER - DIAGNOSTICS\n")
cat("========================================================\n")
cat("Lignes (obs dir.)          :", nrow(panel), "\n")
cat("Colonnes                   :", ncol(panel), "\n")
cat("Annees couvertes           :", min(panel$year), "-", max(panel$year), "\n")
cat("Exporteurs uniques         :", uniqueN(panel$exp_iso3), "\n")
cat("Importateurs uniques       :", uniqueN(panel$imp_iso3), "\n")
cat("Paires directionnelles     :", uniqueN(panel[, .(exp_iso3, imp_iso3)]), "\n")
n_pos_f  <- panel[trade_value >  0, .N]
n_zero_f <- panel[trade_value == 0, .N]
cat(sprintf("Trade value > 0            : %d (%.1f%%)\n",
            n_pos_f,  100 * n_pos_f  / nrow(panel)))
cat(sprintf("Trade value = 0            : %d (%.1f%%)\n",
            n_zero_f, 100 * n_zero_f / nrow(panel)))

# Top 10 paires les plus zero-intensives (sanity check : doivent etre
# des micro-Etats ou des paires improbables geographiquement)
pair_zeros <- panel[, .(n_zero     = sum(trade_value == 0),
                        n_obs      = .N,
                        share_zero = mean(trade_value == 0)),
                    by = .(exp_iso3, imp_iso3)]
top_zeros <- pair_zeros[order(-share_zero, -n_zero, exp_iso3, imp_iso3)][1:10]
cat("\nTop 10 paires zero-intensives (share = % annees a 0) :\n")
print(top_zeros[, .(exp_iso3, imp_iso3, n_zero, n_obs,
                    share = sprintf("%.1f%%", 100 * share_zero))])

cat("\n% NA par variable :\n")
na_rates <- sapply(panel, function(x) mean(is.na(x)) * 100)
print(round(na_rates, 2))

# Source la plus contraignante (sur la fenetre temporelle)
years_baci <- sort(unique(baci$year))
years_ipd  <- sort(unique(ipd$year))
years_wdi  <- sort(unique(wdi$year))
cat("\nFenetres temporelles par source :\n")
cat("  BACI    :", min(years_baci), "-", max(years_baci), "\n")
cat("  IPD     :", min(years_ipd),  "-", max(years_ipd),  "\n")
cat("  WDI     :", min(years_wdi),  "-", max(years_wdi),  "\n")
cat("  Gravity : time-invariant (collapse de 1948-2021)\n")
cat("=> Source contraignante a gauche : BACI (1995)\n")
cat("=> Source contraignante a droite : BACI (2024)\n")

# Pays presents BACI mais absents d'autres sources
baci_iso <- unique(c(panel$exp_iso3, panel$imp_iso3))
ipd_iso  <- unique(c(ipd$exp_iso3, ipd$imp_iso3))
grav_iso <- unique(c(grav_inv$exp_iso3, grav_inv$imp_iso3))
wdi_iso  <- unique(wdi$iso3)

cat("\nPays BACI absents d'autres sources :\n")
cat("  - Absents IPD     :", paste(sort(setdiff(baci_iso, ipd_iso)),  collapse = ", "), "\n")
cat("  - Absents Gravity :", paste(sort(setdiff(baci_iso, grav_iso)), collapse = ", "), "\n")
cat("  - Absents WDI     :", paste(sort(setdiff(baci_iso, wdi_iso)),  collapse = ", "), "\n")


# ---- Section 8 : Sauvegarde -------------------------------------------------

log_step("Section 8 : sauvegarde parquet + csv + README.")

out_parquet <- file.path(PATH_CLEAN, "master_panel.parquet")
out_csv     <- file.path(PATH_CLEAN, "master_panel.csv")
out_readme  <- file.path(PATH_CLEAN, "README.md")

write_parquet(panel, out_parquet)
fwrite(panel, out_csv)

readme_txt <- c(
  "# Master panel - memoire M2",
  "",
  paste("Build date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Sources",
  "- BACI HS92 V202601 (CEPII) : commerce bilateral HS6, agrege par paire-annee",
  "- Gravity V202211 (CEPII)   : variables structurelles time-invariant uniquement",
  "- IPD 1946-2025 (Bailey-Strezhnev-Voeten) : distance ideologique UNGA (AbsIdealDiff)",
  "- DESTA list-of-treaties    : RTA dyadique (base_treaty == 1, en vigueur)",
  "- World Bank WDI            : GDP nominal/reel, inflation, deflateur, population",
  "",
  "## Format",
  "- Panel directionnel : (exp_iso3, imp_iso3, year) - FRA->DEU != DEU->FRA",
  "- Cles : ISO3 alphabetique partout",
  "- Trade_value en milliers USD (convention BACI), 0 pour les flux nuls",
  "- Panel densifie par expansion cartesienne pour estimation PPML",
  "",
  "## Dimensions",
  paste("- Observations            :", nrow(panel)),
  paste("- Annees                  :", min(panel$year), "-", max(panel$year)),
  paste("- Exporteurs              :", uniqueN(panel$exp_iso3)),
  paste("- Importateurs            :", uniqueN(panel$imp_iso3)),
  paste("- Paires directionnelles  :", uniqueN(panel[, .(exp_iso3, imp_iso3)])),
  paste0("- trade_value > 0          : ", n_pos_f,
         sprintf(" (%.1f%%)", 100 * n_pos_f  / nrow(panel))),
  paste0("- trade_value = 0          : ", n_zero_f,
         sprintf(" (%.1f%%)", 100 * n_zero_f / nrow(panel))),
  "",
  "## Variables",
  "- exp_iso3, imp_iso3, year : cles",
  "- trade_value              : exports en milliers USD (BACI)",
  "- ipd                      : |IdealPointFP_exp - IdealPointFP_imp|",
  "- rta                      : 1 si >=1 traite DESTA base en vigueur a year",
  "- exp_nato, imp_nato       : 1 si pays membre NATO en year (hardcode)",
  "- pair_nato                : 'intra' (2 NATO), 'inter' (1 NATO), 'non' (0)",
  "- dist, distw_harm, dist_cap : distances geographiques (km)",
  "- contig                   : frontiere commune (0/1)",
  "- comlang_off, comlang_eth : langue officielle / ethnique commune",
  "- colony                   : relation coloniale post-1945",
  "- comcol                   : colonisateur commun",
  "- comrelig                 : indice de religion commune",
  "- exp/imp_gdp_nominal      : PIB USD courants (NY.GDP.MKTP.CD)",
  "- exp/imp_gdp_real         : PIB USD constants 2015 (NY.GDP.MKTP.KD)",
  "- exp/imp_inflation        : inflation CPI annuelle (FP.CPI.TOTL.ZG)",
  "- exp/imp_deflator         : deflateur GDP annuel (NY.GDP.DEFL.KD.ZG)",
  "- exp/imp_pop              : population totale (SP.POP.TOTL)",
  "",
  "## Notes methodologiques",
  "- Les variables gravity sont collapsees a une ligne par paire en prenant",
  "  la derniere valeur non-NA (proteges contre les NA epars puisque les",
  "  variables choisies sont structurellement time-invariant).",
  "- IPD est symetrise : (USA,CAN) et (CAN,USA) recoivent la meme AbsIdealDiff.",
  "- Belgium-Luxembourg (code BACI 58, jusqu'a 1998) est mappe sur BEL et",
  "  agrege avec Belgium (56) si coexistant.",
  "- Expansion cartesienne pour PPML : panel densifie en (exp x imp x year)",
  "  sur les pays presents au moins 5 annees dans BACI (comme exp ou imp).",
  "  Les paires sans flux BACI recoivent trade_value = 0.",
  "  Les self-flows (exp_iso3 == imp_iso3) sont exclus.",
  "- Filtre activite : un pays doit apparaitre dans au moins 5 annees BACI",
  "  pour entrer dans le cartesien. Aucun autre filtre applique.",
  "  Les FE exporter-year et importer-year du PPML absorberont les atypiques.",
  "- Les variables time-varying (IPD, WDI) restent NA pour les paires-annees",
  "  ou la source ne couvre pas le pays/annee. Comportement standard pour",
  "  PPML : ces obs sont droppees par fixest::fepois lors de l'estimation."
)
writeLines(readme_txt, out_readme)

log_step(paste("Termine. Parquet :", out_parquet))
log_step(paste("        Csv     :", out_csv))
log_step(paste("        Readme  :", out_readme))
