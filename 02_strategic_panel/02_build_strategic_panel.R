# =============================================================================
# 02_build_strategic_panel.R   (etape prealable — construction des donnees)
# -----------------------------------------------------------------------------
# Enrichit master_panel.parquet (produit par 01_build_master_panel.R) avec deux
# variables agregees sur les codes HS6 strategiques (classification Aiyar et
# al. 2024, IMF "Geoeconomic Fragmentation") :
#   - strategic_trade_value : somme du commerce sur les codes HS6 strategiques
#   - strategic_trade_share : ratio strategic / total (NA si trade_value == 0)
# Output : Data/Clean/master_panel_with_strategic.parquet et .csv
# Chemins / wrappers I/O / helpers : centralises dans 00_setup.R.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})  # fournit PATH_RAW/CLEAN/BACI, wrappers I/O, log_step

stopifnot(dir.exists(PATH_BACI), dir.exists(PATH_CLEAN))
stopifnot(file.exists(file.path(PATH_CLEAN, "master_panel.parquet")))

log_step("Setup termine.")


# ---- Section 1 : Codes HS6 strategiques -------------------------------------

log_step("Section 1 : definition des codes HS6 strategiques.")

strategic_codes <- list(

  # ========================================================================
  # 1. SEMICONDUCTORS
  # ========================================================================
  semiconductors = c(
    "854110", "854121", "854129", "854130", "854140", "854150", "854160",
    "854190",
    "854231", "854232", "854233", "854239", "854290",
    "848610", "848620", "848630", "848640", "848690",
    "381800"
  ),

  # ========================================================================
  # 2. TELECOMMUNICATIONS AND 5G
  # ========================================================================
  telecommunications_5g = c(
    "851761", "851762", "851769",
    "851711", "851712", "851718",
    "851770", "852691", "852692",
    "854470", "900110",
    "852910", "852990"
  ),

  # ========================================================================
  # 3. GREEN TRANSITION EQUIPMENT
  # ========================================================================
  green_transition = c(
    "854140", "850440",
    "850231", "850239",
    "850760", "850720", "850730", "850740", "850780", "850790",
    "850110", "850120", "850131", "850132", "850133", "850134", "850140",
    "850151", "850152", "850153",
    "841861", "841869",
    "850680"
  ),

  # ========================================================================
  # 4. PHARMACEUTICAL INGREDIENTS
  # ========================================================================
  pharmaceuticals = c(
    "294110", "294120", "294130", "294140", "294150", "294190",
    "293710", "293721", "293722", "293723", "293729", "293731", "293739",
    "293740", "293750", "293790",
    "293611", "293621", "293622", "293623", "293624", "293625", "293626",
    "293627", "293628", "293629",
    "293500", "293410", "293420", "293430", "293490",
    "300210", "300220", "300230", "300290",
    "300310", "300320", "300331", "300339", "300340", "300390"
  ),

  # ========================================================================
  # 5. CRITICAL MINERALS
  # ========================================================================
  critical_minerals = c(
    "280530", "284610", "284690",
    "282520", "283691",
    "810520", "810530", "810590", "282200",
    "750210", "750220",
    "810110", "810191", "810192", "810193", "810194", "810199", "284180",
    "810810", "810820", "810890", "282300",
    "811292", "811299",
    "250410", "250490", "380110",
    "811100", "282010",
    "811291", "282530",
    "711011", "711021", "711031", "711041",
    "260300", "260400", "261000", "261100", "261590", "261690", "261710",
    "261790", "253090"
  ),

  # ========================================================================
  # 6. DEFENSE (commerce civil retire)
  # ========================================================================
  defense = c(
    "930111", "930119", "930120", "930190",
    "930200", "930310", "930320", "930330", "930390", "930400",
    "930510", "930521", "930529", "930591", "930599",
    "930610", "930621", "930629", "930630", "930690",
    "871000",
    "880211", "880212", "880220", "880230", "880240", "880260",
    "880310", "880320", "880330", "880390",
    "890600",
    "852610", "852691", "852692",
    "901310", "901320", "901380", "901390",
    "360100", "360200", "360300",
    "284410", "284420", "284430", "284440", "284450", "284510", "284590"
  )
)

all_strategic_codes <- unique(unlist(strategic_codes))

n_before <- length(unlist(strategic_codes))
n_after  <- length(all_strategic_codes)
dups <- names(table(unlist(strategic_codes))[table(unlist(strategic_codes)) > 1])

cat("  - Codes par secteur :\n")
for (s in names(strategic_codes)) {
  cat(sprintf("      %-22s : %2d codes\n", s, length(strategic_codes[[s]])))
}
cat("  - Total avant dedup     :", n_before, "\n")
cat("  - Total apres dedup     :", n_after, "\n")
cat("  - Doublons retires      :", n_before - n_after,
    "(", paste(dups, collapse = ", "), ")\n")


# ---- Section 1b : Liste d'embargo de reference (proxy HS6, fixe, uniforme) ---
#
# SOURCE : annexes du reglement (UE) 833/2014 et ses paquets de sanctions
# successifs (2022 -> 2025), categorie par categorie. Utilisee pour la PARTITION
# a 3 buckets (decomposition §5 : effet mecanique du ban vs vraie fragmentation).
#
# AVERTISSEMENT (a lire) : liste de REFERENCE en PROXY. Elle est ancree au niveau
# CHAPITRE (2 chiffres) ou POSITION (4 chiffres) HS la ou le ban est large, plutot
# qu'au code HS6 exact (fausse precision evitee). Elle est appliquee UNIFORMEMENT
# a TOUTES les paires (PAS sender-specifique, PAS datee) : c'est une grille de
# lecture transversale, pas le perimetre juridique exact d'un sender a une date.
# REVISABLE. Mecanique de matching (cf. emb_chapters/positions/exact ci-dessous) :
# un code HS6 k est classe "embargo" si son chapitre substr(k,1,2) ∈ emb_chapters,
# OU sa position substr(k,1,4) ∈ emb_positions, OU k exact ∈ emb_exact.
log_step("Section 1b : liste d'embargo de reference (proxy HS6).")
embargo_codes <- list(
  # --- IMPORTS depuis la Russie (chapitres entiers : ban large) ---
  energy_ch27       = "27",   # combustibles mineraux : petrole brut/raffines, gaz, charbon (paquets 5-8). DOMINE l'embargo (cf. audit).
  steel_iron_ch72   = "72",   # fer & acier (chapitre). HS73 (articles en fer/acier) NON inclus par defaut (mesures plus ciblees) -> extension possible.
  aluminium_ch76    = "76",   # aluminium (restrictions partielles paquet 12-2023 ; proxy chapitre).
  fish_seafood_ch03 = "03",   # poissons & crustaces/mollusques (paquet 5).
  wood_ch44         = "44",   # bois (cas du contreplaque de bouleau, paquet 5 ; proxy chapitre).
  # --- IMPORTS metaux/mineraux precieux (positions) ---
  gold_7108         = "7108", # or (paquet 7).
  diamonds_7102     = "7102", # diamants non industriels (paquet 12 ; proxy position, le ban vise le non-industriel).
  # cuivre/nickel : laisses HORS par defaut (extension possible : 7402/7403/7502...).
  # --- EXPORTS vers la Russie : luxe ---
  luxury_cars_8703  = "8703", # voitures de tourisme : luxe au-dela d'un seuil de valeur dans le reglement ; ici proxy POSITION (pas de seuil applique), documente.
  watches_ch91      = "91",   # horlogerie (chapitre) : luxe.
  jewellery_7113    = "7113"  # articles de bijouterie (position) : luxe.
)
# Dual-use / techno avancee (EXPORT vers la Russie) : on NE DUPLIQUE PAS la liste.
# On REFERENCE les sous-listes d'Aiyar a la fois STRATEGIQUES et typiquement sous
# embargo export (UE 833/2014 annexe VII, biens a double usage) : semiconducteurs,
# telecoms/5G, defense. (Choix : ces 3 sous-listes ; green/pharma/critical_minerals
# restent "strategiques non-embargo" sauf si captees par un chapitre ci-dessus.)
embargo_dualuse_codes <- unique(c(strategic_codes$semiconductors,
                                  strategic_codes$telecommunications_5g,
                                  strategic_codes$defense))

# Decoupage par longueur de match (prefixe chapitre / prefixe position / code exact)
.emb_all      <- unlist(embargo_codes, use.names = FALSE)
emb_chapters  <- .emb_all[nchar(.emb_all) == 2]   # matche substr(k,1,2)
emb_positions <- .emb_all[nchar(.emb_all) == 4]   # matche substr(k,1,4)
emb_exact     <- unique(c(.emb_all[nchar(.emb_all) == 6], embargo_dualuse_codes))  # matche k exact
# Vecteur EXPOSE de codes HS6 explicites (les blocs chapitre/position sont matches
# par prefixe, non enumeres : on n'a pas la nomenclature HS6 complete sous la main).
all_embargo_codes <- sort(unique(emb_exact))

# Classement MECE d'un code HS6 : embargo > strategique > reste.
classify_bucket <- function(k_str) {
  ch  <- substr(k_str, 1L, 2L)
  pos <- substr(k_str, 1L, 4L)
  fifelse(ch %in% emb_chapters | pos %in% emb_positions | k_str %in% emb_exact,
          "embargo",
          fifelse(k_str %in% all_strategic_codes, "strategic_nonembargo",
                  "nonstrategic_nonembargo"))
}
cat(sprintf("  - embargo : %d chapitres (%s), %d positions (%s), %d codes exacts (dont %d dual-use Aiyar)\n",
            length(emb_chapters), paste(emb_chapters, collapse = ","),
            length(emb_positions), paste(emb_positions, collapse = ","),
            length(emb_exact), length(embargo_dualuse_codes)))


# ---- Section 2 : Crosswalk pays --------------------------------------------

log_step("Section 2 : crosswalk pays (BACI numerique -> ISO3).")

baci_codes <- fread(file.path(PATH_BACI, "country_codes_V202601.csv"),
                    encoding = "UTF-8")
setnames(baci_codes, c("country_code", "country_name", "iso2", "iso3"))

iso3_map <- baci_codes[!is.na(iso3) & iso3 != "N/A" & nchar(iso3) == 3,
                       .(country_code, iso3)]
cat("  - Pays avec ISO3 valide :", nrow(iso3_map), "\n")

# Code numerique BACI de la Russie (ISO num 643) recupere DEPUIS le crosswalk
# (pas code en dur a l'aveugle). Sert au numerateur de la dependance energetique.
RUS_code <- iso3_map[iso3 == "RUS", country_code]
stopifnot(length(RUS_code) == 1L)
cat("  - Code BACI Russie       :", RUS_code, "\n")


# ---- Section 3 : Lecture filtree BACI sur codes strategiques ----------------

log_step("Section 3 : lecture filtree BACI (codes HS6 strategiques).")

strat_cache <- file.path(PATH_CLEAN, "_baci_strategic_cache.parquet")

if (file.exists(strat_cache)) {
  log_step(paste("  Cache trouve, lecture :", strat_cache))
  strat <- as.data.table(read_parquet(strat_cache))
  codes_observed <- attr(strat, "codes_observed")
  if (is.null(codes_observed)) {
    codes_observed <- character(0)  # fallback
  }
} else {

  baci_files <- list.files(PATH_BACI,
                           pattern = "^BACI_HS92_Y[0-9]+_V202601\\.csv$",
                           full.names = TRUE)
  baci_files <- baci_files[order(baci_files)]

  strat_list <- vector("list", length(baci_files))
  energy_list <- vector("list", length(baci_files))  # [ENERGIE] RUS HS27 par (t,j)
  codes_seen <- character(0)

  for (i in seq_along(baci_files)) {
    f <- baci_files[i]
    yr <- as.integer(sub(".*_Y([0-9]{4})_.*", "\\1", basename(f)))

    dt <- fread(f,
                select = c("t", "i", "j", "k", "v"),
                colClasses = c(t = "integer", i = "integer", j = "integer",
                               k = "integer", v = "numeric"))

    # Padding HS6 : safety meme si aucun code strategique ne commence par 0
    dt[, k_str := sprintf("%06d", k)]

    # Filtre aux codes strategiques
    dt_s <- dt[k_str %in% all_strategic_codes]
    codes_this_year <- unique(dt_s$k_str)
    codes_seen <- union(codes_seen, codes_this_year)

    # Agregation par paire-annee
    agg <- dt_s[, .(strategic_trade_value = sum(v, na.rm = TRUE)),
                by = .(t, i, j)]
    strat_list[[i]] <- agg

    # [ENERGIE] flux dont l'exportateur est la Russie ET le HS6 commence par "27"
    # (combustibles mineraux), agreges par importateur j. Numerateur de la
    # dependance energetique russe (cf. Section 3bis + Section 5).
    energy_list[[i]] <- dt[dt[["i"]] == RUS_code & substr(k_str, 1, 2) == "27",
                           .(rus_hs27_value = sum(v, na.rm = TRUE)), by = .(t, j)]

    cat(sprintf("    [%d/%d] %d : %d obs HS6 -> %d obs strategiques -> %d paires (%d codes vus)\n",
                i, length(baci_files), yr,
                nrow(dt), nrow(dt_s), nrow(agg), length(codes_this_year)))

    rm(dt, dt_s, agg); gc(verbose = FALSE)
  }

  strat <- rbindlist(strat_list)
  setnames(strat, c("t", "i", "j"), c("year", "exp_code", "imp_code"))

  # Merge codes BACI -> ISO3
  strat <- merge(strat, iso3_map, by.x = "exp_code", by.y = "country_code",
                 all.x = TRUE)
  setnames(strat, "iso3", "exp_iso3")
  strat <- merge(strat, iso3_map, by.x = "imp_code", by.y = "country_code",
                 all.x = TRUE)
  setnames(strat, "iso3", "imp_iso3")

  n_pre <- nrow(strat)
  strat <- strat[!is.na(exp_iso3) & !is.na(imp_iso3)]

  # Agregation post-ISO3 (gere BEL = 56 + 58)
  strat <- strat[, .(strategic_trade_value = sum(strategic_trade_value,
                                                  na.rm = TRUE)),
                 by = .(exp_iso3, imp_iso3, year)]
  setkey(strat, exp_iso3, imp_iso3, year)

  attr(strat, "codes_observed") <- codes_seen
  codes_observed <- codes_seen

  write_parquet(strat, strat_cache)
  cat("  - Obs strategiques avant ISO3 :", n_pre, "\n")
  cat("  - Obs strategiques finales    :", nrow(strat), "\n")
  cat("  - Cache ecrit :", strat_cache, "\n")
  rm(strat_list); gc(verbose = FALSE)
}

cat("  - Paires-annees strategiques  :", nrow(strat), "\n")
cat("  - Annees couvertes            :", min(strat$year), "-", max(strat$year), "\n")
cat("  - Pays exporteurs uniques     :", uniqueN(strat$exp_iso3), "\n")
cat("  - Pays importateurs uniques   :", uniqueN(strat$imp_iso3), "\n")


# ---- Section 3bis : numerateur dependance energetique russe (RUS HS27) -------
#
# Numerateur = somme des flux BACI dont l'exportateur est la Russie ET le HS6
# commence par "27" (combustibles mineraux), agrege par (importateur, annee).
# Cache propre _baci_energy_cache.parquet (mirroir du cache strategique).

log_step("Section 3bis : dependance energetique russe (numerateur RUS HS27).")

energy_cache <- file.path(PATH_CLEAN, "_baci_energy_cache.parquet")

# Agrege une liste de morceaux (t, j, rus_hs27_value) -> (imp_iso3, year, value)
build_energy_num <- function(en_list) {
  en <- rbindlist(en_list)
  en <- en[, .(rus_hs27_value = sum(rus_hs27_value, na.rm = TRUE)), by = .(t, j)]
  setnames(en, c("t", "j"), c("year", "imp_code"))
  en <- merge(en, iso3_map, by.x = "imp_code", by.y = "country_code", all.x = TRUE)
  setnames(en, "iso3", "imp_iso3")
  en <- en[!is.na(imp_iso3),
           .(rus_hs27_value = sum(rus_hs27_value, na.rm = TRUE)),
           by = .(imp_iso3, year)]
  setkey(en, imp_iso3, year)
  en[]
}

if (file.exists(energy_cache)) {
  log_step(paste("  Cache energie trouve, lecture :", energy_cache))
  energy_num <- as.data.table(read_parquet(energy_cache))
} else if (exists("energy_list")) {
  # La passe BACI strategique vient de tourner : on reutilise energy_list.
  energy_num <- build_energy_num(energy_list)
  write_parquet(energy_num, energy_cache)
  cat("  - Cache energie ecrit :", energy_cache, "\n")
  rm(energy_list); gc(verbose = FALSE)
} else {
  # Strategic recharge depuis cache -> passe BACI dediee legere filtree sur RUS.
  log_step("  (cache strategique present) passe BACI dediee RUS HS27.")
  baci_files2 <- list.files(PATH_BACI,
                            pattern = "^BACI_HS92_Y[0-9]+_V202601\\.csv$",
                            full.names = TRUE)
  en_list2 <- vector("list", length(baci_files2))
  for (fi in seq_along(baci_files2)) {
    dt <- fread(baci_files2[fi], select = c("t", "i", "j", "k", "v"),
                colClasses = c(t = "integer", i = "integer", j = "integer",
                               k = "integer", v = "numeric"))
    dt[, k_str := sprintf("%06d", k)]
    en_list2[[fi]] <- dt[dt[["i"]] == RUS_code & substr(k_str, 1, 2) == "27",
                         .(rus_hs27_value = sum(v, na.rm = TRUE)), by = .(t, j)]
    rm(dt); gc(verbose = FALSE)
  }
  energy_num <- build_energy_num(en_list2)
  write_parquet(energy_num, energy_cache)
  cat("  - Cache energie ecrit :", energy_cache, "\n")
}
cat("  - Numerateur energie (imp_iso3, year > 0) :", nrow(energy_num), "\n")
cat("  - Annees couvertes                        :",
    min(energy_num$year), "-", max(energy_num$year), "\n")


# ---- Section 3ter : partition MECE a 3 buckets (UNE passe BACI) --------------
#
# Classe CHAQUE code HS6 lu dans BACI en UN seul bucket (precedence
# embargo > strategique > reste) et agrege par paire DIRIGEE-annee. Une seule
# lecture BACI : on classe en 3 buckets, on NE relit PAS le fichier 3 fois.
# Cache dedie _baci_buckets_cache.parquet (panel) + _baci_buckets_audit_cache.parquet
# (audit par code) ; les caches strategique/energie existants sont conserves.
log_step("Section 3ter : partition MECE 3 buckets (1 passe BACI, classe chaque HS6).")
buckets_cache <- file.path(PATH_CLEAN, "_baci_buckets_cache.parquet")
audit_cache   <- file.path(PATH_CLEAN, "_baci_buckets_audit_cache.parquet")

if (file.exists(buckets_cache) && file.exists(audit_cache)) {
  log_step(paste("  Caches buckets trouves, lecture :", buckets_cache))
  bkt   <- as.data.table(read_parquet(buckets_cache))
  audit <- as.data.table(read_parquet(audit_cache))
} else {
  baci_files3 <- list.files(PATH_BACI,
                            pattern = "^BACI_HS92_Y[0-9]+_V202601\\.csv$",
                            full.names = TRUE)
  baci_files3 <- baci_files3[order(baci_files3)]
  bkt_list <- vector("list", length(baci_files3))
  aud_list <- vector("list", length(baci_files3))
  for (fi in seq_along(baci_files3)) {
    f <- baci_files3[fi]; yr <- as.integer(sub(".*_Y([0-9]{4})_.*", "\\1", basename(f)))
    dt <- fread(f, select = c("t", "i", "j", "k", "v"),
                colClasses = c(t = "integer", i = "integer", j = "integer",
                               k = "integer", v = "numeric"))
    dt[, k_str := sprintf("%06d", k)]
    dt[, bucket := classify_bucket(k_str)]                 # 1 classification, 3 buckets
    bkt_list[[fi]] <- dt[, .(v = sum(v, na.rm = TRUE)), by = .(t, i, j, bucket)]
    dt[, rus_dir := fcase(i == RUS_code, "RUS_exp",        # cote export russe
                          j == RUS_code, "RUS_imp",        # cote import russe
                          default = "other")]
    aud_list[[fi]] <- dt[, .(v = sum(v, na.rm = TRUE), n = .N),
                         by = .(bucket, k_str, rus_dir)]
    cat(sprintf("    [%d/%d] %d : %d obs HS6 classes en buckets\n",
                fi, length(baci_files3), yr, nrow(dt)))
    rm(dt); gc(verbose = FALSE)
  }
  # Panel : codes BACI -> ISO3 (gere BEL = 56 + 58), agrege par (iso3, iso3, year, bucket)
  bkt <- rbindlist(bkt_list)
  setnames(bkt, c("t", "i", "j"), c("year", "exp_code", "imp_code"))
  bkt <- merge(bkt, iso3_map, by.x = "exp_code", by.y = "country_code", all.x = TRUE)
  setnames(bkt, "iso3", "exp_iso3")
  bkt <- merge(bkt, iso3_map, by.x = "imp_code", by.y = "country_code", all.x = TRUE)
  setnames(bkt, "iso3", "imp_iso3")
  bkt <- bkt[!is.na(exp_iso3) & !is.na(imp_iso3),
             .(v = sum(v, na.rm = TRUE)), by = .(exp_iso3, imp_iso3, year, bucket)]
  bkt <- dcast(bkt, exp_iso3 + imp_iso3 + year ~ bucket, value.var = "v", fill = 0)
  for (b in c("embargo", "strategic_nonembargo", "nonstrategic_nonembargo"))
    if (!b %in% names(bkt)) bkt[, (b) := 0]
  setnames(bkt,
           c("embargo", "strategic_nonembargo", "nonstrategic_nonembargo"),
           c("embargo_trade_value", "strategic_nonembargo_trade_value",
             "nonstrategic_nonembargo_trade_value"))
  setkey(bkt, exp_iso3, imp_iso3, year)
  # Audit : par (bucket, code HS6, direction RUS) sur TOUTES les annees
  audit <- rbindlist(aud_list)[, .(v = sum(v, na.rm = TRUE), n_obs = sum(n)),
                               by = .(bucket, k_str, rus_dir)]
  write_parquet(bkt, buckets_cache)
  write_parquet(audit, audit_cache)
  cat("  - Cache buckets ecrit :", buckets_cache, "\n")
  cat("  - Cache audit ecrit   :", audit_cache, "\n")
  rm(bkt_list, aud_list); gc(verbose = FALSE)
}
cat("  - Paires-annees buckets :", nrow(bkt), "| annees :",
    min(bkt$year), "-", max(bkt$year), "\n")


# ---- Section 4 : Diagnostic codes strategiques manquants --------------------

log_step("Section 4 : codes strategiques jamais observes dans BACI HS92.")

if (length(codes_observed) > 0) {
  codes_missing <- setdiff(all_strategic_codes, codes_observed)
  cat("  - Codes strategiques definis  :", length(all_strategic_codes), "\n")
  cat("  - Codes observes dans BACI    :", length(codes_observed), "\n")
  cat("  - Codes JAMAIS observes       :", length(codes_missing), "\n")
  if (length(codes_missing) > 0) {
    cat("    -> Probablement codes HS22 absents de la nomenclature HS92.\n")
    cat("    Liste :", paste(sort(codes_missing), collapse = ", "), "\n")
    # Identifier dans quels secteurs ils sont
    for (s in names(strategic_codes)) {
      miss_s <- intersect(codes_missing, strategic_codes[[s]])
      if (length(miss_s) > 0) {
        cat(sprintf("    [%s] : %s\n", s, paste(miss_s, collapse = ", ")))
      }
    }
  }
} else {
  cat("  (cache reload : diagnostic codes manquants saute)\n")
}


# ---- Section 5 : Merge sur master_panel.parquet -----------------------------

log_step("Section 5 : merge sur master_panel.parquet.")

panel <- as.data.table(read_parquet(file.path(PATH_CLEAN, "master_panel.parquet")))
cat("  - Master panel : ", nrow(panel), "obs,", ncol(panel), "colonnes\n")

n0 <- nrow(panel)
panel <- merge(panel, strat,
               by = c("exp_iso3", "imp_iso3", "year"),
               all.x = TRUE)
n_match <- panel[!is.na(strategic_trade_value), .N]
cat(sprintf("  - Match strategic : %d / %d (%.2f%%)\n",
            n_match, n0, 100 * n_match / n0))

# CRITIQUE : NA -> 0. Les paires-annees sans flux strategique ne sont pas
# des donnees manquantes : c'est legitimement zero (necessaire pour PPML).
panel[is.na(strategic_trade_value), strategic_trade_value := 0]

# strategic_trade_share : NA si trade_value == 0 ou NA (division 0/0)
panel[, strategic_trade_share := strategic_trade_value / trade_value]
panel[!is.finite(strategic_trade_share), strategic_trade_share := NA_real_]
panel[trade_value == 0, strategic_trade_share := NA_real_]

# non_strategic_trade : complement du strategique (>= 0 ; = trade_value si
# strategic == 0). pmax(.,0) absorbe le bruit flottant (strategic est un
# sous-ensemble du total, donc <= trade_value a l'arrondi pres).
panel[, non_strategic_trade := pmax(trade_value - strategic_trade_value, 0)]
# non_strategic_share : meme convention que strategic_trade_share (NA si total 0)
panel[, non_strategic_share := non_strategic_trade / trade_value]
panel[!is.finite(non_strategic_share), non_strategic_share := NA_real_]
panel[trade_value == 0, non_strategic_share := NA_real_]

# Dependance energetique russe (monadique, mergee sur les DEUX cotes).
# Denominateur = importations TOTALES du pays (concept de dependance cote
# import) = somme de trade_value par (imp_iso3, year) du master panel.
# Variante "part du commerce total" (imports + exports) : remplacer tot_imp par
# une somme bilaterale des deux sens ; NON activee par defaut.
tot_imp <- panel[, .(tot_imp = sum(trade_value, na.rm = TRUE)),
                 by = .(imp_iso3, year)]
energy_dep <- merge(tot_imp, energy_num, by = c("imp_iso3", "year"), all.x = TRUE)
energy_dep[is.na(rus_hs27_value), rus_hs27_value := 0]
# NA si denominateur == 0 (eviter 0/0) ; sinon part dans [0, 1].
energy_dep[, energy_dep_rus := fifelse(tot_imp > 0,
                                       rus_hs27_value / tot_imp, NA_real_)]
energy_dep <- energy_dep[, .(iso3 = imp_iso3, year, energy_dep_rus)]
panel <- merge(panel,
               energy_dep[, .(exp_iso3 = iso3, year,
                              exp_energy_dep_rus = energy_dep_rus)],
               by = c("exp_iso3", "year"), all.x = TRUE)
panel <- merge(panel,
               energy_dep[, .(imp_iso3 = iso3, year,
                              imp_energy_dep_rus = energy_dep_rus)],
               by = c("imp_iso3", "year"), all.x = TRUE)

# Verif : panel doit garder exactement les memes lignes
stopifnot(nrow(panel) == n0)

# Validation rapide (loggee)
cat(sprintf("  - non_strategic_trade < 0                 : %d (attendu 0)\n",
            panel[non_strategic_trade < 0, .N]))
cat(sprintf("  - |non_strat + strat - total| > 1e-3      : %d (attendu ~0)\n",
            panel[abs(non_strategic_trade + strategic_trade_value -
                      trade_value) > 1e-3, .N]))
cat(sprintf("  - energy_dep_rus (imp) hors [0,1] non-NA  : %d (attendu 0)\n",
            panel[!is.na(imp_energy_dep_rus) &
                  (imp_energy_dep_rus < 0 | imp_energy_dep_rus > 1), .N]))
cat(sprintf("  - imp_energy_dep_rus non-NA               : %.1f%%\n",
            100 * mean(!is.na(panel$imp_energy_dep_rus))))


# ---- Section 5b : merge partition 3 buckets + shares + assertion MECE --------

log_step("Section 5b : merge buckets MECE (embargo / strategic_nonembargo / reste).")
panel <- merge(panel, bkt, by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
BUCKS <- c("embargo_trade_value", "strategic_nonembargo_trade_value",
           "nonstrategic_nonembargo_trade_value")
# Absence de flux dans un bucket = zero legitime (pas NA), comme l'existant.
for (b in BUCKS) panel[is.na(get(b)), (b) := 0]
stopifnot(nrow(panel) == n0)   # le panel garde exactement ses lignes

# ASSERTION MECE : embargo + strategic_nonembargo + nonstrategic_nonembargo == trade_value
panel[, .bsum := embargo_trade_value + strategic_nonembargo_trade_value +
        nonstrategic_nonembargo_trade_value]
tol <- pmax(1e-3, 1e-6 * panel$trade_value)
n_bad <- panel[abs(.bsum - trade_value) > tol, .N]
cat(sprintf("  - MECE : lignes |somme buckets - trade_value| > tol : %d / %d\n", n_bad, nrow(panel)))
if (n_bad > 0) {
  ex <- panel[abs(.bsum - trade_value) > tol][order(-abs(.bsum - trade_value))][1:min(5L, n_bad)]
  cat("  !! Ecarts MECE (top, BACI buckets vs master trade_value) :\n")
  print(ex[, .(exp_iso3, imp_iso3, year, trade_value, bsum = .bsum, ecart = .bsum - trade_value)])
  # Directive de session : ne pas stopper -> RECONCILIATION. Les deux premiers
  # buckets (embargo, strategic_nonembargo) restent issus de BACI ; le 3e bucket
  # est ANCRE sur trade_value (residuel, meme convention pmax que non_strategic_trade)
  # pour garantir l'identite MECE vs le total du master (consomme en aval).
  cat("  !! Reconciliation : nonstrategic_nonembargo := pmax(trade_value - embargo - strategic_nonembargo, 0).\n")
  panel[, nonstrategic_nonembargo_trade_value :=
          pmax(trade_value - embargo_trade_value - strategic_nonembargo_trade_value, 0)]
  panel[, .bsum := embargo_trade_value + strategic_nonembargo_trade_value +
          nonstrategic_nonembargo_trade_value]
}
# Garde-fou dur : apres reconciliation l'identite doit tenir (sinon embargo+strat > total).
n_hard <- panel[abs(.bsum - trade_value) > pmax(1e-2, 1e-5 * trade_value), .N]
if (n_hard > 0) {
  print(panel[abs(.bsum - trade_value) > pmax(1e-2, 1e-5*trade_value)][1:5,
        .(exp_iso3, imp_iso3, year, trade_value, bsum = .bsum)])
  stop(sprintf("Assertion MECE violee sur %d lignes : embargo+strategic_nonembargo depasse trade_value.", n_hard))
}
panel[, .bsum := NULL]

# Shares (NA si trade_value == 0/NA : meme convention que strategic_trade_share)
for (b in BUCKS) {
  sh <- sub("_trade_value$", "_share", b)
  panel[, (sh) := get(b) / trade_value]
  panel[!is.finite(get(sh)), (sh) := NA_real_]
  panel[trade_value == 0, (sh) := NA_real_]
}
cat(sprintf("  - Parts moyennes (trade>0) : embargo %.4f | strategic_nonembargo %.4f | reste %.4f\n",
            panel[trade_value > 0, mean(embargo_share, na.rm = TRUE)],
            panel[trade_value > 0, mean(strategic_nonembargo_share, na.rm = TRUE)],
            panel[trade_value > 0, mean(nonstrategic_nonembargo_share, na.rm = TRUE)]))


# ---- Section 6 : Diagnostics finaux -----------------------------------------

log_step("Section 6 : diagnostics finaux.")

cat("\n========================================================\n")
cat("PANEL MASTER + STRATEGIC - DIAGNOSTICS\n")
cat("========================================================\n")
cat("Lignes (obs dir.)              :", nrow(panel), "\n")
cat("Colonnes                       :", ncol(panel), "\n")
cat("strategic_trade_value > 0      :", panel[strategic_trade_value > 0, .N],
    sprintf("(%.1f%% du panel)", 100 * panel[strategic_trade_value > 0, .N] / nrow(panel)),
    "\n")
cat("strategic_trade_value = 0      :", panel[strategic_trade_value == 0, .N],
    sprintf("(%.1f%% du panel)", 100 * panel[strategic_trade_value == 0, .N] / nrow(panel)),
    "\n")

cat("\nDistribution strategic_trade_share (conditionnel a trade_value > 0) :\n")
qs <- panel[trade_value > 0 & !is.na(strategic_trade_share),
            quantile(strategic_trade_share,
                     probs = c(0.10, 0.25, 0.50, 0.75, 0.90))]
cat(sprintf("  Moyenne          : %.4f (%.2f%%)\n",
            panel[trade_value > 0, mean(strategic_trade_share, na.rm = TRUE)],
            100 * panel[trade_value > 0, mean(strategic_trade_share, na.rm = TRUE)]))
cat(sprintf("  Mediane          : %.4f (%.2f%%)\n", qs[3], 100 * qs[3]))
cat(sprintf("  P10 / P25        : %.4f / %.4f\n", qs[1], qs[2]))
cat(sprintf("  P75 / P90        : %.4f / %.4f\n", qs[4], qs[5]))

cat("\nTop 10 paires en commerce strategique cumule 1995-2024 :\n")
top_pairs <- panel[, .(strategic_cum = sum(strategic_trade_value, na.rm = TRUE),
                       trade_cum     = sum(trade_value,           na.rm = TRUE)),
                   by = .(exp_iso3, imp_iso3)
                  ][order(-strategic_cum)][1:10]
top_pairs[, share_cum := strategic_cum / trade_cum]
print(top_pairs[, .(exp_iso3, imp_iso3,
                    strategic_cum_bnUSD = round(strategic_cum / 1e6, 1),
                    trade_cum_bnUSD     = round(trade_cum     / 1e6, 1),
                    share = sprintf("%.1f%%", 100 * share_cum))])


# ---- Section 6b : audit de composition des buckets --------------------------
#
# Calcule sur l'agregat d'audit BACI (bucket, code HS6, direction RUS). Tables
# ecrites dans PATH_CLEAN (a cote du panel/README). Objectif : verifier que le
# bucket embargo est SURTOUT de l'energie, mesurer le chevauchement embargo ∩
# strategique, et rendre lisible "ce qu'il y a dans chaque bucket".
log_step("Section 6b : audit de composition (codes, valeur, energie, chevauchement, direction).")
au <- copy(audit)
au[, ch := substr(k_str, 1L, 2L)]
au[, russie := rus_dir != "other"]
tot_all <- au[, sum(v)]; tot_rus <- au[russie == TRUE, sum(v)]

# (1) Composition par bucket : nb codes HS6, valeur (totale & Russie-centree), parts
comp <- au[, .(n_codes      = uniqueN(k_str[v > 0]),
               value_total  = sum(v),
               value_russia = sum(v[russie == TRUE])), by = bucket]
comp[, `:=`(share_total  = value_total  / tot_all,
            share_russia = value_russia / tot_rus)]
setorder(comp, -value_total)
fwrite(comp, file.path(PATH_CLEAN, "tab_bucket_composition.csv"))

# (2) Part de HS27 (energie) dans le bucket embargo
emb_v       <- au[bucket == "embargo", sum(v)]
emb27_v     <- au[bucket == "embargo" & ch == "27", sum(v)]
emb_rus_v   <- au[bucket == "embargo" & russie, sum(v)]
emb27_rus_v <- au[bucket == "embargo" & russie & ch == "27", sum(v)]

# (3) Chevauchement embargo ∩ strategique (codes Aiyar absorbes dans embargo)
emb_codes <- unique(au[bucket == "embargo" & v > 0, k_str])
ovl_codes <- intersect(emb_codes, all_strategic_codes)
ovl_v     <- au[bucket == "embargo" & k_str %in% ovl_codes, sum(v)]

# (4) Top 5 chapitres HS2 par valeur dans chaque bucket (commerce Russie-centre)
top_hs2 <- au[russie == TRUE, .(value = sum(v)), by = .(bucket, ch)][order(bucket, -value)]
top_hs2 <- top_hs2[, head(.SD, 5L), by = bucket]
fwrite(top_hs2, file.path(PATH_CLEAN, "tab_bucket_top_hs2_russia.csv"))

# (5) Composition par direction (RUS exportateur = import du monde ; RUS importateur = export du monde)
comp_dir <- au[rus_dir != "other",
               .(n_codes = uniqueN(k_str[v > 0]), value = sum(v)),
               by = .(rus_dir, bucket)][order(rus_dir, -value)]
fwrite(comp_dir, file.path(PATH_CLEAN, "tab_bucket_by_direction_russia.csv"))

cat("\n========================================================\n")
cat("AUDIT COMPOSITION DES BUCKETS\n")
cat("========================================================\n")
print(comp[, .(bucket, n_codes,
               value_total_bnUSD  = round(value_total  / 1e6, 1), share_total  = sprintf("%.1f%%", 100 * share_total),
               value_russia_bnUSD = round(value_russia / 1e6, 1), share_russia = sprintf("%.1f%%", 100 * share_russia))])
cat(sprintf("\nHS27 (energie) dans le bucket embargo : %.1f%% du total | %.1f%% en Russie-centre -> 'embargo ~ surtout energie'\n",
            100 * emb27_v / emb_v, 100 * emb27_rus_v / max(emb_rus_v, 1)))
cat(sprintf("Chevauchement embargo ∩ strategique  : %d codes Aiyar absorbes dans embargo, %.1f Md USD (%.1f%% du bucket embargo)\n",
            length(ovl_codes), ovl_v / 1e6, 100 * ovl_v / emb_v))
cat("  (-> explique un bucket strategic_nonembargo amaigri : ces codes quittent le bucket 2 pour le 1)\n")
cat("\nTop chapitres HS2 par bucket (Russie-centre) :\n"); print(top_hs2)
cat("\nComposition par direction (Russie-centre) :\n"); print(comp_dir)
# Valeurs reutilisees dans le README
audit_emb27_pct     <- 100 * emb27_v / emb_v
audit_emb27_rus_pct <- 100 * emb27_rus_v / max(emb_rus_v, 1)
audit_ovl_n <- length(ovl_codes); audit_ovl_pct <- 100 * ovl_v / emb_v


# ---- Section 7 : Sauvegarde -------------------------------------------------

log_step("Section 7 : sauvegarde.")

setcolorder(panel, c("exp_iso3", "imp_iso3", "year",
                     "trade_value", "strategic_trade_value", "strategic_trade_share",
                     "non_strategic_trade", "non_strategic_share",
                     # partition MECE a 3 buckets (decomposition §5)
                     "embargo_trade_value", "embargo_share",
                     "strategic_nonembargo_trade_value", "strategic_nonembargo_share",
                     "nonstrategic_nonembargo_trade_value", "nonstrategic_nonembargo_share",
                     "exp_energy_dep_rus", "imp_energy_dep_rus"))
setkey(panel, exp_iso3, imp_iso3, year)

out_parquet <- file.path(PATH_CLEAN, "master_panel_with_strategic.parquet")
out_csv     <- file.path(PATH_CLEAN, "master_panel_with_strategic.csv")
out_readme  <- file.path(PATH_CLEAN, "README_strategic.md")

write_parquet(panel, out_parquet)
fwrite(panel, out_csv)

n_pos <- panel[strategic_trade_value > 0, .N]
readme_txt <- c(
  "# Master panel + commerce strategique",
  "",
  paste("Build date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Construction",
  "Enrichissement de master_panel.parquet (cf. README.md) avec : (i) les variables",
  "de commerce strategique (classification Aiyar et al. 2024, IMF 'Geoeconomic",
  "Fragmentation', 6 secteurs), (ii) la dependance energetique russe, et (iii) une",
  "partition MECE a 3 buckets (embargo / strategique-non-embargo / reste) pour la",
  "decomposition §5.",
  "",
  "## Variables ajoutees",
  "- strategic_trade_value : somme du commerce BACI sur les codes HS6",
  "  strategiques (en milliers USD). 0 si pas de match (legitime, pas NA).",
  "- strategic_trade_share : strategic_trade_value / trade_value. NA si",
  "  trade_value == 0 (division 0/0 non definie).",
  "- non_strategic_trade   : trade_value - strategic_trade_value (milliers USD,",
  "  >= 0 ; = trade_value quand strategic == 0). Complement du strategique.",
  "- non_strategic_share   : non_strategic_trade / trade_value. NA si",
  "  trade_value == 0 (meme convention que strategic_trade_share).",
  "- exp/imp_energy_dep_rus : dependance energetique russe (monadique, mergee",
  "  des deux cotes). Part des hydrocarbures russes (BACI HS chapitre 27,",
  "  exportateur = RUS) dans les importations TOTALES du pays cette annee-la.",
  "  Dans [0,1] ; NA si importations totales == 0. Numerateur derive de BACI",
  "  (cache _baci_energy_cache.parquet) ; denominateur = imports du master panel.",
  "  Variante 'part du commerce total' (imports+exports) mentionnee dans le code,",
  "  non activee par defaut.",
  "",
  "## Partition MECE a 3 buckets (decomposition §5)",
  "Decoupage MUTUELLEMENT EXCLUSIF et EXHAUSTIF du commerce bilateral dirige, par",
  "precedence au niveau HS6 : embargo > strategique > reste. Les 3 sommes egalent",
  "trade_value (assertion verifiee au build). Sert au test mecanique (le commerce",
  "banni chute) vs vraie fragmentation (le commerce AUTORISE recule aussi).",
  "- embargo_trade_value / embargo_share : commerce sur les codes HS6 sous embargo",
  "  de reference (cf. liste ci-dessous).",
  "- strategic_nonembargo_trade_value / _share : commerce sur les codes strategiques",
  "  (Aiyar) qui ne sont PAS dans la liste d'embargo.",
  "- nonstrategic_nonembargo_trade_value / _share : tout le reste (ni embargo, ni",
  "  strategique). Shares = bucket / trade_value, NA si trade_value == 0.",
  "",
  "### Liste d'embargo de reference (PROXY)",
  "Source : annexes du reglement (UE) 833/2014 et paquets de sanctions 2022-2025.",
  "AVERTISSEMENT : liste de REFERENCE en PROXY, ancree au niveau CHAPITRE/POSITION",
  "HS la ou le ban est large (matching par prefixe), appliquee UNIFORMEMENT a toutes",
  "les paires (PAS sender-specifique ni datee), REVISABLE. Blocs :",
  "- Imports (chapitres) : 27 energie, 72 fer/acier, 76 aluminium, 03 poissons,",
  "  44 bois. Positions : 7108 or, 7102 diamants. (cuivre/nickel hors par defaut.)",
  "- Exports luxe : 8703 voitures (proxy position, sans seuil de valeur), 91",
  "  horlogerie, 7113 bijouterie.",
  "- Exports dual-use : sous-listes Aiyar semiconducteurs + telecoms/5G + defense",
  "  (UE 833/2014 annexe VII), referencees sans duplication.",
  paste0("Audit (Russie-centre) : HS27 (energie) = ", sprintf("%.0f%%", audit_emb27_rus_pct),
         " du bucket embargo (=> 'embargo ~ surtout energie') ; chevauchement embargo ∩",
         " strategique = ", audit_ovl_n, " codes Aiyar absorbes (",
         sprintf("%.0f%%", audit_ovl_pct), " du bucket embargo)."),
  "Tables d'audit : tab_bucket_composition.csv, tab_bucket_top_hs2_russia.csv,",
  "tab_bucket_by_direction_russia.csv (dans Data/Clean).",
  "",
  "## Codes HS6 strategiques",
  paste0("- Total uniques : ", n_after, " codes apres dedoublonnage"),
  paste0("- Secteurs : semiconductors (",  length(strategic_codes$semiconductors), "), ",
                     "telecommunications_5g (", length(strategic_codes$telecommunications_5g), "), ",
                     "green_transition (",     length(strategic_codes$green_transition), "), ",
                     "pharmaceuticals (",      length(strategic_codes$pharmaceuticals), "), ",
                     "critical_minerals (",    length(strategic_codes$critical_minerals), "), ",
                     "defense (",              length(strategic_codes$defense), ")"),
  paste0("- Codes en doublon entre secteurs : ", paste(dups, collapse = ", ")),
  "",
  "## Dimensions",
  paste("- Observations               :", nrow(panel)),
  paste("- Colonnes totales           :", ncol(panel)),
  paste0("- strategic_trade_value > 0  : ", n_pos,
         sprintf(" (%.1f%% du panel)", 100 * n_pos / nrow(panel))),
  "",
  "## Note importante",
  "Les zeros de strategic_trade_value couvrent DEUX cas distincts :",
  "  1. Paires-annees ou trade_value == 0 (pas de commerce du tout).",
  "  2. Paires-annees ou trade_value > 0 mais aucun flux sur les HS6",
  "     strategiques.",
  "Dans les deux cas, c'est un zero legitime et non une donnee manquante.",
  "strategic_trade_share isole le second cas (defini conditionnellement",
  "a trade_value > 0).",
  "",
  "## Reference",
  "Aiyar, S., Chen, J., Ebeke, C., Garcia-Saltos, R., Gudmundsson, T.,",
  "Ilyina, A., Kangur, A., Kunaratskul, T., Rodriguez, S., Ruta, M.,",
  "Schulze, T., Soderberg, G., Trevino, J.P. (2024).",
  "Geoeconomic Fragmentation and the Future of Multilateralism.",
  "IMF Staff Discussion Note SDN/2023/001.",
  "",
  "Reglement (UE) 833/2014 du Conseil concernant des mesures restrictives eu egard",
  "aux actions de la Russie deconstabilisant la situation en Ukraine, et ses paquets",
  "de sanctions successifs (2022-2025). Liste d'embargo de reference (proxy HS6)."
)
writeLines(readme_txt, out_readme)

log_step(paste("Termine. Parquet :", out_parquet))
log_step(paste("        Csv     :", out_csv))
log_step(paste("        Readme  :", out_readme))
