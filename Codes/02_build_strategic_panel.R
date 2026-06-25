# =============================================================================
# 02_build_strategic_panel.R
# -----------------------------------------------------------------------------
# Enrichit master_panel.parquet (produit par build_master_panel.R) avec deux
# variables agregees sur les codes HS6 strategiques (classification Aiyar et
# al. 2024, IMF "Geoeconomic Fragmentation") :
#   - strategic_trade_value : somme du commerce sur les codes HS6 strategiques
#   - strategic_trade_share : ratio strategic / total (NA si trade_value == 0)
# Output : Data/Clean/master_panel_with_strategic.parquet et .csv
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

setDTthreads(0)

PATH_ROOT  <- "/Users/zoe/Desktop/Master_thesis"
PATH_RAW   <- file.path(PATH_ROOT, "Data", "Raw")
PATH_CLEAN <- file.path(PATH_ROOT, "Data", "Clean")
PATH_BACI  <- file.path(PATH_RAW, "BACI_HS92_V202601")

stopifnot(dir.exists(PATH_BACI), dir.exists(PATH_CLEAN))
stopifnot(file.exists(file.path(PATH_CLEAN, "master_panel.parquet")))

log_step <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

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


# ---- Section 2 : Crosswalk pays --------------------------------------------

log_step("Section 2 : crosswalk pays (BACI numerique -> ISO3).")

baci_codes <- fread(file.path(PATH_BACI, "country_codes_V202601.csv"),
                    encoding = "UTF-8")
setnames(baci_codes, c("country_code", "country_name", "iso2", "iso3"))

iso3_map <- baci_codes[!is.na(iso3) & iso3 != "N/A" & nchar(iso3) == 3,
                       .(country_code, iso3)]
cat("  - Pays avec ISO3 valide :", nrow(iso3_map), "\n")


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

# Verif : panel doit garder exactement les memes lignes
stopifnot(nrow(panel) == n0)


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


# ---- Section 7 : Sauvegarde -------------------------------------------------

log_step("Section 7 : sauvegarde.")

setcolorder(panel, c("exp_iso3", "imp_iso3", "year",
                     "trade_value", "strategic_trade_value", "strategic_trade_share"))
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
  "Enrichissement de master_panel.parquet (cf. README.md) avec deux variables",
  "agregees sur le commerce strategique (classification Aiyar et al. 2024, IMF",
  "'Geoeconomic Fragmentation', 6 secteurs).",
  "",
  "## Variables ajoutees",
  "- strategic_trade_value : somme du commerce BACI sur les codes HS6",
  "  strategiques (en milliers USD). 0 si pas de match (legitime, pas NA).",
  "- strategic_trade_share : strategic_trade_value / trade_value. NA si",
  "  trade_value == 0 (division 0/0 non definie).",
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
  "IMF Staff Discussion Note SDN/2023/001."
)
writeLines(readme_txt, out_readme)

log_step(paste("Termine. Parquet :", out_parquet))
log_step(paste("        Csv     :", out_csv))
log_step(paste("        Readme  :", out_readme))
