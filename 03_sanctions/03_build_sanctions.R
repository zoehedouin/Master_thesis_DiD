# =============================================================================
# 03_build_sanctions.R   (etape prealable — constructeur de traitement, ACTIF)
# -----------------------------------------------------------------------------
# Constructeur PUR des VARIABLES DE TRAITEMENT du design DiD (sanctions/votes
# ONU). A partir de GSDB v4 il derive :
#   * sanctions binaires      : sanction_any / sanction_trade / sanction_nontrade
#   * sanctions par TYPE      : commercial complet/partiel, financier, armes,
#                               voyage, militaire, autre (replique col. 2 GSDB-R4)
#   * doses d'intensite       : sanc_n_active_core / sanc_n_active_all
#                               (-> paliers 0/1/2-5/6+ pour l'AVSQ dCDH)
#   * n_common_sanctioners    : nb de coalitions tierces sanctionnant i ET j
# Conserve la covariable de regime en NIVEAU : exp/imp_polyarchy (V-Dem v16).
# Le 2x2 condamne x sanctionne se construit ensuite en croisant ces sanctions
# avec les votes ONU (cf. 04_build_un_votes.R).
#
# Output : Data/Clean/sanctions_panel.parquet  (panel de traitement ACTIF, lu
#          par 04/05/08/09). Squelette gravite + sanctions + niveaux polyarchy.
#
# Famille "sanctions"           (source : GSDB v4 dyadic, 1950-2023)
#   - sanction_any             = 1 si sanction active entre i et j (undirected)
#   - sanction_trade           = 1 si sanction de TYPE trade en vigueur
#                                 (! tautologique en gravity : coupe le commerce)
#   - sanction_nontrade        = 1 si sanction non-trade (financial, travel,
#                                 arms, military, other). Mesure "propre".
#   - n_common_sanctioners     = nb. de coalitions tierces sanctionnant i ET j
#
# NB historique : ce script s'appelait 06_build_geopol_measures.R puis
# 03_build_treatments.R. Les familles d'INSTRUMENTS IV (distances V-Dem/DPI/
# Polity + relations ATOP/MID) ont ete EXTRAITES dans un script LEGACY archive :
#   _archive/iv_legacy/build_iv_panel.R  (hors pipeline actif).
# Ce script-ci ne produit plus que le panel de sanctions actif.
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
})  # chemins, wrappers I/O NFD-safe, log_step/tic/toc, YEAR_*

# Alias locaux pour ne pas toucher le reste du script :
PATH_RAW   <- PATH_IV         # sources brutes des familles (Data/Raw/IV)
PATH_PANEL <- PATH_STRATEGIC  # frame de reference = master_panel_with_strategic

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


# --- V-Dem est lue pour la covariable de regime active exp/imp_polyarchy
# --- (NIVEAU, Section 8). GSDB (sanctions) est deja en ISO3.
# ---- Section 2 : V-Dem v16 (niveau polyarchy) -------------------------------

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

# Flag DIRIGE (additif) : ensemble des (sender, year) ou le sender a sanctionne
# la RUSSIE (target == "RUS"). Derive de la table DIRIGEE sanc_dir AVANT toute
# symetrisation ; GSDB dyadic a deja expanse les sanctions UE en Etats membres
# senders. Sert a etiqueter en aval "le PARTENAIRE sanctionne la Russie"
# (cf. Section 8 -> colonne sanc_partner_to_rus). N'altere AUCUNE colonne
# symetrisee existante.
dir_to_rus <- unique(sanc_dir[target == "RUS", .(sender, year)])
dir_to_rus[, flag := 1L]
cat("  - Sanctions DIRIGEES partenaire->RUS (sender-years) :", nrow(dir_to_rus),
    "| senders distincts :", uniqueN(dir_to_rus$sender), "\n")

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




# ---- Section 8 : Panel par paire-annee --------------------------------------

log_step("Section 8 : assemblage panel sanctions (paire-annee).")

# Squelette : on part du panel master (deja en memoire), avec ses colonnes
# trade_value, ipd, rta, dist, contig, comlang_off, colony. sanc_panel sera
# self-contained pour faciliter les diagnostics downstream.
panel_pairs <- copy(panel)
setnames(panel_pairs, c("exp_iso3", "imp_iso3"), c("iso_a", "iso_b"))
panel_pairs[, log_dist := log(dist)]
cat("  - Squelette pairs-years (master_panel) :", nrow(panel_pairs), "\n")

sanc_panel <- copy(panel_pairs)
rm(panel, panel_pairs); gc(verbose = FALSE)

# Merger V-Dem (exp puis imp) : NIVEAU exp/imp_polyarchy = covariable de regime
# ACTIVE du DiD. (Les DISTANCES IV sont reconstruites ailleurs, cf. le script
# legacy _archive/iv_legacy/build_iv_panel.R.)
vdem_exp <- vdem[, .(iso_a = iso3, year, vdem_a = v2x_polyarchy)]
vdem_imp <- vdem[, .(iso_b = iso3, year, vdem_b = v2x_polyarchy)]
sanc_panel <- merge(sanc_panel, vdem_exp, by = c("iso_a", "year"), all.x = TRUE)
sanc_panel <- merge(sanc_panel, vdem_imp, by = c("iso_b", "year"), all.x = TRUE)
sanc_panel[, exp_polyarchy := vdem_a]   # niveau v2x_polyarchy [0,1], cote exporter
sanc_panel[, imp_polyarchy := vdem_b]   # niveau v2x_polyarchy [0,1], cote importer
sanc_panel[, c("vdem_a", "vdem_b") := NULL]


# Sanctions undirected (any/trade/nontrade)
sanc_panel <- merge(sanc_panel, sanc_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
sanc_panel[is.na(sanction_any)      & year <= 2023L, sanction_any      := 0L]
sanc_panel[is.na(sanction_trade)    & year <= 2023L, sanction_trade    := 0L]
sanc_panel[is.na(sanction_nontrade) & year <= 2023L, sanction_nontrade := 0L]
sanc_panel[year > 2023L, c("sanction_any", "sanction_trade",
                          "sanction_nontrade") := NA_integer_]

# Indicatrices par type (undirected) + score de canal sanc_n_types
sanc_panel <- merge(sanc_panel, sanc_type_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
for (v in type_cols) sanc_panel[is.na(get(v)) & year <= 2023L, (v) := 0L]
sanc_panel[year > 2023L, (type_cols) := NA_integer_]
sanc_panel[, sanc_n_types := sanc_arms + sanc_military + sanc_financial +
           sanc_travel + sanc_other + sanc_trade_complete + sanc_trade_partial]

# Doses d'intensite (undirected)
sanc_panel <- merge(sanc_panel, dose_und,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
sanc_panel[is.na(sanc_n_active_all)  & year <= 2023L, sanc_n_active_all  := 0L]
sanc_panel[is.na(sanc_n_active_core) & year <= 2023L, sanc_n_active_core := 0L]
sanc_panel[year > 2023L, c("sanc_n_active_all", "sanc_n_active_core") := NA_integer_]

# Common sanctioners (count)
sanc_panel <- merge(sanc_panel, common_snc,
                  by = c("iso_a", "iso_b", "year"), all.x = TRUE)
sanc_panel[is.na(n_common_sanctioners) & year <= 2023L,
         n_common_sanctioners := 0L]
sanc_panel[year > 2023L, n_common_sanctioners := NA_integer_]

# Flag DIRIGE additif : sanc_partner_to_rus = 1 si le cote NON-RUS de la paire
# est sender d'une sanction CIBLANT la Russie cette annee. Label de PARTENAIRE
# (identique sur les deux sens de trade de la paire RUS-partenaire) ; 0 pour les
# dyades sans RUS. NE depend PAS de sanction_any (non-dirige) : corrige le 2x2
# qui confondait sanctionneurs et cibles co-sanctionnees par la Russie.
sanc_panel[, .ptn := fifelse(iso_a == "RUS", iso_b,
                    fifelse(iso_b == "RUS", iso_a, NA_character_))]
sanc_panel <- merge(sanc_panel,
                    dir_to_rus[, .(.ptn = sender, year, sanc_partner_to_rus = flag)],
                    by = c(".ptn", "year"), all.x = TRUE)
sanc_panel[is.na(sanc_partner_to_rus), sanc_partner_to_rus := 0L]
sanc_panel[!is.na(.ptn) & year > 2023L, sanc_partner_to_rus := NA_integer_]
sanc_panel[, .ptn := NULL]


# ---- Section 9 : Diagnostics ------------------------------------------------

log_step("Section 9 : diagnostics couverture.")

setnames(sanc_panel, c("iso_a", "iso_b"), c("exp_iso3", "imp_iso3"))

cat("\n=== Panel sanctions (actif) ===\n")
cat("Lignes (paires-annees)              :", nrow(sanc_panel), "\n")
cat("Annees                              :",
    min(sanc_panel$year), "-", max(sanc_panel$year), "\n\n")

cat("Couverture par variable (non-NA en % du panel) :\n")
cov_vars <- c("exp_polyarchy", "imp_polyarchy",
              "sanction_any", "sanction_trade", "sanction_nontrade",
              "n_common_sanctioners")
for (v in cov_vars) {
  pct <- 100 * mean(!is.na(sanc_panel[[v]]))
  cat(sprintf("  %-22s : %.1f%%\n", v, pct))
}
cat("\nStats sanctions (sur obs avec sanction_any non-NA) :\n")
snc <- sanc_panel[!is.na(sanction_any)]
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

cat("\nNiveau polyarchy (covariable de regime active) :\n")
cat(sprintf("  exp_polyarchy non-NA : %.1f%%  | imp_polyarchy non-NA : %.1f%%\n",
            100 * mean(!is.na(sanc_panel$exp_polyarchy)),
            100 * mean(!is.na(sanc_panel$imp_polyarchy))))


# ---- Section 10 : Sauvegarde -----------------------------------------------

log_step("Section 10 : sauvegarde.")

out <- PATH_SANCTIONS_PANEL
write_parquet_safe(sanc_panel, out)
cat("  - Ecrit :", out, "\n")
cat("  -", nrow(sanc_panel), "lignes,", ncol(sanc_panel), "colonnes\n")

log_step("Termine.")
