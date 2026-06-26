# =============================================================================
# 05_build_covariates.R   (etape prealable — covariables / panel d'analyse assemble)
# -----------------------------------------------------------------------------
# POINT D'INTEGRATION du pipeline : assemble UN panel de covariables unique,
# RESTREINT aux dyades Russie-centrees (exp==RUS OU imp==RUS), source de verite
# pour les descriptives (06), la validite/SMD (07) et le conditionnement des
# estimateurs (08/09/11). Script de DONNEES : lit/derive/joint/ecrit, aucune
# estimation, aucun tableau, aucune figure, aucun SMD.
#
# Output  : Data/Clean/covariates_panel.parquet  (PATH_COVARIATES)
# Entrees : master_panel_with_strategic.parquet (commerce, energie, UE, PIB/pop,
#           distance, IPD) ; sanctions_panel.parquet (sanc_partner_to_rus dirige + polyarchy
#           niveau) ; un_votes.parquet (votes du partenaire).
# Chemins / wrappers I/O / helpers : 00_setup.R.
#
# REGLE D'INTEGRATION : on JOINT les covariables deja construites en amont (on
# ne les recalcule pas) ; on NE re-stocke PAS les colonnes brutes de sanctions
# (03) ni de votes (04) — seuls le 2x2 derive et le vote-partenaire sont gardes.
# La RUSSIE est la CIBLE : elle n'est JAMAIS un partenaire condamnateur/
# sanctionneur ; partner_iso3 pointe toujours sur le cote NON-russe.
#
# NB historique : 03 ecrit desormais sanctions_panel.parquet (l'ancien
# iv_panel a ete archive lors de la scission sanctions/IV).
#
# ---------------------------------------------------------------------------
# NOTES DE DESIGN (destination des covariables — a lire avant usage aval) :
#
# (1) ABSORPTION PAR LES FE. La spec PPML de reference porte des FE
#     exportateur-annee + importateur-annee + paire dirigee. Toute covariable
#     monadique pays-annee (PIB, pop, polyarchy, UE, energie cote partenaire) et
#     tout invariant dyadique (distance, exposition pre-2014) y sont MECANIQUEMENT
#     absorbes. Ce panel n'est donc PAS a brancher tel quel comme controles dans
#     le PPML a FE pays-annee. Usages reels : (i) balance/SMD descriptive (07) ;
#     (ii) conditionnement dans les modeles lineaires dCDH et tendances
#     paralleles conditionnelles (c'est la que l'energie mord) ; (iii)
#     interactions d'heterogeneite (energie x post-2022, exposition x post...).
#
# (2) EXPOSITION PRE-2014 = invariante dans le temps -> absorbee par les FE
#     paire. Role : balance, restriction/ponderation du groupe de controle,
#     interactions. Jamais en controle direct dans un modele a FE paire.
#
# (3) ENERGIE = exposition COMMERCIALE, pas dependance de consommation.
#     *_energy_dep_rus = imports d'hydrocarbures russes (HS27) / imports totaux
#     (BACI). Exposition commerciale a l'energie russe, pas une dependance de
#     consommation (un pays peut peu importer mais dependre, ou re-exporter).
#     Defendable ; la version fine (IEA/Eurostat/BP) est un repli non active.
#
# (4) OTAN/UE = DESCRIPTIF de balance, PAS covariable de conditionnement. Quasi
#     colineaires au traitement (sanctions souvent prises au niveau UE) -> "bad
#     control" qui absorberait l'effet ; de plus invariants -> deja absorbes par
#     les FE paire. Gardes uniquement pour documenter le sorting (07) et etiqueter
#     la cellule "coalition occidentale" du 2x2.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "countrycode")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(countrycode)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})  # fournit PATH_*, wrappers I/O, log_step

log_step("Setup termine.")
PRE_WINDOW <- 2008:2013   # fenetre pre-choc Crimee pour l'exposition


# ---- Section 1 : lecture des panels amont -----------------------------------

log_step("Section 1 : lecture master_panel_with_strategic + sanctions_panel + un_votes.")

full <- read_parquet_safe(PATH_STRATEGIC)
cat("  - master strategic (complet) :", nrow(full), "x", ncol(full), "\n")

sanc <- read_parquet_safe(PATH_SANCTIONS_PANEL,
                          col_select = c("exp_iso3", "imp_iso3", "year",
                                         "exp_polyarchy", "imp_polyarchy",
                                         "sanc_partner_to_rus"))
votes <- read_parquet_safe(PATH_UN_VOTES)


# ---- Section 2 : agregats monadiques (panel COMPLET, avant restriction) ------
# partner_total_trade et l'exposition pre-2014 se derivent du panel ENTIER.

log_step("Section 2 : agregats monadiques (commerce total, exposition pre-2014).")

# Commerce total par pays-annee (somme des deux roles : exportateur + importateur)
tot_exp <- full[, .(tr = sum(trade_value, na.rm = TRUE)), by = .(iso3 = exp_iso3, year)]
tot_imp <- full[, .(tr = sum(trade_value, na.rm = TRUE)), by = .(iso3 = imp_iso3, year)]
tot_trade <- rbind(tot_exp, tot_imp)[, .(total_trade = sum(tr)), by = .(iso3, year)]

# Imports totaux par pays-annee (pour la part-import de l'exposition)
imp_tot <- full[, .(imports = sum(trade_value, na.rm = TRUE)),
                by = .(iso3 = imp_iso3, year)]

# Commerce c<->RUS (deux sens) et imports de c DEPUIS la Russie
c_rus <- rbind(
  full[imp_iso3 == "RUS", .(iso3 = exp_iso3, year, v = trade_value)],   # c -> RUS
  full[exp_iso3 == "RUS", .(iso3 = imp_iso3, year, v = trade_value)]    # RUS -> c
)[, .(trade_rus = sum(v, na.rm = TRUE)), by = .(iso3, year)]
c_rus_imp <- full[exp_iso3 == "RUS", .(imp_from_rus = sum(trade_value, na.rm = TRUE)),
                  by = .(iso3 = imp_iso3, year)]   # imports de c depuis RUS

# Parts annuelles puis MOYENNE 2008-2013 (part de RUS dans le commerce de c)
expo <- merge(c_rus, tot_trade, by = c("iso3", "year"), all.x = TRUE)
expo[, share := fifelse(total_trade > 0, trade_rus / total_trade, NA_real_)]
expo_imp <- merge(c_rus_imp, imp_tot, by = c("iso3", "year"), all.x = TRUE)
expo_imp[, share_imp := fifelse(imports > 0, imp_from_rus / imports, NA_real_)]

exposure <- expo[year %in% PRE_WINDOW,
                 .(exposure_rus_pre2014 = mean(share, na.rm = TRUE)), by = iso3]
exposure_imp <- expo_imp[year %in% PRE_WINDOW,
                 .(exposure_rus_pre2014_imp = mean(share_imp, na.rm = TRUE)), by = iso3]
exposure <- merge(exposure, exposure_imp, by = "iso3", all = TRUE)
exposure[is.nan(exposure_rus_pre2014),     exposure_rus_pre2014 := NA_real_]
exposure[is.nan(exposure_rus_pre2014_imp), exposure_rus_pre2014_imp := NA_real_]
cat("  - Exposition pre-2014 : pays couverts :", exposure[!is.na(exposure_rus_pre2014), .N], "\n")


# ---- Section 3 : base Russie-centree + attribution partenaire ----------------

log_step("Section 3 : restriction aux dyades Russie-centrees + attribution partenaire.")

keep_cols <- c("exp_iso3", "imp_iso3", "year",
               "trade_value", "strategic_trade_value", "non_strategic_trade",
               "non_strategic_share", "exp_energy_dep_rus", "imp_energy_dep_rus",
               "ipd", "dist", "exp_nato", "imp_nato", "pair_nato",
               "exp_eu", "imp_eu", "pair_eu",
               "exp_gdp_nominal", "imp_gdp_nominal", "exp_pop", "imp_pop")
base <- full[exp_iso3 == "RUS" | imp_iso3 == "RUS", ..keep_cols]
rm(full); gc(verbose = FALSE)
cat("  - Dyades Russie-centrees :", nrow(base), "\n")

base[, partner_iso3    := fifelse(exp_iso3 == "RUS", imp_iso3, exp_iso3)]
base[, rus_as_exporter := exp_iso3 == "RUS"]
stopifnot(base[partner_iso3 == "RUS", .N] == 0L)   # jamais RUS<->RUS

# Joindre polyarchy (niveau) + sanc_partner_to_rus (flag DIRIGE) depuis sanctions_panel
base <- merge(base, sanc, by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)

# Versions "partner_*" : cote NON-russe (energie, polyarchy, PIB, pop)
pick <- function(exp_col, imp_col) fifelse(base$rus_as_exporter,
                                           base[[imp_col]], base[[exp_col]])
base[, partner_energy_dep_rus := pick("exp_energy_dep_rus", "imp_energy_dep_rus")]
base[, partner_polyarchy      := pick("exp_polyarchy",      "imp_polyarchy")]
base[, partner_gdp            := pick("exp_gdp_nominal",    "imp_gdp_nominal")]
base[, partner_pop            := pick("exp_pop",            "imp_pop")]


# ---- Section 4 : derivees nouvelles (cheap) ---------------------------------

log_step("Section 4 : derivees (commerce total, PIB/tete, region, exposition).")

base <- merge(base, tot_trade[, .(partner_iso3 = iso3, year, partner_total_trade = total_trade)],
              by = c("partner_iso3", "year"), all.x = TRUE)
base[, partner_gdp_pc := fifelse(!is.na(partner_pop) & partner_pop > 0,
                                 partner_gdp / partner_pop, NA_real_)]
# Region/continent du partenaire (pas de colonne region en amont -> countrycode)
reg_map <- data.table(partner_iso3 = unique(base$partner_iso3))
reg_map[, partner_region := countrycode(partner_iso3, "iso3c", "continent", warn = FALSE)]
base <- merge(base, reg_map, by = "partner_iso3", all.x = TRUE)
# Exposition pre-2014 (scalaire par pays) -> mappe sur le partenaire
base <- merge(base, exposure[, .(partner_iso3 = iso3, exposure_rus_pre2014,
                                 exposure_rus_pre2014_imp)],
              by = "partner_iso3", all.x = TRUE)


# ---- Section 5 : vote du partenaire (monadique -> dyadique) ------------------

log_step("Section 5 : vote du partenaire (via partner_iso3).")

vcols <- c("iso3", "vote_2014", "vote_2022", "condemn_2014", "condemn_2022")
if ("align_2022" %in% names(votes)) vcols <- c(vcols, "align_2022")
base <- merge(base, votes[, ..vcols][, partner_iso3 := iso3][, iso3 := NULL],
              by = "partner_iso3", all.x = TRUE)


# ---- Section 6 : le 2x2 condamne x sanctionne (derive de 03 x 04) ------------
# Cote "sanctionne" = flag DIRIGE sanc_partner_to_rus (sender = partenaire,
# target = RUS), expose par 03. La version NON-DIRIGEE (sanction_any) a ete
# ABANDONNEE : elle confondait les sanctionneurs de la Russie avec les Etats que
# la Russie (co-)sanctionne — notamment les cibles de sanctions multilaterales
# ONU (Iran, Coree du Nord, Soudan, Mali, RCA, Guinee-Bissau, Soudan du Sud) et
# des cas regionaux (ARM, AZE, KAZ, KGZ, BLR), qui polluaient a tort la cellule
# (c). Avec le flag dirige, ces pays repartent en (d) et (c) se vide.
#   - time-varying  : sanc_partner_to_rus[year] (le partenaire sanctionne-t-il la
#                     Russie CETTE annee ; NA pour year>2023).
#   - static post-X : le partenaire a-t-il sanctionne la Russie en >= X
#                     (max sur la fenetre year>=X) -> ancre temporelle propre
#                     pour le DiD 2x2. Constant par partenaire.
# La Russie n'apparait dans AUCUNE cellule (partner_iso3 != "RUS").

log_step("Section 6 : construction du 2x2 dirige (statique 2014/2022 + dependant de l'annee).")

fin_int <- function(x) fifelse(is.finite(x), as.integer(x), NA_integer_)
s22 <- base[year >= 2022L, .(sanctioned_post2022 = fin_int(max(sanc_partner_to_rus, na.rm = TRUE))),
            by = partner_iso3]
s14 <- base[year >= 2014L, .(sanctioned_post2014 = fin_int(max(sanc_partner_to_rus, na.rm = TRUE))),
            by = partner_iso3]
base <- merge(base, s22, by = "partner_iso3", all.x = TRUE)
base <- merge(base, s14, by = "partner_iso3", all.x = TRUE)

CELL_LV <- c("a_both", "b_condemn_only", "c_sanction_only", "d_neither")
mk_cell <- function(condemn, sanc) factor(fcase(
  condemn == 1L & sanc == 1L, "a_both",
  condemn == 1L & sanc == 0L, "b_condemn_only",
  condemn == 0L & sanc == 1L, "c_sanction_only",
  condemn == 0L & sanc == 0L, "d_neither",
  default = NA_character_), levels = CELL_LV)

base[, cell_2022_static := mk_cell(condemn_2022, sanctioned_post2022)]
base[, cell_2014_static := mk_cell(condemn_2014, sanctioned_post2014)]
base[, cell_2022_t      := mk_cell(condemn_2022, sanc_partner_to_rus)]
base[, cell_2014_t      := mk_cell(condemn_2014, sanc_partner_to_rus)]


# ---- Section 7 : selection finale (pas de re-stockage des bruts sanctions) ---

log_step("Section 7 : selection des colonnes du panel de covariables.")

out_cols <- c(
  # cles + attribution
  "exp_iso3", "imp_iso3", "year", "partner_iso3", "rus_as_exporter",
  # commerce
  "trade_value", "strategic_trade_value", "non_strategic_trade", "non_strategic_share",
  "partner_total_trade",
  # energie
  "exp_energy_dep_rus", "imp_energy_dep_rus", "partner_energy_dep_rus",
  # regime
  "exp_polyarchy", "imp_polyarchy", "partner_polyarchy",
  # taille
  "exp_gdp_nominal", "imp_gdp_nominal", "partner_gdp", "partner_gdp_pc",
  "exp_pop", "imp_pop", "partner_pop",
  # geo / alignement
  "dist", "ipd", "partner_region",
  # exposition pre-2014 (invariante)
  "exposure_rus_pre2014", "exposure_rus_pre2014_imp",
  # UE / OTAN (descriptifs de balance, PAS conditionnement)
  "exp_eu", "imp_eu", "pair_eu", "pair_nato",
  # vote du partenaire (pas le bloc sanctions brut)
  "vote_2014", "vote_2022", "condemn_2014", "condemn_2022",
  # 2x2 derive
  "sanc_partner_to_rus",
  "sanctioned_post2014", "sanctioned_post2022",
  "cell_2014_static", "cell_2022_static", "cell_2014_t", "cell_2022_t"
)
if ("align_2022" %in% names(base)) out_cols <- c(out_cols, "align_2022")
cov <- base[, ..out_cols]
setkey(cov, exp_iso3, imp_iso3, year)


# ---- Section 8 : validation (le script se verifie seul) ---------------------

log_step("Section 8 : validation (bornes, 2x2, couverture, dimensions).")

# Bornes [0,1]
for (v in c("exposure_rus_pre2014", "exposure_rus_pre2014_imp",
            "partner_energy_dep_rus", "exp_energy_dep_rus", "imp_energy_dep_rus",
            "partner_polyarchy", "exp_polyarchy", "imp_polyarchy")) {
  bad <- cov[!is.na(get(v)) & (get(v) < 0 | get(v) > 1), .N]
  cat(sprintf("  - %-26s hors [0,1] : %d\n", v, bad))
  stopifnot(bad == 0L)
}
# PIB/tete & commerce total : jamais negatifs
stopifnot(cov[!is.na(partner_gdp_pc) & partner_gdp_pc < 0, .N] == 0L)
stopifnot(cov[!is.na(partner_total_trade) & partner_total_trade < 0, .N] == 0L)

# Russie jamais dans une cellule + dyades bien Russie-centrees uniquement
stopifnot(cov[partner_iso3 == "RUS", .N] == 0L)
stopifnot(cov[exp_iso3 != "RUS" & imp_iso3 != "RUS", .N] == 0L)

# Effectifs des cellules 2x2 statiques (nombre de PARTENAIRES distincts)
cell_counts <- function(col) {
  d <- unique(cov[!is.na(get(col)), .(partner_iso3, cell = get(col))])
  d[, .N, by = cell][order(cell)]
}
cat("\n  2x2 statique 2022 (partenaires distincts par cellule) :\n")
print(cell_counts("cell_2022_static"))
cat("  2x2 statique 2014 (partenaires distincts par cellule) :\n")
print(cell_counts("cell_2014_static"))

# Couverture non-NA des nouvelles variables
cat("\n  Couverture non-NA (sur lignes du panel) :\n")
for (v in c("partner_total_trade", "partner_gdp_pc", "partner_region",
            "exposure_rus_pre2014", "exposure_rus_pre2014_imp",
            "partner_polyarchy", "condemn_2022", "cell_2022_static"))
  cat(sprintf("    %-26s : %.1f%%\n", v, 100 * mean(!is.na(cov[[v]]))))

# Partenaires non apparies (deux sens d'interet)
partners <- sort(unique(cov$partner_iso3))
no_vote  <- sort(setdiff(partners, votes$iso3))
no_expo  <- sort(cov[is.na(exposure_rus_pre2014), unique(partner_iso3)])
no_poly  <- sort(cov[is.na(partner_polyarchy),    unique(partner_iso3)])
cat("\n  - Partenaires SANS vote ONU (", length(no_vote), ") :", paste(no_vote, collapse=" "), "\n")
cat("  - Partenaires SANS exposition pre-2014 (", length(no_expo), ") :", paste(no_expo, collapse=" "), "\n")
cat("  - Partenaires SANS polyarchy (any year) (", length(no_poly), ") :", paste(no_poly, collapse=" "), "\n")


# ---- Section 9 : sauvegarde -------------------------------------------------

log_step("Section 9 : sauvegarde.")
write_parquet_safe(cov, PATH_COVARIATES)
cat("  - Ecrit :", PATH_COVARIATES, "\n")
cat("  -", nrow(cov), "lignes,", ncol(cov), "colonnes (",
    uniqueN(cov$partner_iso3), "partenaires,",
    min(cov$year), "-", max(cov$year), ")\n")

log_step("Termine.")
