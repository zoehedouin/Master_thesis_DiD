# =============================================================================
# 06_descriptives_did.R   (§1 Statistiques descriptives — bloc DiD Russie-centre)
# -----------------------------------------------------------------------------
# MONTRE la structure des donnees et du choc russe (pas d'estimation causale,
# pas d'event study -> 08 ; pas de SMD/verdict de balance -> 07). Implemente le
# "Bloc Russie" de la feuille de route §1 : evolution du commerce avec la Russie
# par statut, calendrier du traitement (onset 2014 vs intensification 2022),
# tableau croise du 2x2, distributions brutes des covariables par groupe.
#
# FRONTIERE AVEC 07 : ici on affiche des moyennes / densites / indices BRUTS,
# sans standardiser et sans conclure. L'ecart standardise (SMD), le seuil
# |SMD|>0.1 et le verdict de balance restent en 07_validity.
#
# RUSSIE = CIBLE : on raisonne en partner_iso3 (le cote NON-russe). Le 2x2 et les
# votes decrivent le PARTENAIRE. 2x2 principal = cell_2022_static.
#
# Output  : 06_descriptives_did/{figures,tables,maps} (sorties prefixees "did_").
# Entrees : covariates_panel.parquet (source principale) ; sanctions_panel.parquet
#           (doses d'intensite sanc_n_active_core) ; un_votes.parquet (decomptes).
# Chemins / wrappers I/O / helpers : 00_setup.R.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "ggplot2", "arrow", "scales", "rnaturalearth",
          "rnaturalearthdata", "sf", "kableExtra", "patchwork")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(arrow); library(scales)
  library(rnaturalearth); library(sf); library(kableExtra); library(patchwork)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})
PART <- "06_descriptives_did"   # co-localisation des sorties (out_fig/tab/map)

log_step("Setup termine.")

PATH_FIG <- out_fig()
PATH_TAB <- out_tab()
PATH_MAP <- out_map()

# Theme + annotations d'evenements (calque du socle 06)
theme_memoir <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
theme_set(theme_memoir)

add_shock <- function() list(
  geom_vline(xintercept = 2014, lty = 2, alpha = 0.5, color = "grey40"),
  geom_vline(xintercept = 2022, lty = 2, alpha = 0.5, color = "grey40"),
  annotate("text", x = 2014, y = Inf, label = "Crimea",  vjust = 1.5, hjust = -0.1,
           size = 2.5, color = "grey40"),
  annotate("text", x = 2022, y = Inf, label = "Invasion", vjust = 1.5, hjust = -0.1,
           size = 2.5, color = "grey40"))

# Palette des cellules du 2x2 + libelles parlants
CELL_LV  <- c("a_both", "b_condemn_only", "c_sanction_only", "d_neither")
CELL_LAB <- c(a_both = "Condemn + sanction", b_condemn_only = "Condemn only",
              c_sanction_only = "Sanction only", d_neither = "Neither")
pal_cell <- c("a_both" = "#2166AC", "b_condemn_only" = "#92C5DE",
              "c_sanction_only" = "#F4A582", "d_neither" = "#969696")

# Collecteur d'erreurs : chaque sortie est isolee dans safely()
errors <- list()
safely <- function(name, expr) {
  out <- tryCatch(eval(expr), error = function(e) e)
  if (inherits(out, "error")) {
    errors[[name]] <<- conditionMessage(out)
    cat("  ** SKIP", name, ":", conditionMessage(out), "\n")
  }
  invisible(out)
}

# Helper d'ecriture de table (.csv + .tex kableExtra)
write_tab <- function(dt, base, digits = 2, caption = "") {
  fwrite(dt, file.path(PATH_TAB, paste0(base, ".csv")))
  tex <- kbl(dt, format = "latex", booktabs = TRUE, digits = digits,
             caption = caption, linesep = "")
  writeLines(as.character(tex), file.path(PATH_TAB, paste0(base, ".tex")))
}


# ---- Section 1 : chargement + agregats par partenaire -----------------------

log_step("Section 1 : chargement des panels + agregats par partenaire.")

cov   <- read_parquet_safe(PATH_COVARIATES)
votes <- read_parquet_safe(PATH_UN_VOTES)
cat("  - covariates_panel :", nrow(cov), "x", ncol(cov),
    "|", uniqueN(cov$partner_iso3), "partenaires,",
    min(cov$year), "-", max(cov$year), "\n")

# Commerce total avec la Russie par (partenaire, annee) : somme des DEUX sens.
trade_py <- cov[, .(trade_rus = sum(trade_value, na.rm = TRUE),
                    strat_rus = sum(strategic_trade_value, na.rm = TRUE)),
                by = .(partner_iso3, year)]

# Onset DERIVE (pas une colonne du panel) : 1ere annee ou le partenaire
# sanctionne la Russie (sanc_partner_to_rus == 1). NA si jamais traite.
onset <- cov[sanc_partner_to_rus == 1L, .(onset_year = min(year)), by = partner_iso3]

# Table PAR PARTENAIRE (une ligne) : cellules (constantes), votes, et covariables
# de reference moyennees sur la fenetre pre-2022 (2018-2021).
REFYR <- 2018:2021
pp <- cov[, .(
  cell_2022   = cell_2022_static[1],
  cell_2014   = cell_2014_static[1],
  sanc_post22 = sanctioned_post2022[1],
  condemn_2022 = condemn_2022[1],
  condemn_2014 = condemn_2014[1],
  exposure    = exposure_rus_pre2014[1],
  energy_ref    = mean(partner_energy_dep_rus[year %in% REFYR], na.rm = TRUE),
  polyarchy_ref = mean(partner_polyarchy[year %in% REFYR],     na.rm = TRUE),
  gdp_pc_ref    = mean(partner_gdp_pc[year %in% REFYR],        na.rm = TRUE),
  region      = partner_region[1]
), by = partner_iso3]
pp <- merge(pp, onset, by = "partner_iso3", all.x = TRUE)
# Poids commercial pre-choc (moyenne 2019-2021 du commerce avec la Russie)
prechoc <- trade_py[year %in% 2019:2021, .(trade_prechoc = mean(trade_rus, na.rm = TRUE)),
                    by = partner_iso3]
pp <- merge(pp, prechoc, by = "partner_iso3", all.x = TRUE)
for (v in c("energy_ref","polyarchy_ref","gdp_pc_ref"))
  pp[is.nan(get(v)), (v) := NA_real_]
pp[, cell_2022_lab := factor(CELL_LAB[as.character(cell_2022)], levels = CELL_LAB)]


# ---- Section 2 : did_fig01 - indice de commerce base 100 par statut ----------

safely("did_fig01", quote({
  log_step("did_fig01 : indice commerce avec la Russie (base 100 = 2013) par statut.")
  idx100 <- function(v, yr) { b <- v[yr == 2013L]; if (length(b) && b > 0) 100 * v / b else NA_real_ }

  # (a) sanctionneur (post-2022) vs non
  ga <- merge(trade_py, pp[, .(partner_iso3, sanc_post22)], by = "partner_iso3")
  ga <- ga[!is.na(sanc_post22), .(trade = sum(trade_rus)), by = .(grp = sanc_post22, year)][order(grp, year)]
  ga[, idx := idx100(trade, year), by = grp]
  ga[, grp := factor(grp, levels = c(1, 0), labels = c("Sanctioner of Russia", "Non-sanctioner"))]
  pa <- ggplot(ga[!is.na(idx)], aes(year, idx, color = grp)) +
    geom_line(linewidth = 0.9) + add_shock() +
    scale_color_manual(values = c("Sanctioner of Russia" = "#B2182B", "Non-sanctioner" = "#2166AC")) +
    labs(title = "(a) Trade with Russia, by sanction status", x = NULL,
         y = "Index (2013 = 100)", color = NULL)

  # (b) par cellule du 2x2
  gb <- merge(trade_py, pp[, .(partner_iso3, cell_2022)], by = "partner_iso3")
  gb <- gb[!is.na(cell_2022), .(trade = sum(trade_rus)), by = .(cell_2022, year)][order(cell_2022, year)]
  gb[, idx := idx100(trade, year), by = cell_2022]
  # Cellule (c) "Sanction only" vide depuis le recodage Option B (KAZ -> (d)) :
  # le niveau inutilise est automatiquement absent de la legende (drop par defaut).
  gb[, cell := factor(CELL_LAB[as.character(cell_2022)], levels = CELL_LAB)]
  pb <- ggplot(gb[!is.na(idx)], aes(year, idx, color = cell)) +
    geom_line(linewidth = 0.9) + add_shock() +
    scale_color_manual(values = setNames(pal_cell, CELL_LAB)) +
    labs(title = "(b) Trade with Russia, by 2x2 cell", x = NULL,
         y = "Index (2013 = 100)", color = NULL)

  p <- pa / pb + plot_annotation(
    title = "Bilateral trade with Russia: dynamics by treatment status",
    subtitle = "Trade summed over both directions, indexed to 2013 = 100",
    caption = "Source: BACI-CEPII, GSDB v4, UN votes. Descriptive (no estimation).")
  ggsave(file.path(PATH_FIG, "did_fig01_trade_index_by_status.png"), p,
         width = 11, height = 9, dpi = 300)
}))


# ---- Section 3 : did_fig02 - calendrier du traitement (onset vs intensite) ---

safely("did_fig02", quote({
  log_step("did_fig02 : calendrier du traitement (onset histogram + intensite).")
  # (i) histogramme des onsets (1er passage sanc_partner_to_rus==1 par partenaire)
  oh <- onset[!is.na(onset_year), .N, by = onset_year][order(onset_year)]
  p_i <- ggplot(oh, aes(onset_year, N)) +
    geom_col(fill = "#B2182B", alpha = 0.85) + add_shock() +
    labs(title = "(i) Sanction onsets against Russia", x = NULL,
         y = "New sanctioning partners")

  # (ii) intensite agregee : sanc_n_active_core sur les dyades RUS (mean treated + max)
  sp <- read_parquet_safe(PATH_SANCTIONS_PANEL,
          col_select = c("exp_iso3", "imp_iso3", "year", "sanc_n_active_core"))
  sp <- sp[exp_iso3 == "RUS" | imp_iso3 == "RUS"]
  sp[, ptn := fifelse(exp_iso3 == "RUS", imp_iso3, exp_iso3)]
  dose_py <- sp[, .(dose = suppressWarnings(as.numeric(max(sanc_n_active_core, na.rm = TRUE)))),
                by = .(ptn, year)]
  dose_py[!is.finite(dose), dose := NA_real_]
  di <- dose_py[, .(mean_treated = mean(dose[dose > 0], na.rm = TRUE),
                    max_dose = max(dose, na.rm = TRUE)), by = year][order(year)]
  di[!is.finite(max_dose), max_dose := NA_real_]
  dim <- melt(di, id.vars = "year", variable.name = "stat", value.name = "n_active")
  dim[, stat := factor(stat, levels = c("max_dose", "mean_treated"),
                       labels = c("Max active cases", "Mean (treated dyads)"))]
  p_ii <- ggplot(dim[!is.na(n_active)], aes(year, n_active, color = stat)) +
    geom_line(linewidth = 0.9) + add_shock() +
    scale_color_manual(values = c("Max active cases" = "#B2182B", "Mean (treated dyads)" = "#2166AC")) +
    labs(title = "(ii) Sanction intensity on Russia (active cases)", x = NULL,
         y = "Active sanction cases (core)", color = NULL)

  p <- p_i / p_ii + plot_annotation(
    title = "Treatment calendar: 2014 is the onset, 2022 is the intensification",
    subtitle = "Few new sanctioning partners in 2022, but a jump in active cases",
    caption = "Source: GSDB v4 (sanc_partner_to_rus, sanc_n_active_core). Descriptive.")
  ggsave(file.path(PATH_FIG, "did_fig02_treatment_calendar.png"), p,
         width = 11, height = 9, dpi = 300)
}))


# ---- Section 4 : did_fig03 - distributions BRUTES des covariables par groupe -

safely("did_fig03", quote({
  log_step("did_fig03 : distributions brutes des covariables par groupe (pas de SMD).")
  d <- pp[, .(partner_iso3, energy_ref, exposure, polyarchy_ref, gdp_pc_ref,
              sanc_post22, condemn_2022)]
  d[, log_gdp_pc := log(gdp_pc_ref)]
  long <- melt(d, id.vars = c("partner_iso3", "sanc_post22", "condemn_2022"),
               measure.vars = c("energy_ref", "exposure", "polyarchy_ref", "log_gdp_pc"),
               variable.name = "covar", value.name = "val")
  long[, covar := factor(covar, levels = c("energy_ref","exposure","polyarchy_ref","log_gdp_pc"),
       labels = c("Energy dep. (RU, HS27)", "Pre-2014 exposure to RU",
                  "Polyarchy (V-Dem)", "log GDP per capita"))]
  # Deux groupements empiles : sanctionneur vs non, condamneur vs non
  l1 <- long[!is.na(sanc_post22), .(covar, val, grp = fifelse(sanc_post22 == 1L, "Sanctioner", "Non-sanctioner"), split = "Sanction")]
  l2 <- long[!is.na(condemn_2022), .(covar, val, grp = fifelse(condemn_2022 == 1L, "Condemner", "Non-condemner"), split = "UN vote 2022")]
  ld <- rbind(l1, l2)[!is.na(val)]
  # Box-plots (pas densites) : bien plus lisibles pour des variables tres
  # asymetriques. scales="free" par facette (x ET y libres) pour que chaque
  # covariable respire. On NE tronque PAS les outliers (on les veut visibles).
  p <- ggplot(ld, aes(x = val, y = grp, fill = grp)) +
    geom_boxplot(outlier.size = 0.6, alpha = 0.7, linewidth = 0.3) +
    facet_wrap(~ split + covar, scales = "free", ncol = 4) +
    scale_fill_manual(values = c("Sanctioner" = "#B2182B", "Non-sanctioner" = "#2166AC",
                                 "Condemner" = "#762A83", "Non-condemner" = "#1B7837")) +
    labs(title = "Raw covariate distributions by group (first look at sorting)",
         subtitle = "Raw distributions (NOT standardized) - formal balance/SMD lives in 07_validity",
         x = NULL, y = NULL, fill = NULL,
         caption = "One partner per observation; reference values averaged 2018-2021.")
  ggsave(file.path(PATH_FIG, "did_fig03_covariate_distributions_by_group.png"), p,
         width = 13, height = 7, dpi = 300)
}))


# ---- Section 5 : did_fig04 - sorting energie x exposition (scatter) ----------

safely("did_fig04", quote({
  log_step("did_fig04 : nuage exposition pre-2014 x dependance energetique (par cellule).")
  d <- pp[!is.na(exposure) & !is.na(energy_ref) & !is.na(cell_2022)]
  # Zoom sur l'amas via coord_cartesian (ne SUPPRIME aucun point, contrairement
  # a xlim/scale_x). Points hors cadre signales en note (pas effaces).
  XMAX <- 0.25; YMAX <- 0.12
  off <- d[exposure > XMAX | energy_ref > YMAX]
  off_txt <- if (nrow(off))
    paste0(off$partner_iso3, " (exp ", round(100 * off$exposure), "%, ener ",
           round(100 * off$energy_ref), "%)", collapse = ", ") else "aucun"
  cap <- paste0("Source: BACI-CEPII. Descriptive sorting view; diagnostic in 07. ",
                "Vue zoomee (coord_cartesian). ", nrow(off),
                " partenaire(s) hors cadre : ", off_txt, ".")
  p <- ggplot(d, aes(exposure, energy_ref, color = cell_2022_lab)) +
    geom_point(size = 2.4, alpha = 0.6,
               position = position_jitter(width = 0.003, height = 0.002, seed = 1L)) +
    scale_color_manual(values = setNames(pal_cell, CELL_LAB)) +
    scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
    coord_cartesian(xlim = c(0, XMAX), ylim = c(0, YMAX)) +
    labs(title = "Sorting at a glance: pre-2014 exposure x Russian energy dependence",
         subtitle = "One point per partner, colored by 2x2 cell (raw - no standardized gap)",
         x = "Pre-2014 trade exposure to Russia", y = "Russian energy dependence (HS27, 2018-2021)",
         color = NULL, caption = cap)
  ggsave(file.path(PATH_FIG, "did_fig04_sorting_energy_exposure.png"), p,
         width = 10, height = 7, dpi = 300)
}))


# ---- Section 6 : did_fig05 - part strategique par cellule (prepare §5) -------

safely("did_fig05", quote({
  log_step("did_fig05 : part strategique du commerce avec la Russie par cellule.")
  ts <- merge(trade_py, pp[, .(partner_iso3, cell_2022)], by = "partner_iso3")
  ts <- ts[!is.na(cell_2022) & trade_rus > 0]
  ts[, period := fifelse(year <= 2021L, "Pre-2022 (2019-2021)", "Post-2022 (2022-2024)")]
  ts <- ts[year >= 2019L]
  agg <- ts[, .(strat_share = sum(strat_rus, na.rm = TRUE) / sum(trade_rus, na.rm = TRUE)),
            by = .(cell_2022, period)]
  # Cellule (c) "Sanction only" vide depuis Option B -> niveau absent des barres.
  agg[, cell := factor(CELL_LAB[as.character(cell_2022)], levels = CELL_LAB)]
  agg[, period := factor(period, levels = c("Pre-2022 (2019-2021)", "Post-2022 (2022-2024)"))]
  p <- ggplot(agg, aes(cell, strat_share, fill = period)) +
    geom_col(position = position_dodge(), width = 0.7) +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = c("Pre-2022 (2019-2021)" = "#92C5DE", "Post-2022 (2022-2024)" = "#B2182B")) +
    labs(title = "Strategic share of trade with Russia, by 2x2 cell",
         subtitle = "Motivates the strategic / non-strategic decomposition (roadmap 5)",
         x = NULL, y = "Strategic share", fill = NULL) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  ggsave(file.path(PATH_FIG, "did_fig05_strategic_share_by_cell.png"), p,
         width = 10, height = 6, dpi = 300)
}))


# ---- Section 7 : did_map01 - carte des cellules du 2x2 ----------------------

safely("did_map01", quote({
  log_step("did_map01 : carte mondiale des cellules du 2x2 (cell_2022_static).")
  world <- ne_countries(scale = 50, returnclass = "sf")
  world$iso3_clean <- ifelse(world$iso_a3 == "-99" | is.na(world$iso_a3),
                             world$adm0_a3, world$iso_a3)
  world <- world[world$iso3_clean != "ATA", ]
  wm <- merge(world, pp[, .(iso3_clean = partner_iso3, cell_2022_lab)],
              by = "iso3_clean", all.x = TRUE)
  rus_sf <- wm[wm$iso3_clean == "RUS", ]
  p <- ggplot(wm) +
    geom_sf(aes(fill = cell_2022_lab), color = "white", size = 0.1) +
    geom_sf(data = rus_sf, fill = "#404040", color = "white", size = 0.1) +
    scale_fill_manual(values = setNames(pal_cell, CELL_LAB), na.value = "grey92", name = NULL) +
    coord_sf(crs = "+proj=robin") +
    labs(title = "Geography of alignment toward Russia (2022)",
         subtitle = "Partners by 2x2 cell (condemn x sanction); Russia in dark grey",
         caption = "Source: UN ES-11/1 vote, GSDB v4. Map: Natural Earth.") +
    theme(panel.grid.major = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank())
  ggsave(file.path(PATH_MAP, "did_map01_cell_2022_world.png"), p,
         width = 12, height = 7, dpi = 300)
}))


# ---- Section 8 : tables -----------------------------------------------------

safely("did_tab01", quote({
  log_step("did_tab01 : couverture du panel Russie-centre.")
  tab <- data.table(
    metric = c("Partenaires uniques", "Annee min", "Annee max", "Paires-annees",
               "% commerce nul (trade_value==0)", "Partenaires energie non-NA",
               "Partenaires exposition non-NA", "Partenaires avec vote ONU 2022"),
    value = c(uniqueN(cov$partner_iso3), min(cov$year), max(cov$year), nrow(cov),
              round(100 * mean(cov$trade_value == 0, na.rm = TRUE), 1),
              pp[!is.na(energy_ref), .N], pp[!is.na(exposure), .N],
              pp[!is.na(condemn_2022), .N]))
  write_tab(tab, "did_tab01_panel_coverage", digits = 1,
            caption = "Couverture du panel Russie-centre")
}))

safely("did_tab02", quote({
  log_step("did_tab02 : tableau croise du 2x2 (n partenaires + poids commercial).")
  mk <- function(cellcol) {
    d <- unique(pp[!is.na(get(cellcol)), .(partner_iso3, cell = get(cellcol), trade_prechoc)])
    t <- d[, .(n_partners = .N, trade_weight = sum(trade_prechoc, na.rm = TRUE)), by = cell][order(cell)]
    t[, trade_share := round(100 * trade_weight / sum(trade_weight), 1)]
    t[, cell := CELL_LAB[as.character(cell)]]
    t[]
  }
  t22 <- mk("cell_2022"); t22[, vintage := "2022 (ES-11/1)"]
  t14 <- mk("cell_2014"); t14[, vintage := "2014 (Res 68/262)"]
  tab <- rbind(t22, t14)[, .(vintage, cell, n_partners, trade_weight, trade_share)]
  write_tab(tab, "did_tab02_crosstab_2x2", digits = 0,
            caption = "2x2 condamne x sanctionne : nb de partenaires et poids commercial (commerce moyen 2019-2021 avec la Russie)")
}))

safely("did_tab03", quote({
  log_step("did_tab03 : partenaires emblematiques par cellule (top 5 poids commercial).")
  d <- pp[!is.na(cell_2022) & !is.na(trade_prechoc)]
  setorder(d, cell_2022, -trade_prechoc)
  top <- d[, head(.SD, 5), by = cell_2022,
           .SDcols = c("partner_iso3", "trade_prechoc")]
  top[, cell := CELL_LAB[as.character(cell_2022)]]
  top[, rank := rowid(cell_2022)]
  tab <- top[, .(cell, rank, partner_iso3, trade_prechoc = round(trade_prechoc))]
  write_tab(tab, "did_tab03_emblematic_by_cell", digits = 0,
            caption = "Partenaires emblematiques par cellule (top 5 par poids commercial pre-choc)")
}))

safely("did_tab04", quote({
  log_step("did_tab04 : decompte des votes ONU (garde-fou vs totaux officiels).")
  vc <- function(col) { t <- as.list(table(factor(votes[[col]], levels = c("yes","no","abstain","absent")))); as.data.table(t) }
  v14 <- vc("vote_2014")[, resolution := "Res 68/262 (2014)"]
  v22 <- vc("vote_2022")[, resolution := "ES-11/1 (2022)"]
  tab <- rbind(v14, v22)[, .(resolution, yes, no, abstain, absent)]
  # Garde-fou : coherence avec les totaux officiels
  ok14 <- all(tab[resolution == "Res 68/262 (2014)", .(yes,no,abstain,absent)] == c(100,11,58,24))
  ok22 <- all(tab[resolution == "ES-11/1 (2022)",   .(yes,no,abstain,absent)] == c(141,5,35,12))
  if (!(ok14 && ok22)) cat("  !! WARNING did_tab04 : decomptes != totaux officiels\n")
  stopifnot(ok14, ok22)
  write_tab(tab, "did_tab04_un_vote_counts", digits = 0,
            caption = "Decompte des votes ONU par categorie (193 membres)")
}))

safely("did_tab05", quote({
  log_step("did_tab05 : matrice de transition vote_2014 x vote_2022.")
  lv <- c("yes","no","abstain","absent")
  tt <- as.data.table(table(vote_2014 = factor(votes$vote_2014, lv),
                            vote_2022 = factor(votes$vote_2022, lv)))
  m <- dcast(tt, vote_2014 ~ vote_2022, value.var = "N")
  setnames(m, "vote_2014", "vote_2014\\vote_2022")
  write_tab(m, "did_tab05_vote_transition_2014_2022", digits = 0,
            caption = "Matrice de transition des votes (lignes 2014 x colonnes 2022), nb de pays")
}))

safely("did_tab06", quote({
  log_step("did_tab06 : commerce avec la Russie par statut x sous-periode.")
  d <- merge(trade_py, pp[, .(partner_iso3, sanc_post22, condemn_2022, cell_2022)], by = "partner_iso3")
  d[, period := fcase(year <= 2013L, "pre-2014", year <= 2021L, "2014-2021", default = "2022-2024")]
  d[, period := factor(period, levels = c("pre-2014", "2014-2021", "2022-2024"))]
  by_status <- function(stcol, lab) {
    x <- d[!is.na(get(stcol)), .(mean_trade = mean(trade_rus, na.rm = TRUE)),
           by = .(status = paste0(lab, ": ", get(stcol)), period)]
    x
  }
  s1 <- by_status("sanc_post22", "sanctioner")
  s2 <- by_status("condemn_2022", "condemner")
  s3 <- d[!is.na(cell_2022), .(mean_trade = mean(trade_rus, na.rm = TRUE)),
          by = .(status = CELL_LAB[as.character(cell_2022)], period)]
  tab <- rbind(s1, s2, s3)
  tab <- dcast(tab, status ~ period, value.var = "mean_trade")
  write_tab(tab, "did_tab06_trade_by_status_periods", digits = 0,
            caption = "Commerce moyen avec la Russie (milliers USD) par statut x sous-periode")
}))


# ---- Section 9 : cloture ----------------------------------------------------

log_step("Section 9 : recapitulatif.")
cat("\n== Figures ==\n"); print(list.files(PATH_FIG, pattern = "^did_"))
cat("\n== Tables ==\n");  print(list.files(PATH_TAB, pattern = "^did_"))
cat("\n== Maps ==\n");    print(list.files(PATH_MAP, pattern = "^did_"))
if (length(errors)) {
  cat("\n== SKIPS (", length(errors), ") ==\n")
  for (n in names(errors)) cat(" -", n, ":", errors[[n]], "\n")
} else cat("\n== Aucun skip. ==\n")
log_step("06_descriptives_did.R termine.")
