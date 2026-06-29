# =============================================================================
# 08_figures.R — FIGURES additionnelles section 08_ols (monde OLS/log intensite).
# -----------------------------------------------------------------------------
# FIGURES UNIQUEMENT : lit des CSV deja produits, ecrit des PNG. AUCUNE
# reestimation (pas de did_multiplegt_dyn / dist_lag_het / feols), aucun chiffre
# fabrique -- toutes les valeurs proviennent des tables sources.
#   FIG 1 : gradient dose-reponse par palier (tab_dcdh_by_tier.csv, lignes ATE).
#   FIG 2 : escalade des sanctions russes par type (tab_russia_cases_by_type.csv).
# es_fig02_dcdh_tiers.png (dynamique onset vs escalade) N'EST PAS regeneree :
# complementaire du gradient transversal de la FIG 1 (cf. note dans les rapports).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (meme mecanisme que 08_distlag.R)
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))
})
PART <- "08_ols"
PATH_TAB <- out_tab("EventStudy")
PATH_FIG <- out_fig("EventStudy")
# Locale UTF-8 : sans ca, le device PNG mange les accents (env. en locale C).
suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

theme_08 <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
RED <- "#B2182B"; BLU <- "#2166AC"

# ---- FIGURE 1 : gradient dose-reponse par palier (ATE par modele) -----------
log_step("FIG 1 : gradient dose-reponse par palier (tab_dcdh_by_tier.csv).")
bt <- fread(file.path(PATH_TAB, "tab_dcdh_by_tier.csv"))
ate <- bt[term == "ATE", .(model, estimate, lb, ub)]
LAB1 <- c(normalized_per_tier = "Par cran de dose (normalisé)",
          cross_ge1 = "Franchir core>=1 (~2014, onset)",
          cross_ge2 = "Franchir core>=2 (intermédiaire)",
          cross_ge6 = "Franchir core>=6 (2022, escalade lourde)")
ord  <- c("normalized_per_tier", "cross_ge1", "cross_ge2", "cross_ge6")  # intensite croissante
ate  <- ate[match(ord, model)]
ate[, lbl := factor(LAB1[model], levels = rev(LAB1[ord]))]   # per-dose en haut, ge6 en bas
ate[, sig := !(lb < 0 & ub > 0)]                             # IC exclut 0 ?
ate[, lbl_ns := fifelse(sig, "", "n.s.")]
cat("  - ATE lus :", paste(sprintf("%s=%.3f%s", ate$model, ate$estimate, fifelse(ate$sig, "", " (n.s.)")),
                           collapse = " | "), "\n")
ratio <- abs(ate[model == "cross_ge6", estimate]) / abs(ate[model == "cross_ge1", estimate])

p1 <- ggplot(ate, aes(estimate, lbl)) +
  geom_vline(xintercept = 0, lty = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = lb, xmax = ub, color = sig), height = 0.18, linewidth = 0.7) +
  geom_point(aes(color = sig, shape = sig), size = 3.4, fill = "white", stroke = 1.1) +
  geom_text(aes(label = lbl_ns), vjust = -1.1, size = 3, color = "grey45") +
  scale_color_manual(values = c(`TRUE` = RED, `FALSE` = "grey55"), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
  scale_x_continuous(breaks = seq(-0.8, 0.2, 0.1)) +
  labs(title = "Effet des sanctions par palier d'intensité : le gradient dose-réponse",
       subtitle = sprintf("ATE par seuil de dose (IC 95%%). Le palier lourd core>=6 (2022) ~ %.1fx l'onset core>=1 (2014).", ratio),
       x = "Effet sur log(commerce) (ATE)", y = NULL,
       caption = "Source : dCDH AVSQ (de Chaisemartin & D'Haultfoeuille), log(trade+1), cluster paire ; 147 paires au palier 6+. Point creux gris = non significatif.") +
  theme_08
ggsave(file.path(PATH_FIG, "es_fig_dose_gradient.png"), p1, width = 10, height = 6, dpi = 300)
cat("  - ecrit es_fig_dose_gradient.png\n")

# ---- FIGURE 2 : escalade des sanctions russes par type (2008-2023) ----------
log_step("FIG 2 : escalade des sanctions russes par type (tab_russia_cases_by_type.csv).")
rc <- fread(file.path(PATH_TAB, "tab_russia_cases_by_type.csv"))
# Types principaux (lignes superposees). NB : trade_complete reste ~0-1 (note).
LAB2 <- c(n_arms = "Armes", n_military = "Militaire", n_financial = "Financier",
          n_travel = "Voyage", n_trade = "Commercial")
long <- melt(rc, id.vars = "year", measure.vars = names(LAB2),
             variable.name = "type", value.name = "n")
long[, type := factor(LAB2[as.character(type)], levels = LAB2)]
# Annotation factuelle : trade_complete max sur la periode (lu dans le CSV).
tc_max <- max(rc$n_trade_compl, na.rm = TRUE)
cat(sprintf("  - 2021->2022 (financier) : %d -> %d cas | trade_complete max = %d sur 2008-2023\n",
            rc[year == 2021, n_financial], rc[year == 2022, n_financial], tc_max))
pal5 <- c(Armes = "#B2182B", Militaire = "#EF8A62", Financier = "#2166AC",
          Voyage = "#67A9CF", Commercial = "#1B7837")

p2 <- ggplot(long, aes(year, n, color = type)) +
  geom_vline(xintercept = c(2014, 2022), lty = 3, color = "grey60") +
  annotate("text", x = 2014, y = Inf, label = "Crimée 2014", vjust = 1.4, hjust = 1.05,
           size = 2.8, color = "grey45") +
  annotate("text", x = 2022, y = Inf, label = "Invasion 2022", vjust = 1.4, hjust = 1.05,
           size = 2.8, color = "grey45") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2008, 2023, 2)) +
  scale_color_manual(values = pal5) +
  labs(title = "Russie : intensification des sanctions par type (2008-2023)",
       subtitle = sprintf("Plateau plat 2014-2021 puis bond ~x4-5 en 2022 sur des canaux deja actifs (intensification, pas nouvel onset). Embargo complet (trade_complete) <= %d sur toute la periode.", tc_max),
       x = NULL, y = "Nombre de cas distincts actifs (cible Russie)", color = NULL,
       caption = "Source : GSDB v4 (cas distincts actifs par an, sanctioned_state = Russie). Annees 2012-2013 absentes de la source.") +
  theme_08
ggsave(file.path(PATH_FIG, "es_fig_russia_escalation_by_type.png"), p2, width = 10, height = 6, dpi = 300)
cat("  - ecrit es_fig_russia_escalation_by_type.png\n")
# VARIANTE possible (non produite par defaut) : facettes par type, memes axes
#   ggplot(long, aes(year,n)) + geom_line() + facet_wrap(~type) -- utile si les
#   echelles paraissent trop disparates ; ici elles restent lisibles superposees.

cat("\n=============================================================\n")
cat("RECAP — 08_figures.R (figures uniquement, aucune reestimation)\n")
for (f in c("es_fig_dose_gradient.png", "es_fig_russia_escalation_by_type.png")) {
  p <- file.path(PATH_FIG, f)
  cat(sprintf("  %s : %s\n", f, if (file.exists(p)) paste(round(file.info(p)$size/1024), "Ko") else "ABSENT"))
}
cat("=============================================================\n")
log_step("08_figures.R termine.")
