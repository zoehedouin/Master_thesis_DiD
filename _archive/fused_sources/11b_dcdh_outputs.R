# =============================================================================
# 11b_dcdh_outputs.R  -- post-traitement de l'objet dCDH (script 11)
# Construit la table event-study + la figure, sans relancer l'estimation.
# =============================================================================
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
PATH_ROOT <- "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis"
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "EventStudy")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "EventStudy")

m <- readRDS(file.path(PATH_TAB, "_dcdh_tier_obj.rds"))
eff <- as.data.table(m$results$Effects,   keep.rownames = "term")
pla <- as.data.table(m$results$Placebos,  keep.rownames = "term")
ate <- as.data.table(m$results$ATE,       keep.rownames = "term")
setnames(eff, c("LB CI","UB CI"), c("lb","ub")); setnames(pla, c("LB CI","UB CI"), c("lb","ub"))
setnames(ate, c("LB CI","UB CI"), c("lb","ub"))

eff[, rel := as.integer(sub("Effect_","",term))]
pla[, rel := -as.integer(sub("Placebo_","",term))]
ref <- data.table(term="Ref_0", Estimate=0, SE=NA, lb=NA, ub=NA, rel=0)

es <- rbind(pla[, .(term, rel, Estimate, SE, lb, ub)],
            ref[, .(term, rel, Estimate, SE, lb, ub)],
            eff[, .(term, rel, Estimate, SE, lb, ub)])[order(rel)]
fwrite(es,  file.path(PATH_TAB, "tab_dcdh_eventstudy.csv"))
fwrite(ate, file.path(PATH_TAB, "tab_dcdh_ate.csv"))
cat("=== event-study table ===\n"); print(es)
cat("\n=== ATE ===\n"); print(ate)

p <- ggplot(es, aes(rel, Estimate)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = 0.5, lty = 3, color = "grey60") +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.15, color = "#B2182B", na.rm = TRUE) +
  geom_line(color = "#B2182B", linewidth = 0.5, na.rm = TRUE) +
  geom_point(size = 2.4, color = "#B2182B", na.rm = TRUE) +
  scale_x_continuous(breaks = min(es$rel):max(es$rel)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=10, color="grey40"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA)) +
  labs(title = "Intensite des sanctions (dose en paliers) et commerce bilateral",
       subtitle = "de Chaisemartin & D'Haultfoeuille. log(trade+1). Cluster paire. Placebos = pre-tendances.",
       x = "Temps relatif au 1er changement de palier (annees)",
       y = "Effet sur log(commerce)",
       caption = "Dose = sanc_n_active_core en paliers 0/1/2-5/6+. Controles jamais-traites echantillonnes (8 Go RAM).")
ggsave(file.path(PATH_FIG, "es_fig02_dcdh_intensity.png"), p, width = 10, height = 6, dpi = 300)
cat("\nFigure ecrite : es_fig02_dcdh_intensity.png\n")
