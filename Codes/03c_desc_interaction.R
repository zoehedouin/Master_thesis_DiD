# =============================================================================
# 03c_desc_interaction.R
# -----------------------------------------------------------------------------
# Descriptives trade x geopolitics x NATO :
#   BLOC 1 (Figs 1-3)  : relation IPD x trade (raw + residualise + strategic)
#   BLOC 2 (Figs 4-6)  : event studies 2022 (Ukraine) et 2018 (Trade War)
#   BLOC 3 (Figs 7-10) : quartiles IPD, NATO x strategic, RTA dynamics
#   BLOC 4 (Fig 11)    : matrice de correlation
#   BLOC 5 (Tabs 1-4)  : transition, NATO summary, before/after 2022, partial corr
# Source : Data/Clean/master_panel_with_strategic.parquet
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "ggplot2", "arrow", "scales",
          "kableExtra", "patchwork", "fixest")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) {
  install.packages(miss, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(arrow)
  library(scales)
  library(kableExtra)
  library(patchwork)
  library(fixest)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_DATA <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "Interaction")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "Interaction")
PATH_MAP  <- file.path(PATH_ROOT, "Output", "Maps",    "Interaction")
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_MAP, recursive = TRUE, showWarnings = FALSE)

log_step <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

theme_memoir <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
theme_set(theme_memoir)

pal_nato <- c("intra" = "#2166AC", "inter" = "#B2182B", "non" = "#969696")

add_events <- function() {
  list(
    annotate("rect", xmin = 2007.5, xmax = 2009.5, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "grey30"),
    geom_vline(xintercept = 2014, lty = 2, alpha = 0.5, color = "grey40"),
    geom_vline(xintercept = 2018, lty = 2, alpha = 0.5, color = "grey40"),
    geom_vline(xintercept = 2020, lty = 2, alpha = 0.5, color = "grey40"),
    geom_vline(xintercept = 2022, lty = 2, alpha = 0.5, color = "grey40"),
    annotate("text", x = 2008.5, y = Inf, label = "GFC",       vjust = 1.5,
             size = 2.5, color = "grey40"),
    annotate("text", x = 2014,   y = Inf, label = "Crimea",    vjust = 1.5,
             hjust = -0.1, size = 2.5, color = "grey40"),
    annotate("text", x = 2018,   y = Inf, label = "Trade\nWar", vjust = 1.5,
             hjust = -0.1, size = 2.5, color = "grey40"),
    annotate("text", x = 2020,   y = Inf, label = "COVID",     vjust = 1.5,
             hjust = -0.1, size = 2.5, color = "grey40"),
    annotate("text", x = 2022,   y = Inf, label = "Ukraine",   vjust = 1.5,
             hjust = -0.1, size = 2.5, color = "grey40")
  )
}

errors <- list()

log_step("Setup termine.")


# ---- Section 1 : Load -------------------------------------------------------

log_step("Section 1 : load panel.")
panel <- as.data.table(read_parquet(PATH_DATA))
cat("  - Obs :", nrow(panel), "  Cols :", ncol(panel), "\n")

# Helper : binned scatter par quantiles
bin_scatter <- function(dt, xvar, yvar, n_bins = 30) {
  d <- copy(dt)
  brks <- quantile(d[[xvar]], probs = seq(0, 1, length.out = n_bins + 1),
                   na.rm = TRUE)
  brks <- unique(brks)
  d[, .bin := cut(get(xvar), breaks = brks, include.lowest = TRUE)]
  d[!is.na(.bin), .(x = mean(get(xvar), na.rm = TRUE),
                    y = mean(get(yvar), na.rm = TRUE),
                    n = .N),
    by = .bin][order(.bin)]
}


# =============================================================================
# BLOC 1 : Relation brute commerce x IPD
# =============================================================================

# ---- Fig 1 : binned scatter raw --------------------------------------------

log_step("Fig 1 : binned scatter raw (IPD vs log trade).")

d1 <- panel[trade_value > 0 & !is.na(ipd),
            .(ipd, log_trade = log(trade_value))]
bs1 <- bin_scatter(d1, "ipd", "log_trade", n_bins = 30)

p1 <- ggplot(bs1, aes(x, y)) +
  geom_point(size = 3, color = "#2166AC") +
  geom_smooth(method = "lm", se = TRUE, color = "#B2182B",
              fill = "#B2182B", alpha = 0.15, size = 0.7) +
  labs(title = "Bilateral Trade and Geopolitical Distance",
       subtitle = "Binned scatter plot, 30 quantile bins of IPD",
       x = "Mean IPD", y = "Mean log(Trade Value)",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig01_binscatter_raw.png"),
       p1, width = 10, height = 6, dpi = 300)


# ---- Fig 2 : binned scatter residualise (FE) -------------------------------

log_step("Fig 2 : binned scatter residualise (peut prendre 1-2 min).")

df_pos <- panel[trade_value > 0 & !is.na(ipd)]
df_pos[, log_trade := log(trade_value)]
df_pos[, exp_year  := paste(exp_iso3, year, sep = "_")]
df_pos[, imp_year  := paste(imp_iso3, year, sep = "_")]
df_pos[, pair      := paste(exp_iso3, imp_iso3, sep = "_")]

res_t <- feols(log_trade ~ 1 | exp_year + imp_year + pair, data = df_pos)
res_i <- feols(ipd       ~ 1 | exp_year + imp_year + pair, data = df_pos)
df_pos[, resid_trade := residuals(res_t)]
df_pos[, resid_ipd   := residuals(res_i)]

bs2 <- bin_scatter(df_pos, "resid_ipd", "resid_trade", n_bins = 30)

# Coefficient et R^2 de la regression residus sur residus
fit_res <- lm(resid_trade ~ resid_ipd, data = df_pos)
coef_res <- coef(fit_res)[2]
r2_res   <- summary(fit_res)$r.squared

ann_txt <- sprintf("Slope = %.3f\nR² = %.4f\nN = %s",
                   coef_res, r2_res, format(nrow(df_pos), big.mark = ","))

p2 <- ggplot(bs2, aes(x, y)) +
  geom_point(size = 3, color = "#2166AC") +
  geom_smooth(method = "lm", se = TRUE, color = "#B2182B",
              fill = "#B2182B", alpha = 0.15, size = 0.7) +
  annotate("label", x = -Inf, y = Inf, label = ann_txt,
           hjust = -0.05, vjust = 1.2, size = 3.2,
           label.size = 0.3, fill = "white") +
  labs(title = "Bilateral Trade and IPD — Within Variation",
       subtitle = "Residualized by exporter-year, importer-year, and pair fixed effects",
       x = "Residualized IPD", y = "Residualized log(Trade)",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig02_binscatter_residualized.png"),
       p2, width = 10, height = 6, dpi = 300)

rm(df_pos, res_t, res_i); gc(verbose = FALSE)


# ---- Fig 3 : binned scatter strategic vs total ----------------------------

log_step("Fig 3 : binned scatter strategic vs total (cote a cote).")

d3 <- panel[strategic_trade_value > 0 & !is.na(ipd),
            .(ipd, log_trade = log(strategic_trade_value))]
bs3 <- bin_scatter(d3, "ipd", "log_trade", n_bins = 30)

mk_bins <- function(dt, title) {
  ggplot(dt, aes(x, y)) +
    geom_point(size = 3, color = "#2166AC") +
    geom_smooth(method = "lm", se = TRUE, color = "#B2182B",
                fill = "#B2182B", alpha = 0.15, size = 0.7) +
    labs(title = title, x = "Mean IPD", y = "Mean log(Trade)")
}

p3 <- (mk_bins(bs1, "(A) Total trade") | mk_bins(bs3, "(B) Strategic trade")) +
  plot_annotation(
    title = "Total vs. Strategic Trade and Geopolitical Distance",
    subtitle = "Binned scatter (30 quantile bins) on positive trade observations",
    caption = "Source: BACI-CEPII, Bailey et al. (2017)",
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )
ggsave(file.path(PATH_FIG, "inter_fig03_binscatter_strategic.png"),
       p3, width = 14, height = 6, dpi = 300)


# =============================================================================
# BLOC 2 : Event studies
# =============================================================================

# ---- Fig 4 : event study 2022 (Ukraine) -----------------------------------

log_step("Fig 4 : event study 2022 (total trade).")

ipd_pre22 <- panel[year %in% 2019:2021 & !is.na(ipd),
                   .(ipd_pre = mean(ipd)),
                   by = .(exp_iso3, imp_iso3)]
med_22 <- median(ipd_pre22$ipd_pre)
ipd_pre22[, group := factor(ifelse(ipd_pre < med_22, "Aligned", "Distant"),
                            levels = c("Aligned", "Distant"))]

event_22 <- merge(panel[year %in% 2017:2024,
                        .(exp_iso3, imp_iso3, year, trade_value,
                          strategic_trade_value)],
                  ipd_pre22[, .(exp_iso3, imp_iso3, group)],
                  by = c("exp_iso3", "imp_iso3"))

evt22_total <- event_22[, .(trade = sum(trade_value, na.rm = TRUE)),
                       by = .(year, group)]
evt22_total[, base := trade[year == 2022], by = group]
evt22_total[, idx := 100 * trade / base]

p4 <- ggplot(evt22_total, aes(year, idx, color = group)) +
  geom_line(size = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 100, lty = 3, color = "grey50") +
  geom_vline(xintercept = 2022, lty = 2, color = "grey40") +
  annotate("text", x = 2022, y = max(evt22_total$idx) * 1.02,
           label = "Ukraine Invasion", hjust = -0.05,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Aligned" = "#2166AC", "Distant" = "#B2182B")) +
  labs(title = "Trade Dynamics Around the 2022 Ukraine Invasion",
       subtitle = "Pairs split by median pre-2022 IPD (2019-2021 mean). Indexed: 2022 = 100",
       x = NULL, y = "Total trade (2022 = 100)", color = "Pair type",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig04_event_study_2022.png"),
       p4, width = 10, height = 6, dpi = 300)


# ---- Fig 5 : event study 2022 (strategic) ---------------------------------

log_step("Fig 5 : event study 2022 (strategic).")

evt22_strat <- event_22[, .(trade = sum(strategic_trade_value, na.rm = TRUE)),
                       by = .(year, group)]
evt22_strat[, base := trade[year == 2022], by = group]
evt22_strat[, idx := 100 * trade / base]

p5 <- ggplot(evt22_strat, aes(year, idx, color = group)) +
  geom_line(size = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 100, lty = 3, color = "grey50") +
  geom_vline(xintercept = 2022, lty = 2, color = "grey40") +
  annotate("text", x = 2022, y = max(evt22_strat$idx) * 1.02,
           label = "Ukraine Invasion", hjust = -0.05,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Aligned" = "#2166AC", "Distant" = "#B2182B")) +
  labs(title = "Strategic Trade Dynamics Around the 2022 Ukraine Invasion",
       subtitle = "Pairs split by median pre-2022 IPD (2019-2021 mean). Indexed: 2022 = 100",
       x = NULL, y = "Strategic trade (2022 = 100)", color = "Pair type",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig05_event_study_2022_strategic.png"),
       p5, width = 10, height = 6, dpi = 300)


# ---- Fig 6 : event study 2018 (Trade War) --------------------------------

log_step("Fig 6 : event study 2018 (Trade War).")

ipd_pre18 <- panel[year %in% 2015:2017 & !is.na(ipd),
                   .(ipd_pre = mean(ipd)),
                   by = .(exp_iso3, imp_iso3)]
med_18 <- median(ipd_pre18$ipd_pre)
ipd_pre18[, group := factor(ifelse(ipd_pre < med_18, "Aligned", "Distant"),
                            levels = c("Aligned", "Distant"))]

event_18 <- merge(panel[year %in% 2014:2024, .(exp_iso3, imp_iso3, year, trade_value)],
                  ipd_pre18[, .(exp_iso3, imp_iso3, group)],
                  by = c("exp_iso3", "imp_iso3"))
evt18 <- event_18[, .(trade = sum(trade_value, na.rm = TRUE)), by = .(year, group)]
evt18[, base := trade[year == 2018], by = group]
evt18[, idx := 100 * trade / base]

p6 <- ggplot(evt18, aes(year, idx, color = group)) +
  geom_line(size = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 100, lty = 3, color = "grey50") +
  geom_vline(xintercept = 2018, lty = 2, color = "grey40") +
  annotate("text", x = 2018, y = max(evt18$idx) * 1.02,
           label = "US-China Trade War", hjust = -0.05,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Aligned" = "#2166AC", "Distant" = "#B2182B")) +
  labs(title = "Trade Dynamics Around the 2018 US-China Trade War",
       subtitle = "Pairs split by median pre-2018 IPD (2015-2017 mean). Indexed: 2018 = 100",
       x = NULL, y = "Total trade (2018 = 100)", color = "Pair type",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig06_event_study_2018.png"),
       p6, width = 10, height = 6, dpi = 300)


# ---- Fig 6b : event study 2018 (strategic) --------------------------------

log_step("Fig 6b : event study 2018 (strategic).")

event_18s <- merge(
  panel[year %in% 2014:2024,
        .(exp_iso3, imp_iso3, year, strategic_trade_value)],
  ipd_pre18[, .(exp_iso3, imp_iso3, group)],
  by = c("exp_iso3", "imp_iso3"))
evt18_strat <- event_18s[, .(trade = sum(strategic_trade_value, na.rm = TRUE)),
                         by = .(year, group)]
evt18_strat[, base := trade[year == 2018], by = group]
evt18_strat[, idx := 100 * trade / base]

p6b <- ggplot(evt18_strat, aes(year, idx, color = group)) +
  geom_line(size = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 100, lty = 3, color = "grey50") +
  geom_vline(xintercept = 2018, lty = 2, color = "grey40") +
  annotate("text", x = 2018, y = max(evt18_strat$idx) * 1.02,
           label = "US-China Trade War", hjust = -0.05,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Aligned" = "#2166AC", "Distant" = "#B2182B")) +
  labs(title = "Strategic Trade Dynamics Around the 2018 US-China Trade War",
       subtitle = "Pairs split by median pre-2018 IPD (2015-2017 mean). Indexed: 2018 = 100",
       x = NULL, y = "Strategic trade (2018 = 100)", color = "Pair type",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig06b_event_study_2018_strategic.png"),
       p6b, width = 10, height = 6, dpi = 300)


# ---- Fig 6c : event study 2014 Crimea (total) -----------------------------

log_step("Fig 6c : event study 2014 (Crimea annexation, total).")

ipd_pre14 <- panel[year %in% 2011:2013 & !is.na(ipd),
                   .(ipd_pre = mean(ipd)),
                   by = .(exp_iso3, imp_iso3)]
med_14 <- median(ipd_pre14$ipd_pre)
ipd_pre14[, group := factor(ifelse(ipd_pre < med_14, "Aligned", "Distant"),
                            levels = c("Aligned", "Distant"))]

# Fenetre 2010-2020 : 4 ans pre, 6 ans post (s'arrete avant Trade War 2018)
event_14 <- merge(
  panel[year %in% 2010:2020,
        .(exp_iso3, imp_iso3, year, trade_value, strategic_trade_value)],
  ipd_pre14[, .(exp_iso3, imp_iso3, group)],
  by = c("exp_iso3", "imp_iso3"))

evt14_total <- event_14[, .(trade = sum(trade_value, na.rm = TRUE)),
                        by = .(year, group)]
evt14_total[, base := trade[year == 2014], by = group]
evt14_total[, idx := 100 * trade / base]

p6c <- ggplot(evt14_total, aes(year, idx, color = group)) +
  geom_line(size = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 100, lty = 3, color = "grey50") +
  geom_vline(xintercept = 2014, lty = 2, color = "grey40") +
  annotate("text", x = 2014, y = max(evt14_total$idx) * 1.02,
           label = "Crimea Annexation", hjust = -0.05,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Aligned" = "#2166AC", "Distant" = "#B2182B")) +
  labs(title = "Trade Dynamics Around the 2014 Annexation of Crimea",
       subtitle = "Pairs split by median pre-2014 IPD (2011-2013 mean). Indexed: 2014 = 100",
       x = NULL, y = "Total trade (2014 = 100)", color = "Pair type",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig06c_event_study_2014_crimea.png"),
       p6c, width = 10, height = 6, dpi = 300)


# ---- Fig 6d : event study 2014 Crimea (strategic) -------------------------

log_step("Fig 6d : event study 2014 (Crimea, strategic).")

evt14_strat <- event_14[, .(trade = sum(strategic_trade_value, na.rm = TRUE)),
                        by = .(year, group)]
evt14_strat[, base := trade[year == 2014], by = group]
evt14_strat[, idx := 100 * trade / base]

p6d <- ggplot(evt14_strat, aes(year, idx, color = group)) +
  geom_line(size = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 100, lty = 3, color = "grey50") +
  geom_vline(xintercept = 2014, lty = 2, color = "grey40") +
  annotate("text", x = 2014, y = max(evt14_strat$idx) * 1.02,
           label = "Crimea Annexation", hjust = -0.05,
           size = 3, color = "grey30") +
  scale_color_manual(values = c("Aligned" = "#2166AC", "Distant" = "#B2182B")) +
  labs(title = "Strategic Trade Dynamics Around the 2014 Annexation of Crimea",
       subtitle = "Pairs split by median pre-2014 IPD (2011-2013 mean). Indexed: 2014 = 100",
       x = NULL, y = "Strategic trade (2014 = 100)", color = "Pair type",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig06d_event_study_2014_crimea_strategic.png"),
       p6d, width = 10, height = 6, dpi = 300)


# =============================================================================
# BLOC 3 : Decompositions croisees
# =============================================================================

# ---- Fig 7 : growth 2019-2023 par quartile d'IPD --------------------------

log_step("Fig 7 : growth 2019-2023 by IPD quartile.")

trade_19 <- panel[year == 2019, .(exp_iso3, imp_iso3, t19 = trade_value)]
trade_23 <- panel[year == 2023, .(exp_iso3, imp_iso3, t23 = trade_value)]
gr <- merge(trade_19, trade_23, by = c("exp_iso3", "imp_iso3"))
gr <- merge(gr, ipd_pre22[, .(exp_iso3, imp_iso3, ipd_pre)],
            by = c("exp_iso3", "imp_iso3"))
gr <- gr[t19 > 0]
gr[, growth_pct := 100 * (t23 / t19 - 1)]
gr[, q := cut(ipd_pre,
              breaks = quantile(ipd_pre, probs = seq(0, 1, 0.25), na.rm = TRUE),
              include.lowest = TRUE,
              labels = c("Q1 (aligned)", "Q2", "Q3", "Q4 (distant)"))]
gr <- gr[!is.na(q)]

p7 <- ggplot(gr, aes(q, growth_pct, fill = q)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85, width = 0.6) +
  scale_fill_manual(values = c("Q1 (aligned)" = "#2166AC", "Q2" = "#92C5DE",
                               "Q3" = "#F4A582", "Q4 (distant)" = "#B2182B"),
                    guide = "none") +
  coord_cartesian(ylim = c(-100, 300)) +
  labs(title = "Trade Growth 2019-2023 by Pre-Invasion IPD Quartile",
       subtitle = "Per-pair growth in trade value, quartiles of 2019-2021 mean IPD (y truncated at [-100, 300]%)",
       x = NULL, y = "Trade growth (%)",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig07_growth_by_ipd_quartile.png"),
       p7, width = 10, height = 6, dpi = 300)


# ---- Fig 8 : strategic share by IPD quartile and period ------------------

log_step("Fig 8 : strategic share by IPD quartile x period.")

# IPD pre par periode
ipd_pre_2010 <- panel[year %in% 2010:2014 & !is.na(ipd),
                      .(ipd_pre = mean(ipd)), by = .(exp_iso3, imp_iso3)]
ipd_pre_2010[, q := cut(ipd_pre,
                        breaks = quantile(ipd_pre, probs = seq(0, 1, 0.25),
                                          na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Q1 (aligned)", "Q2", "Q3", "Q4 (distant)"))]

ipd_pre_2020 <- panel[year %in% 2020:2024 & !is.na(ipd),
                      .(ipd_pre = mean(ipd)), by = .(exp_iso3, imp_iso3)]
ipd_pre_2020[, q := cut(ipd_pre,
                        breaks = quantile(ipd_pre, probs = seq(0, 1, 0.25),
                                          na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Q1 (aligned)", "Q2", "Q3", "Q4 (distant)"))]

# Share strategique moyen par quartile et periode (conditionnel a trade > 0)
shr_2010 <- merge(panel[year %in% 2010:2014 & trade_value > 0 & !is.na(strategic_trade_share),
                        .(exp_iso3, imp_iso3, strategic_trade_share)],
                  ipd_pre_2010[, .(exp_iso3, imp_iso3, q)],
                  by = c("exp_iso3", "imp_iso3"))
shr_2010 <- shr_2010[!is.na(q), .(share = mean(strategic_trade_share),
                                  period = "2010-2014"), by = q]

shr_2020 <- merge(panel[year %in% 2020:2024 & trade_value > 0 & !is.na(strategic_trade_share),
                        .(exp_iso3, imp_iso3, strategic_trade_share)],
                  ipd_pre_2020[, .(exp_iso3, imp_iso3, q)],
                  by = c("exp_iso3", "imp_iso3"))
shr_2020 <- shr_2020[!is.na(q), .(share = mean(strategic_trade_share),
                                  period = "2020-2024"), by = q]

shr <- rbind(shr_2010, shr_2020)

p8 <- ggplot(shr, aes(q, share, fill = period)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("2010-2014" = "#92C5DE", "2020-2024" = "#B2182B")) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(title = "Strategic Trade Share by IPD Quartile and Period",
       subtitle = "Mean of pair-year strategic_trade_share (conditional on positive trade)",
       x = NULL, y = "Mean strategic trade share", fill = "Period",
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "inter_fig08_strategic_share_by_ipd.png"),
       p8, width = 10, height = 6, dpi = 300)


# ---- Fig 9 : decomposition NATO x strategic en 2023 -----------------------

log_step("Fig 9 : trade NATO x strategic 2024.")

d9 <- panel[year == 2024 & !is.na(pair_nato),
            .(strategic     = sum(strategic_trade_value, na.rm = TRUE) / 1e6,
              non_strategic = (sum(trade_value, na.rm = TRUE)
                              - sum(strategic_trade_value, na.rm = TRUE)) / 1e6),
            by = pair_nato]
d9 <- melt(d9, id.vars = "pair_nato", variable.name = "type", value.name = "bn")
d9[, pair_nato := factor(pair_nato, levels = c("intra", "inter", "non"))]
d9[, type := factor(type, levels = c("non_strategic", "strategic"),
                    labels = c("Non-strategic", "Strategic"))]

pal9 <- c("intra-Strategic" = "#2166AC", "intra-Non-strategic" = "#92C5DE",
          "inter-Strategic" = "#B2182B", "inter-Non-strategic" = "#F4A582",
          "non-Strategic"   = "#525252", "non-Non-strategic"   = "#BDBDBD")
d9[, key := paste(pair_nato, type, sep = "-")]

p9 <- ggplot(d9, aes(pair_nato, bn, fill = key)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = pal9, name = NULL,
                    labels = c("intra-Strategic"     = "intra | Strategic",
                               "intra-Non-strategic" = "intra | Non-strategic",
                               "inter-Strategic"     = "inter | Strategic",
                               "inter-Non-strategic" = "inter | Non-strategic",
                               "non-Strategic"       = "non | Strategic",
                               "non-Non-strategic"   = "non | Non-strategic")) +
  scale_y_continuous(labels = comma_format(suffix = " Bn")) +
  labs(title = "Trade Composition by NATO Status and Strategic Content (2024)",
       subtitle = "Stacked exports in billions USD; Strategic (dark) vs Non-strategic (light)",
       x = NULL, y = "Trade (Bn USD)",
       caption = "Source: BACI-CEPII, NATO") +
  theme(legend.text = element_text(size = 8))
ggsave(file.path(PATH_FIG, "inter_fig09_trade_nato_strategic.png"),
       p9, width = 10, height = 6, dpi = 300)


# ---- Fig 10 : RTA share by NATO over time --------------------------------

log_step("Fig 10 : RTA active share by pair_nato over time.")

rta_dyn <- panel[!is.na(pair_nato), .(share_rta = mean(rta)), by = .(year, pair_nato)]
rta_dyn[, pair_nato := factor(pair_nato, levels = c("intra", "inter", "non"))]

p10 <- ggplot(rta_dyn, aes(year, share_rta, color = pair_nato)) +
  geom_line(size = 0.9) + geom_point(size = 1.2) +
  scale_color_manual(values = pal_nato, name = "Pair") +
  scale_y_continuous(labels = label_percent()) +
  add_events() +
  labs(title = "Share of Pairs with Active RTA by NATO Status",
       subtitle = "Fraction of directional pairs with rta = 1 in each year",
       x = NULL, y = "Share of pairs with RTA active",
       caption = "Source: DESTA, NATO")
ggsave(file.path(PATH_FIG, "inter_fig10_rta_by_nato.png"),
       p10, width = 10, height = 6, dpi = 300)


# =============================================================================
# BLOC 4 : Matrice de correlation
# =============================================================================

# ---- Fig 11 : correlation matrix -----------------------------------------

log_step("Fig 11 : correlation matrix.")

vars11 <- c("log_trade", "log_strat", "strategic_trade_share",
            "ipd", "log_dist", "rta",
            "log_exp_gdp", "log_imp_gdp", "exp_nato", "imp_nato")

dc <- panel[trade_value > 0 & strategic_trade_value > 0 &
            !is.na(ipd) & !is.na(dist) &
            !is.na(exp_gdp_real) & !is.na(imp_gdp_real)]
dc[, log_trade   := log(trade_value)]
dc[, log_strat   := log(strategic_trade_value)]
dc[, log_dist    := log(dist)]
dc[, log_exp_gdp := log(exp_gdp_real)]
dc[, log_imp_gdp := log(imp_gdp_real)]
dc <- dc[, ..vars11]
dc <- dc[complete.cases(dc)]
cat("  - N pour correlation matrix :", nrow(dc), "\n")

cmat <- cor(dc)
cor_long <- data.table(
  Var1  = rep(rownames(cmat), times = ncol(cmat)),
  Var2  = rep(colnames(cmat), each  = nrow(cmat)),
  value = as.vector(cmat)
)
cor_long[, Var1 := factor(Var1, levels = vars11)]
cor_long[, Var2 := factor(Var2, levels = rev(vars11))]

p11 <- ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.8) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1), name = "Corr.") +
  coord_equal() +
  labs(title = "Correlation Matrix of Key Variables",
       subtitle = sprintf("Pearson correlations, N = %s pair-year observations",
                          format(nrow(dc), big.mark = ",")),
       x = NULL, y = NULL,
       caption = "Source: BACI-CEPII, CEPII Gravity, World Bank WDI, DESTA, Bailey et al. (2017), NATO") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(PATH_FIG, "inter_fig11_correlation_matrix.png"),
       p11, width = 10, height = 9, dpi = 300)

rm(dc); gc(verbose = FALSE)


# =============================================================================
# BLOC 5 : Tables
# =============================================================================

# ---- Tab 1 : transition matrix commerce x IPD ----------------------------

log_step("Tab 1 : transition matrix IPD x trade quartiles.")

tab1_dt <- panel[trade_value > 0 & !is.na(ipd),
                 .(exp_iso3, imp_iso3, year, ipd, trade_value)]
tab1_dt[, q_ipd := cut(ipd, breaks = quantile(ipd, probs = seq(0, 1, 0.25)),
                       include.lowest = TRUE,
                       labels = c("IPD Q1 (aligned)", "IPD Q2", "IPD Q3",
                                  "IPD Q4 (distant)"))]
tab1_dt[, q_trade := cut(trade_value,
                          breaks = quantile(trade_value, probs = seq(0, 1, 0.25)),
                          include.lowest = TRUE,
                          labels = c("Trade Q1 (low)", "Trade Q2",
                                     "Trade Q3", "Trade Q4 (high)"))]

trans <- tab1_dt[!is.na(q_ipd) & !is.na(q_trade), .N, by = .(q_ipd, q_trade)]
total_n <- sum(trans$N)
trans[, pct := 100 * N / total_n]

trans_wide <- dcast(trans, q_ipd ~ q_trade,
                    value.var = "pct", fill = 0)
trans_wide[, Total := rowSums(.SD), .SDcols = -"q_ipd"]
total_row <- as.list(c("Total", colSums(trans_wide[, -1])))
names(total_row) <- names(trans_wide)
trans_wide <- rbind(trans_wide, as.data.table(total_row))
setnames(trans_wide, "q_ipd", "IPD quartile")

fwrite(trans_wide, file.path(PATH_TAB, "inter_tab01_transition_matrix.csv"))

tex_tab1 <- kbl(trans_wide, format = "latex", booktabs = TRUE, digits = 2,
                caption = "Joint distribution: IPD quartile x Trade-value quartile (\\%)",
                label = "tab:trans_matrix") |>
  kable_styling(latex_options = c("hold_position", "scale_down"))
writeLines(as.character(tex_tab1),
           file.path(PATH_TAB, "inter_tab01_transition_matrix.tex"))


# ---- Tab 2 : NATO x strategic summary ------------------------------------

log_step("Tab 2 : NATO x strategic summary.")

tab2 <- panel[!is.na(pair_nato), .(
  n_obs              = .N,
  trade_mean         = mean(trade_value, na.rm = TRUE),
  strategic_mean     = mean(strategic_trade_value, na.rm = TRUE),
  share_strat_mean   = mean(strategic_trade_share, na.rm = TRUE),
  ipd_mean           = mean(ipd, na.rm = TRUE),
  rta_active_share   = 100 * mean(rta, na.rm = TRUE),
  dist_mean          = mean(dist, na.rm = TRUE)
), by = pair_nato]
tab2 <- tab2[order(factor(pair_nato, levels = c("intra", "inter", "non")))]
setnames(tab2,
         c("pair_nato", "n_obs", "trade_mean", "strategic_mean",
           "share_strat_mean", "ipd_mean", "rta_active_share", "dist_mean"),
         c("NATO pair", "N obs", "Mean trade (k USD)", "Mean strategic (k USD)",
           "Mean strategic share", "Mean IPD",
           "RTA active share (%)", "Mean distance (km)"))

fwrite(tab2, file.path(PATH_TAB, "inter_tab02_nato_strategic_summary.csv"))

tex_tab2 <- kbl(tab2, format = "latex", booktabs = TRUE, digits = 3,
                format.args = list(big.mark = ","),
                caption = "NATO category x strategic content summary (1995-2024)",
                label = "tab:nato_strategic") |>
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex_tab2),
           file.path(PATH_TAB, "inter_tab02_nato_strategic_summary.tex"))


# ---- Tab 3 : before/after 2022 by IPD quartile ---------------------------

log_step("Tab 3 : before/after 2022 by IPD quartile.")

# Reutilise ipd_pre22 et ses quartiles
ipd_pre22[, q := cut(ipd_pre,
                     breaks = quantile(ipd_pre, probs = seq(0, 1, 0.25), na.rm = TRUE),
                     include.lowest = TRUE,
                     labels = c("Q1 (aligned)", "Q2", "Q3", "Q4 (distant)"))]

p_pre  <- merge(panel[year %in% 2019:2021], ipd_pre22[, .(exp_iso3, imp_iso3, q)],
                by = c("exp_iso3", "imp_iso3"))
p_post <- merge(panel[year %in% 2022:2024], ipd_pre22[, .(exp_iso3, imp_iso3, q)],
                by = c("exp_iso3", "imp_iso3"))

agg_pre  <- p_pre [!is.na(q), .(
  trade_pre  = mean(trade_value, na.rm = TRUE),
  strat_pre  = mean(strategic_trade_value, na.rm = TRUE),
  share_pre  = mean(strategic_trade_share, na.rm = TRUE)
), by = q]

agg_post <- p_post[!is.na(q), .(
  trade_post = mean(trade_value, na.rm = TRUE),
  strat_post = mean(strategic_trade_value, na.rm = TRUE),
  share_post = mean(strategic_trade_share, na.rm = TRUE)
), by = q]

tab3 <- merge(agg_pre, agg_post, by = "q")
tab3[, `:=`(
  trade_chg_pct = 100 * (trade_post / trade_pre - 1),
  strat_chg_pct = 100 * (strat_post / strat_pre - 1)
)]
setorder(tab3, q)
tab3_disp <- tab3[, .(
  `IPD quartile`             = q,
  `Trade 2019-21 (k USD)`    = round(trade_pre,  1),
  `Trade 2022-24 (k USD)`    = round(trade_post, 1),
  `Trade change (%)`         = round(trade_chg_pct, 1),
  `Strategic 2019-21 (k USD)` = round(strat_pre,  1),
  `Strategic 2022-24 (k USD)` = round(strat_post, 1),
  `Strategic change (%)`     = round(strat_chg_pct, 1),
  `Strat. share pre`         = round(share_pre,  4),
  `Strat. share post`        = round(share_post, 4)
)]
fwrite(tab3_disp, file.path(PATH_TAB, "inter_tab03_before_after_2022.csv"))

tex_tab3 <- kbl(tab3_disp, format = "latex", booktabs = TRUE,
                format.args = list(big.mark = ","),
                caption = "Trade and strategic trade before vs. after 2022, by pre-invasion IPD quartile",
                label = "tab:before_after_2022") |>
  kable_styling(latex_options = c("hold_position", "scale_down"))
writeLines(as.character(tex_tab3),
           file.path(PATH_TAB, "inter_tab03_before_after_2022.tex"))


# ---- Tab 4 : partial correlations ----------------------------------------

log_step("Tab 4 : partial correlations IPD vs log(trade).")

d4 <- panel[trade_value > 0 & !is.na(ipd) & !is.na(dist) &
            !is.na(exp_gdp_real) & !is.na(imp_gdp_real)]
d4[, log_trade   := log(trade_value)]
d4[, log_dist    := log(dist)]
d4[, log_exp_gdp := log(exp_gdp_real)]
d4[, log_imp_gdp := log(imp_gdp_real)]
d4[, exp_year := paste(exp_iso3, year, sep = "_")]
d4[, imp_year := paste(imp_iso3, year, sep = "_")]
d4[, pair     := paste(exp_iso3, imp_iso3, sep = "_")]

# 1. raw correlation
c1 <- cor(d4$log_trade, d4$ipd)
n1 <- nrow(d4)

# 2. conditional on log(dist), log(gdp_exp), log(gdp_imp)
r_t_2 <- residuals(feols(log_trade ~ log_dist + log_exp_gdp + log_imp_gdp, data = d4))
r_i_2 <- residuals(feols(ipd       ~ log_dist + log_exp_gdp + log_imp_gdp, data = d4))
c2 <- cor(r_t_2, r_i_2)
n2 <- length(r_t_2)

# 3. within-pair (demeaned by pair)
r_t_3 <- residuals(feols(log_trade ~ 1 | pair, data = d4))
r_i_3 <- residuals(feols(ipd       ~ 1 | pair, data = d4))
c3 <- cor(r_t_3, r_i_3)
n3 <- length(r_t_3)

# 4. within-pair-year (full FE)
r_t_4 <- residuals(feols(log_trade ~ 1 | exp_year + imp_year + pair, data = d4))
r_i_4 <- residuals(feols(ipd       ~ 1 | exp_year + imp_year + pair, data = d4))
c4 <- cor(r_t_4, r_i_4)
n4 <- length(r_t_4)

tab4 <- data.table(
  Specification = c("Raw (pooled)",
                    "Conditional on log(dist), log(GDP exp), log(GDP imp)",
                    "Within-pair (demeaned by pair)",
                    "Within-pair-year (exporter-year, importer-year, pair FE)"),
  Correlation   = round(c(c1, c2, c3, c4), 4),
  N             = format(c(n1, n2, n3, n4), big.mark = ",")
)
fwrite(tab4, file.path(PATH_TAB, "inter_tab04_partial_correlations.csv"))

tex_tab4 <- kbl(tab4, format = "latex", booktabs = TRUE,
                caption = "Partial correlations: log(Trade) and IPD",
                label = "tab:partial_corr") |>
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex_tab4),
           file.path(PATH_TAB, "inter_tab04_partial_correlations.tex"))


# ---- Done -------------------------------------------------------------------

log_step("Termine.")

cat("\nFigures :\n"); print(list.files(PATH_FIG))
cat("\nTables :\n");  print(list.files(PATH_TAB))

if (length(errors)) {
  cat("\nERREURS / SKIP :\n")
  for (n in names(errors)) cat(" -", n, ":", errors[[n]], "\n")
} else {
  cat("\nAucune erreur, toutes les figures et tables generees.\n")
}
