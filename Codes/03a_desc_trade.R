# =============================================================================
# 03a_desc_trade.R
# -----------------------------------------------------------------------------
# Statistiques descriptives sur le commerce mondial :
#   - 9 figures (PNG 300 dpi)
#   - 3 tables (TeX + CSV)
# Source : Data/Clean/master_panel_with_strategic.parquet
# Output : Output/Figures/ et Output/Tables/
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "ggplot2", "arrow", "scales",
          "rnaturalearth", "rnaturalearthdata", "sf", "kableExtra")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) {
  install.packages(miss, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(arrow)
  library(scales)
  library(rnaturalearth)
  library(sf)
  library(kableExtra)
})

PATH_ROOT  <- "/Users/zoe/Desktop/Master_thesis"
PATH_DATA  <- file.path(PATH_ROOT, "Data", "Clean", "master_panel_with_strategic.parquet")
PATH_FIG   <- file.path(PATH_ROOT, "Output", "Figures", "Trade")
PATH_TAB   <- file.path(PATH_ROOT, "Output", "Tables",  "Trade")
PATH_MAP   <- file.path(PATH_ROOT, "Output", "Maps",    "Trade")
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_MAP, recursive = TRUE, showWarnings = FALSE)

log_step <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

# Theme global
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

# Sources minimales par figure (definies inline ci-dessous)

log_step("Setup termine.")


# ---- Section 1 : Load + objets reutilisables -------------------------------

log_step("Section 1 : load panel et agreg annuels.")

panel <- as.data.table(read_parquet(PATH_DATA))
cat("  - Obs panel :", nrow(panel), "  Cols :", ncol(panel), "\n")

# 1a. USA GDP deflator -> index base 2015=100 (cumul des taux annuels)
usa_def <- unique(panel[exp_iso3 == "USA" & !is.na(exp_deflator),
                        .(year, rate = exp_deflator)])
setorder(usa_def, year)
usa_def[, idx := NA_real_]
BASE_YR <- 2015L
i_base  <- which(usa_def$year == BASE_YR)
usa_def[i_base, idx := 100]
if (i_base < nrow(usa_def)) {
  for (i in (i_base + 1):nrow(usa_def)) {
    usa_def[i, idx := usa_def[i - 1, idx] * (1 + usa_def[i, rate] / 100)]
  }
}
if (i_base > 1) {
  for (i in (i_base - 1):1) {
    usa_def[i, idx := usa_def[i + 1, idx] / (1 + usa_def[i + 1, rate] / 100)]
  }
}
cat("  - Index deflator USA 1995/2015/2024 :",
    round(usa_def[year == 1995, idx], 1),
    round(usa_def[year == 2015, idx], 1),
    round(usa_def[year == 2024, idx], 1), "\n")

# 1b. GDP unique par pays-annee (cote exp). On prend exp_gdp_nominal car
# chaque pays apparait plusieurs fois mais avec la meme valeur de GDP.
world_gdp <- unique(panel[!is.na(exp_gdp_nominal),
                          .(iso3 = exp_iso3, year, gdp = exp_gdp_nominal)])
world_gdp_yr <- world_gdp[, .(world_gdp_usd = sum(gdp, na.rm = TRUE)), by = year]

# 1c. Agregats annuels mondiaux
world_trade_yr <- panel[, .(
  trade_total     = sum(trade_value,           na.rm = TRUE),
  trade_strategic = sum(strategic_trade_value, na.rm = TRUE)
), by = year][order(year)]

# Merger deflator + GDP mondial pour les figs 1-4
world_trade_yr <- merge(world_trade_yr, usa_def[, .(year, idx)], by = "year")
world_trade_yr <- merge(world_trade_yr, world_gdp_yr, by = "year")
# trade_value est en milliers USD ; on convertit en milliards
world_trade_yr[, trade_total_bn     := trade_total     / 1e6]
world_trade_yr[, trade_strategic_bn := trade_strategic / 1e6]
world_trade_yr[, trade_real_bn      := trade_total_bn / (idx / 100)]
world_trade_yr[, strategic_share    := 100 * trade_strategic / trade_total]
world_trade_yr[, trade_gdp_ratio    := 100 * (trade_total * 1000) / world_gdp_usd]

cat("  - Annees agregees   :", nrow(world_trade_yr), "\n")


# ---- Section 2 : Fig 1 - Commerce mondial nominal vs reel -------------------

log_step("Section 2 : Fig 1 - commerce mondial nominal et reel.")

df1 <- melt(world_trade_yr[, .(year, Nominal = trade_total_bn, `Real (USD 2015)` = trade_real_bn)],
            id.vars = "year", variable.name = "serie", value.name = "value")

p1 <- ggplot(df1, aes(year, value, color = serie)) +
  geom_line(size = 0.9) +
  scale_color_manual(values = c("Nominal" = "#1f77b4", "Real (USD 2015)" = "#d62728")) +
  scale_y_continuous(labels = comma_format(suffix = " Bn")) +
  add_events() +
  labs(title = "World trade: nominal vs real",
       subtitle = "Sum of directional bilateral export flows; real values deflated by US GDP deflator (2015 = 100)",
       x = NULL, y = "Billions USD", color = NULL,
       caption = "Source: BACI-CEPII, World Bank WDI")
ggsave(file.path(PATH_FIG, "fig01_global_trade_timeseries.png"),
       p1, width = 10, height = 6, dpi = 300)


# ---- Section 3 : Fig 2 - Ratio Commerce / PIB mondial -----------------------

log_step("Section 3 : Fig 2 - ratio trade/GDP.")

peak_yr <- world_trade_yr[which.max(trade_gdp_ratio), year]
peak_val <- world_trade_yr[which.max(trade_gdp_ratio), trade_gdp_ratio]

p2 <- ggplot(world_trade_yr, aes(year, trade_gdp_ratio)) +
  geom_line(size = 0.9, color = "#2c3e50") +
  geom_point(data = world_trade_yr[year == peak_yr],
             aes(year, trade_gdp_ratio), color = "#e74c3c", size = 3) +
  annotate("text", x = peak_yr, y = peak_val,
           label = sprintf("Peak %d: %.1f%%", peak_yr, peak_val),
           hjust = -0.1, vjust = -0.5, size = 3.2, color = "#e74c3c") +
  annotate("text", x = 2017, y = min(world_trade_yr$trade_gdp_ratio) + 1,
           label = "Slowbalization", color = "grey30",
           fontface = "italic", size = 3.5) +
  add_events() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Global trade openness",
       subtitle = "Sum of directional bilateral exports / world nominal GDP (current USD)",
       x = NULL, y = "Trade / GDP",
       caption = "Source: BACI-CEPII, World Bank WDI")
ggsave(file.path(PATH_FIG, "fig02_trade_gdp_ratio.png"),
       p2, width = 10, height = 6, dpi = 300)


# ---- Section 4 : Fig 3 - Part du commerce strategique -----------------------

log_step("Section 4 : Fig 3 - part du commerce strategique.")

p3 <- ggplot(world_trade_yr, aes(year, strategic_share)) +
  geom_line(size = 0.9, color = "#c0392b") +
  geom_point(size = 1.5, color = "#c0392b") +
  add_events() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Share of strategic trade in world trade",
       subtitle = "Sectors: semiconductors, telecom, green transition, pharma, critical minerals, defense",
       x = NULL, y = "Share of world exports",
       caption = "Source: BACI-CEPII")
ggsave(file.path(PATH_FIG, "fig03_strategic_share_timeseries.png"),
       p3, width = 10, height = 6, dpi = 300)


# ---- Section 5 : Fig 4 - Stacked area strategique vs non --------------------

log_step("Section 5 : Fig 4 - stacked area strategique/non.")

df4 <- melt(world_trade_yr[, .(year,
                                Strategic     = trade_strategic_bn,
                                `Non-strategic` = trade_total_bn - trade_strategic_bn)],
            id.vars = "year", variable.name = "type", value.name = "value")
df4[, type := factor(type, levels = c("Non-strategic", "Strategic"))]

p4 <- ggplot(df4, aes(year, value, fill = type)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = c("Strategic" = "#E41A1C", "Non-strategic" = "#cccccc")) +
  scale_y_continuous(labels = comma_format(suffix = " Bn")) +
  add_events() +
  labs(title = "World trade: strategic vs non-strategic",
       subtitle = "Sum of directional bilateral export flows, current USD",
       x = NULL, y = "Trade (Bn USD)", fill = NULL,
       caption = "Source: BACI-CEPII")
ggsave(file.path(PATH_FIG, "fig04_trade_strategic_stacked.png"),
       p4, width = 10, height = 6, dpi = 300)


# ---- Section 6 : Fig 5 - Top 15 corridors 2020-2024 -------------------------

log_step("Section 6 : Fig 5 - top 15 corridors 2020-2024.")

# Mode NATO dominant sur la periode (souvent stable, on prend la modalite
# la plus frequente pour cette paire)
pair_mode <- function(x) {
  t <- table(x)
  names(t)[which.max(t)]
}
top15 <- panel[year %in% 2020:2024 & trade_value > 0,
               .(trade_cum_bn = sum(trade_value) / 1e6,
                 pair_nato    = pair_mode(pair_nato)),
               by = .(exp_iso3, imp_iso3)
              ][order(-trade_cum_bn)][1:15]
top15[, label := paste0(exp_iso3, " → ", imp_iso3)]
top15[, label := factor(label, levels = rev(label))]

p5 <- ggplot(top15, aes(label, trade_cum_bn, fill = pair_nato)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = pal_nato) +
  scale_y_continuous(labels = comma_format(suffix = " Bn")) +
  labs(title = "Top 15 trade corridors, 2020-2024",
       subtitle = "Cumulative directional exports (exporter → importer), current USD",
       x = NULL, y = "Cumulative exports (Bn USD)", fill = "NATO pair",
       caption = "Source: BACI-CEPII, NATO")
ggsave(file.path(PATH_FIG, "fig05_top15_corridors.png"),
       p5, width = 10, height = 6, dpi = 300)


# ---- Section 7 : Fig 6 - Carte openness 2020-2024 ---------------------------

log_step("Section 7 : Fig 6 - carte openness mondiale.")

# Exports + imports par pays (depuis les deux cotes)
ex_by <- panel[year %in% 2020:2024,
               .(exp_total = sum(trade_value, na.rm = TRUE)),
               by = .(iso3 = exp_iso3, year)]
im_by <- panel[year %in% 2020:2024,
               .(imp_total = sum(trade_value, na.rm = TRUE)),
               by = .(iso3 = imp_iso3, year)]
gdp_by <- unique(panel[year %in% 2020:2024 & !is.na(exp_gdp_nominal),
                       .(iso3 = exp_iso3, year, gdp = exp_gdp_nominal)])

c_yr <- merge(ex_by, im_by, by = c("iso3", "year"), all = TRUE)
c_yr <- merge(c_yr, gdp_by, by = c("iso3", "year"), all.x = TRUE)
c_yr[is.na(exp_total), exp_total := 0]
c_yr[is.na(imp_total), imp_total := 0]
c_yr[, trade := exp_total + imp_total]

openness <- c_yr[, .(trade_avg = mean(trade), gdp_avg = mean(gdp, na.rm = TRUE)),
                  by = iso3]
# trade en milliers USD, gdp en USD : openness % = 100 * trade*1000 / gdp
openness[, openness_pct := 100 * trade_avg * 1000 / gdp_avg]
openness <- openness[!is.na(openness_pct) & is.finite(openness_pct)]

world <- ne_countries(scale = 50, returnclass = "sf")
world_map <- merge(world, openness, by.x = "iso_a3", by.y = "iso3", all.x = TRUE)
world_map <- world_map[world_map$iso_a3 != "ATA", ]   # exclure Antarctique

p6 <- ggplot(world_map) +
  geom_sf(aes(fill = openness_pct), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "magma", trans = "log10",
                       labels = comma_format(suffix = "%"),
                       na.value = "grey90", name = "Openness") +
  coord_sf(crs = "+proj=robin") +
  labs(title = "Trade openness by country, 2020-2024 average",
       subtitle = "(Total exports + Total imports) / Nominal GDP, log scale",
       caption = "Source: Natural Earth, BACI-CEPII, World Bank WDI") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())
ggsave(file.path(PATH_MAP, "fig06_map_openness.png"),
       p6, width = 12, height = 7, dpi = 300)


# ---- Section 8 : Fig 7 - Commerce par pair_nato (parts) --------------------

log_step("Section 8 : Fig 7 - commerce par pair_nato (%).")

trade_nato_yr <- panel[!is.na(pair_nato),
                       .(trade = sum(trade_value, na.rm = TRUE)),
                       by = .(year, pair_nato)]
trade_nato_yr[, share := trade / sum(trade), by = year]
trade_nato_yr[, pair_nato := factor(pair_nato, levels = c("non", "inter", "intra"))]

p7 <- ggplot(trade_nato_yr, aes(year, share, fill = pair_nato)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = pal_nato) +
  scale_y_continuous(labels = label_percent()) +
  add_events() +
  labs(title = "World trade composition by NATO bloc",
       subtitle = "Relative shares of exports — intra (both NATO), inter (one NATO), non (neither)",
       x = NULL, y = "Share of world exports", fill = "Pair",
       caption = "Source: BACI-CEPII, NATO")
ggsave(file.path(PATH_FIG, "fig07_trade_by_nato.png"),
       p7, width = 10, height = 6, dpi = 300)


# ---- Section 9 : Fig 8 - Commerce strategique par pair_nato (parts) -------

log_step("Section 9 : Fig 8 - strategique par pair_nato (%).")

strat_nato_yr <- panel[!is.na(pair_nato),
                       .(trade = sum(strategic_trade_value, na.rm = TRUE)),
                       by = .(year, pair_nato)]
strat_nato_yr[, share := trade / sum(trade), by = year]
strat_nato_yr[, pair_nato := factor(pair_nato, levels = c("non", "inter", "intra"))]

p8 <- ggplot(strat_nato_yr, aes(year, share, fill = pair_nato)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = pal_nato) +
  scale_y_continuous(labels = label_percent()) +
  add_events() +
  labs(title = "Strategic trade composition by NATO bloc",
       subtitle = "Relative shares of strategic exports — intra (both NATO), inter (one NATO), non (neither)",
       x = NULL, y = "Share of strategic exports", fill = "Pair",
       caption = "Source: BACI-CEPII, NATO")
ggsave(file.path(PATH_FIG, "fig08_strategic_by_nato.png"),
       p8, width = 10, height = 6, dpi = 300)


# ---- Section 10 : Fig 9 - Nombre de paires actives -------------------------

log_step("Section 10 : Fig 9 - nb paires actives.")

active_yr <- panel[trade_value > 0, .(n_active = .N), by = year]

p9 <- ggplot(active_yr, aes(year, n_active)) +
  geom_line(size = 0.9, color = "#16a085") +
  geom_point(size = 1.5, color = "#16a085") +
  scale_y_continuous(labels = comma) +
  add_events() +
  labs(title = "Extensive margin: number of active pairs per year",
       subtitle = "Directional pairs (exporter → importer) with positive trade flow",
       x = NULL, y = "Number of pairs",
       caption = "Source: BACI-CEPII")
ggsave(file.path(PATH_FIG, "fig09_active_pairs.png"),
       p9, width = 10, height = 6, dpi = 300)


# ---- Section 11 : Tab 1 - Summary statistics --------------------------------

log_step("Section 11 : Tab 1 - summary stats.")

sumvars <- c("trade_value", "strategic_trade_value", "strategic_trade_share",
             "ipd", "dist", "exp_gdp_real", "imp_gdp_real",
             "rta", "exp_nato", "imp_nato")

summarize_dt <- function(dt, vars) {
  out <- rbindlist(lapply(vars, function(v) {
    x <- dt[[v]]
    data.table(
      Variable = v,
      N        = sum(!is.na(x)),
      Mean     = mean(x, na.rm = TRUE),
      SD       = sd(x, na.rm = TRUE),
      Min      = min(x, na.rm = TRUE),
      P25      = quantile(x, 0.25, na.rm = TRUE),
      Median   = quantile(x, 0.50, na.rm = TRUE),
      P75      = quantile(x, 0.75, na.rm = TRUE),
      Max      = max(x, na.rm = TRUE)
    )
  }))
  out
}

tab1a <- summarize_dt(panel,                  sumvars)
tab1b <- summarize_dt(panel[trade_value > 0], sumvars)

tab1a[, Panel := "(A) All observations"]
tab1b[, Panel := "(B) trade_value > 0"]
tab1   <- rbind(tab1a, tab1b)
setcolorder(tab1, c("Panel", "Variable"))

fwrite(tab1, file.path(PATH_TAB, "tab01_summary_stats.csv"))

# Format LaTeX : 2 panels separes
fmt_row <- function(x) {
  fmt <- function(v) format(v, big.mark = ",", scientific = FALSE,
                            nsmall = 2, digits = 4)
  data.table(Variable = x$Variable,
             N      = format(x$N, big.mark = ","),
             Mean   = fmt(x$Mean), SD = fmt(x$SD),
             Min    = fmt(x$Min),  P25 = fmt(x$P25),
             Median = fmt(x$Median), P75 = fmt(x$P75), Max = fmt(x$Max))
}

tex1 <- kbl(fmt_row(tab1a), format = "latex", booktabs = TRUE,
            caption = "Summary statistics - Full panel",
            label = "tab:summary_all") |>
  kable_styling(latex_options = c("scale_down")) |>
  pack_rows("Panel A: All observations", 1, nrow(tab1a))
tex2 <- kbl(fmt_row(tab1b), format = "latex", booktabs = TRUE,
            caption = "Summary statistics - Trade > 0",
            label = "tab:summary_pos") |>
  kable_styling(latex_options = c("scale_down")) |>
  pack_rows("Panel B: trade_value > 0", 1, nrow(tab1b))
writeLines(c(as.character(tex1), "", as.character(tex2)),
           file.path(PATH_TAB, "tab01_summary_stats.tex"))


# ---- Section 12 : Tab 2 - Evolution par sous-periode -----------------------

log_step("Section 12 : Tab 2 - evolution par sous-periode.")

panel[, decade := fcase(
  year <= 2004, "1995-2004",
  year <= 2014, "2005-2014",
  default      = "2015-2024"
)]

# Trade/GDP par decade : on agrege puis on calcule le ratio (eviter
# la moyenne d'un ratio bruite)
tab2 <- panel[, .(
  n_active_pairs   = uniqueN(paste(exp_iso3, imp_iso3)[trade_value > 0]),
  trade_total_bn   = sum(trade_value,           na.rm = TRUE) / 1e6,
  strategic_bn     = sum(strategic_trade_value, na.rm = TRUE) / 1e6,
  share_strategic  = 100 * sum(strategic_trade_value, na.rm = TRUE) /
                           sum(trade_value, na.rm = TRUE),
  ipd_mean         = mean(ipd, na.rm = TRUE)
), by = decade][order(decade)]

# trade/GDP : moyenne par annee dans la decade
trade_gdp_dec <- world_trade_yr[, .(year, trade_gdp_ratio)]
trade_gdp_dec[, decade := fcase(year <= 2004, "1995-2004",
                                year <= 2014, "2005-2014",
                                default      = "2015-2024")]
trade_gdp_dec <- trade_gdp_dec[, .(trade_gdp_pct = mean(trade_gdp_ratio)),
                                by = decade]
tab2 <- merge(tab2, trade_gdp_dec, by = "decade")
setcolorder(tab2, c("decade", "n_active_pairs", "trade_total_bn",
                    "strategic_bn", "share_strategic", "trade_gdp_pct",
                    "ipd_mean"))
setnames(tab2,
         c("decade", "n_active_pairs", "trade_total_bn", "strategic_bn",
           "share_strategic", "trade_gdp_pct", "ipd_mean"),
         c("Period", "Active pairs", "Trade (Bn USD)",
           "Strategic (Bn USD)", "Strategic share (%)",
           "Trade/GDP (%)", "Mean IPD"))

fwrite(tab2, file.path(PATH_TAB, "tab02_evolution_decades.csv"))

tex_tab2 <- kbl(tab2, format = "latex", booktabs = TRUE, digits = 2,
                format.args = list(big.mark = ","),
                caption = "World trade evolution by sub-period",
                label = "tab:evolution") |>
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex_tab2),
           file.path(PATH_TAB, "tab02_evolution_decades.tex"))


# ---- Section 13 : Tab 3 - Commerce par categorie NATO ----------------------

log_step("Section 13 : Tab 3 - par pair_nato.")

tab3 <- panel[!is.na(pair_nato), .(
  n_obs              = .N,
  n_pairs            = uniqueN(paste(exp_iso3, imp_iso3)),
  trade_mean         = mean(trade_value, na.rm = TRUE),
  strategic_mean     = mean(strategic_trade_value, na.rm = TRUE),
  share_strat_mean   = mean(strategic_trade_share, na.rm = TRUE),
  ipd_mean           = mean(ipd, na.rm = TRUE),
  dist_mean          = mean(dist, na.rm = TRUE)
), by = pair_nato]
tab3 <- tab3[order(factor(pair_nato, levels = c("intra", "inter", "non")))]
setnames(tab3,
         c("pair_nato", "n_obs", "n_pairs", "trade_mean", "strategic_mean",
           "share_strat_mean", "ipd_mean", "dist_mean"),
         c("NATO pair", "N obs", "N pairs", "Mean trade (k USD)",
           "Mean strategic (k USD)", "Mean strategic share",
           "Mean IPD", "Mean distance (km)"))

fwrite(tab3, file.path(PATH_TAB, "tab03_by_nato.csv"))

tex_tab3 <- kbl(tab3, format = "latex", booktabs = TRUE, digits = 3,
                format.args = list(big.mark = ","),
                caption = "Trade by NATO category (1995-2024 panel)",
                label = "tab:by_nato") |>
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex_tab3),
           file.path(PATH_TAB, "tab03_by_nato.tex"))


# ---- Done -------------------------------------------------------------------

log_step("Termine.")
cat("\nFigures :\n"); print(list.files(PATH_FIG, full.names = FALSE))
cat("\nTables :\n");  print(list.files(PATH_TAB, full.names = FALSE))
