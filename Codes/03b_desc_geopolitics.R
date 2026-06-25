# =============================================================================
# 03b_desc_geopolitics.R
# -----------------------------------------------------------------------------
# Descriptives geopolitiques :
#   Bloc 1 (Figs 1-6)  : IPD dans le monde
#   Bloc 2 (Figs 7-10) : NATO + IPD x NATO + within-between
#   Bloc 3 (Tabs 1-3)  : summary stats, mean par periode x NATO, movers
# Source : Data/Clean/master_panel_with_strategic.parquet
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "ggplot2", "arrow", "scales",
          "rnaturalearth", "rnaturalearthdata", "sf", "kableExtra",
          "patchwork")
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
  library(patchwork)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_DATA <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "Geopolitics")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "Geopolitics")
PATH_MAP  <- file.path(PATH_ROOT, "Output", "Maps",    "Geopolitics")
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_MAP, recursive = TRUE, showWarnings = FALSE)

log_step <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

# Theme (identique a 03a, fond blanc explicite)
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

# Helper : modal pair_nato par paire
pair_mode <- function(x) {
  t <- table(x)
  names(t)[which.max(t)]
}

# Collecteur d'erreurs (figure skipping)
errors <- list()
safely <- function(name, expr) {
  out <- tryCatch(eval(expr), error = function(e) e)
  if (inherits(out, "error")) {
    errors[[name]] <<- conditionMessage(out)
    cat("  ** SKIP", name, ":", conditionMessage(out), "\n")
  }
  invisible(out)
}

log_step("Setup termine.")


# ---- Section 1 : Load + helpers --------------------------------------------

log_step("Section 1 : load panel.")
panel <- as.data.table(read_parquet(PATH_DATA))
cat("  - Obs :", nrow(panel), "  Cols :", ncol(panel), "\n")

# Sous-ensemble pair-annee : une seule direction (IPD symetrique)
panel_pair <- panel[exp_iso3 < imp_iso3]
cat("  - Pair-years (dedoublonne, exp_iso3 < imp_iso3) :", nrow(panel_pair), "\n")

# Monde sf pour les cartes (avec ISO3 propre)
world <- ne_countries(scale = 50, returnclass = "sf")
world$iso3_clean <- ifelse(world$iso_a3 == "-99" | is.na(world$iso_a3),
                           world$adm0_a3, world$iso_a3)
world <- world[world$iso3_clean != "ATA", ]


# ---- Section 2 : Fig 1 - Global IPD time series ----------------------------

log_step("Section 2 : Fig 1 - global IPD mean (weighted vs unweighted).")

ipd_yr <- panel[!is.na(ipd), .(
  unweighted     = mean(ipd),
  trade_weighted = if (sum(trade_value, na.rm = TRUE) > 0)
                      weighted.mean(ipd, w = trade_value)
                   else NA_real_
), by = year][order(year)]

df1 <- melt(ipd_yr, id.vars = "year", variable.name = "type",
            value.name = "ipd_mean")
df1[, type := factor(type, levels = c("unweighted", "trade_weighted"),
                     labels = c("Unweighted", "Trade-weighted"))]

p1 <- ggplot(df1, aes(year, ipd_mean, color = type)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.2) +
  scale_color_manual(values = c("Unweighted" = "#2c3e50",
                                "Trade-weighted" = "#c0392b")) +
  add_events() +
  labs(title = "Global Average Geopolitical Distance (IPD), 1995-2024",
       subtitle = "Unweighted vs. trade-weighted bilateral IPD",
       x = NULL, y = "Mean |IdealPoint difference|", color = NULL,
       caption = "Source: Bailey, Strezhnev & Voeten (2017), BACI-CEPII")
ggsave(file.path(PATH_FIG, "geop_fig01_ipd_global_mean.png"),
       p1, width = 10, height = 6, dpi = 300)


# ---- Section 3 : Fig 2 - Key pairs facet -----------------------------------

log_step("Section 3 : Fig 2 - IPD time series for selected pairs.")

key_pairs <- data.table(
  a = c("USA", "USA", "CHN", "USA", "USA", "FRA", "USA", "CHN"),
  b = c("CHN", "RUS", "RUS", "IND", "BRA", "DEU", "TUR", "IRN"),
  label = c("USA - CHN", "USA - RUS", "CHN - RUS", "USA - IND",
            "USA - BRA", "FRA - DEU", "USA - TUR", "CHN - IRN")
)
# Canonical pair sort to align with panel_pair (exp_iso3 < imp_iso3)
key_pairs[, `:=`(p1 = pmin(a, b), p2 = pmax(a, b))]

key_dt <- merge(panel_pair[!is.na(ipd), .(exp_iso3, imp_iso3, year, ipd)],
                key_pairs[, .(p1, p2, label)],
                by.x = c("exp_iso3", "imp_iso3"),
                by.y = c("p1", "p2"))
key_dt[, label := factor(label, levels = key_pairs$label)]

p2 <- ggplot(key_dt, aes(year, ipd)) +
  geom_line(size = 0.7, color = "#2c3e50") +
  geom_point(size = 0.8, color = "#2c3e50") +
  facet_wrap(~ label, nrow = 2, scales = "free_y") +
  add_events() +
  labs(title = "Bilateral IPD Evolution for Selected Country Pairs",
       subtitle = "|IdealPoint difference| between exporter and importer",
       x = NULL, y = "IPD",
       caption = "Source: Bailey, Strezhnev & Voeten (2017)")
ggsave(file.path(PATH_FIG, "geop_fig02_ipd_key_pairs.png"),
       p2, width = 14, height = 8, dpi = 300)


# ---- Section 4 : Fig 3 - Heatmap 2000 vs 2024 ------------------------------

log_step("Section 4 : Fig 3 - heatmap IPD 20 countries 2000 vs 2024.")

heatmap_iso <- c("USA", "CHN", "RUS", "IND", "BRA", "DEU", "FRA", "GBR",
                 "JPN", "KOR", "TUR", "SAU", "IRN", "ZAF", "AUS", "CAN",
                 "MEX", "IDN", "EGY", "POL")

# Construire matrice symetrique de 2024 pour le clustering
ipd23 <- panel[year == 2024 & exp_iso3 %in% heatmap_iso &
                 imp_iso3 %in% heatmap_iso & !is.na(ipd),
               .(exp_iso3, imp_iso3, ipd)]
m23 <- dcast(ipd23, exp_iso3 ~ imp_iso3, value.var = "ipd")
rn  <- m23$exp_iso3
m23 <- as.matrix(m23[, -1])
rownames(m23) <- rn
# Diagonal = 0 (IPD a soi-meme = 0). Missing -> mediane (juste pour ordering).
diag_iso <- intersect(rn, colnames(m23))
m23[is.na(m23)] <- median(m23, na.rm = TRUE)
for (k in diag_iso) m23[k, k] <- 0
# hclust sur la dist symetrique
hc <- hclust(as.dist(m23), method = "ward.D2")
ordered_iso <- rownames(m23)[hc$order]

# Donnees pour les deux annees
heat_dt <- panel[year %in% c(2000, 2024) &
                   exp_iso3 %in% heatmap_iso & imp_iso3 %in% heatmap_iso &
                   !is.na(ipd),
                 .(exp_iso3, imp_iso3, year, ipd)]
heat_dt[, exp_iso3 := factor(exp_iso3, levels = ordered_iso)]
heat_dt[, imp_iso3 := factor(imp_iso3, levels = ordered_iso)]
mid_val <- median(heat_dt$ipd, na.rm = TRUE)

make_heatmap <- function(yr) {
  ggplot(heat_dt[year == yr], aes(exp_iso3, imp_iso3, fill = ipd)) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                         midpoint = mid_val, name = "IPD",
                         na.value = "grey90") +
    labs(title = as.character(yr), x = NULL, y = NULL) +
    coord_equal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8))
}

p3 <- (make_heatmap(2000) | make_heatmap(2024)) +
  plot_annotation(
    title = "Bilateral Geopolitical Distance: 2000 vs. 2024",
    subtitle = "Countries ordered by hierarchical clustering on 2024 IPD matrix",
    caption = "Source: Bailey, Strezhnev & Voeten (2017)",
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )
ggsave(file.path(PATH_FIG, "geop_fig03_ipd_heatmap.png"),
       p3, width = 14, height = 7, dpi = 300)


# ---- Section 5 : Fig 4 - Distribution of IPD at 3 dates --------------------

log_step("Section 5 : Fig 4 - distribution IPD 2000/2014/2024.")

df4 <- panel_pair[year %in% c(2000, 2014, 2024) & !is.na(ipd),
                  .(year = factor(year), ipd)]

p4 <- ggplot(df4, aes(ipd, fill = year, color = year)) +
  geom_density(alpha = 0.3, size = 0.7) +
  scale_color_manual(values = c("2000" = "#1f77b4", "2014" = "#ff7f0e",
                                "2024" = "#d62728")) +
  scale_fill_manual(values  = c("2000" = "#1f77b4", "2014" = "#ff7f0e",
                                "2024" = "#d62728")) +
  labs(title = "Distribution of Bilateral IPD",
       subtitle = "Densities across all country pairs, three benchmark years",
       x = "IPD", y = "Density", color = "Year", fill = "Year",
       caption = "Source: Bailey, Strezhnev & Voeten (2017)")
ggsave(file.path(PATH_FIG, "geop_fig04_ipd_distribution.png"),
       p4, width = 10, height = 6, dpi = 300)


# ---- Section 6 : Fig 5 - Map IPD to USA 2024 -------------------------------

log_step("Section 6 : Fig 5 - map IPD to USA in 2024.")

ipd_usa23 <- panel[year == 2024 & exp_iso3 == "USA" & !is.na(ipd),
                   .(iso3 = imp_iso3, ipd_to_usa = ipd)]
wmap5 <- merge(world, ipd_usa23, by.x = "iso3_clean", by.y = "iso3",
               all.x = TRUE)
mid5 <- median(ipd_usa23$ipd_to_usa, na.rm = TRUE)

usa_sf <- wmap5[wmap5$iso3_clean == "USA", ]

p5 <- ggplot(wmap5) +
  geom_sf(aes(fill = ipd_to_usa), color = "white", size = 0.1) +
  geom_sf(data = usa_sf, fill = "#404040", color = "white", size = 0.1) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                       midpoint = mid5, na.value = "grey85",
                       name = "IPD to USA") +
  coord_sf(crs = "+proj=robin") +
  labs(title = "Geopolitical Distance to the United States (2024)",
       subtitle = "Bilateral |IdealPoint difference| with the US (USA shown in dark grey)",
       caption = "Source: Bailey, Strezhnev & Voeten (2017). Map: Natural Earth") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())
ggsave(file.path(PATH_MAP, "geop_fig05_map_ipd_usa_2024.png"),
       p5, width = 12, height = 7, dpi = 300)


# ---- Section 7 : Fig 6 - Map change IPD to USA 2010-2024 -------------------

log_step("Section 7 : Fig 6 - map IPD change to USA 2010-2024.")

ipd_usa10 <- panel[year == 2010 & exp_iso3 == "USA" & !is.na(ipd),
                   .(iso3 = imp_iso3, ipd_2010 = ipd)]
ipd_chg <- merge(ipd_usa10, ipd_usa23, by = "iso3")
ipd_chg[, delta := ipd_to_usa - ipd_2010]

wmap6 <- merge(world, ipd_chg[, .(iso3, delta)],
               by.x = "iso3_clean", by.y = "iso3", all.x = TRUE)
usa_sf6 <- wmap6[wmap6$iso3_clean == "USA", ]

max_abs <- max(abs(ipd_chg$delta), na.rm = TRUE)

p6 <- ggplot(wmap6) +
  geom_sf(aes(fill = delta), color = "white", size = 0.1) +
  geom_sf(data = usa_sf6, fill = "#404040", color = "white", size = 0.1) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                       midpoint = 0,
                       limits = c(-max_abs, max_abs),
                       na.value = "grey85",
                       name = expression(Delta * " IPD to USA")) +
  coord_sf(crs = "+proj=robin") +
  labs(title = "Change in Geopolitical Distance to the US, 2010-2024",
       subtitle = "Blue = moved closer to US, Red = moved further away",
       caption = "Source: Bailey, Strezhnev & Voeten (2017). Map: Natural Earth") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())
ggsave(file.path(PATH_MAP, "geop_fig06_map_ipd_usa_change.png"),
       p6, width = 12, height = 7, dpi = 300)


# ---- Section 8 : Fig 7 - NATO maps 2000 vs 2024 ----------------------------

log_step("Section 8 : Fig 7 - NATO map 2000 vs 2024.")

nato_2000 <- unique(panel[year == 2000 & exp_nato == 1L, exp_iso3])
nato_2024 <- unique(panel[year == 2024 & exp_nato == 1L, exp_iso3])
new_members <- setdiff(nato_2024, nato_2000)

cat("  - Membres NATO en 2000 :", length(nato_2000), "\n")
cat("  - Membres NATO en 2024 :", length(nato_2024), "\n")
cat("  - Nouveaux 2001-2024   :", length(new_members),
    "->", paste(sort(new_members), collapse = ", "), "\n")

w7 <- world
w7$nato_2000 <- factor(
  ifelse(w7$iso3_clean %in% nato_2000, "NATO", "Non-NATO"),
  levels = c("Non-NATO", "NATO"))
w7$nato_2024 <- factor(
  ifelse(w7$iso3_clean %in% nato_2000,   "NATO (since 2000)",
  ifelse(w7$iso3_clean %in% new_members, "Joined 2001-2024", "Non-NATO")),
  levels = c("Non-NATO", "Joined 2001-2024", "NATO (since 2000)"))

p7a <- ggplot(w7) +
  geom_sf(aes(fill = nato_2000), color = "white", size = 0.1) +
  scale_fill_manual(values = c("Non-NATO" = "#E5E5E5",
                               "NATO"     = "#2166AC"),
                    name = NULL) +
  coord_sf(crs = "+proj=robin") +
  labs(title = "2000") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())

p7b <- ggplot(w7) +
  geom_sf(aes(fill = nato_2024), color = "white", size = 0.1) +
  scale_fill_manual(values = c("Non-NATO"          = "#E5E5E5",
                               "Joined 2001-2024"  = "#92C5DE",
                               "NATO (since 2000)" = "#2166AC"),
                    name = NULL) +
  coord_sf(crs = "+proj=robin") +
  labs(title = "2024") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())

p7 <- (p7a | p7b) +
  plot_annotation(
    title = "NATO Membership: 2000 vs. 2024",
    caption = "Source: NATO. Map: Natural Earth",
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )
ggsave(file.path(PATH_MAP, "geop_fig07_map_nato.png"),
       p7, width = 14, height = 6, dpi = 300)


# ---- Section 9 : Fig 8 - NATO members count over time ----------------------

log_step("Section 9 : Fig 8 - NATO members count.")

nato_count <- panel[exp_nato == 1L, .(n_nato = uniqueN(exp_iso3)), by = year]
setorder(nato_count, year)

enlargement_yrs <- c(1999, 2004, 2009, 2017, 2020, 2023, 2024)
enlarge_labels  <- c("CZE/HUN/POL", "BGR/EST/LVA/LTU/ROU/SVK/SVN",
                     "ALB/HRV", "MNE", "MKD", "FIN", "SWE")

p8 <- ggplot(nato_count, aes(year, n_nato)) +
  geom_col(fill = "#2166AC", alpha = 0.85) +
  geom_text(aes(label = n_nato), vjust = -0.4, size = 3, color = "grey20") +
  scale_y_continuous(limits = c(0, max(nato_count$n_nato) + 3),
                     expand = c(0, 0)) +
  annotate("text",
           x = enlargement_yrs,
           y = nato_count[match(enlargement_yrs, year), n_nato] + 2.5,
           label = enlarge_labels, angle = 90, hjust = 0,
           size = 2.7, color = "#B2182B") +
  labs(title = "NATO Membership Over Time",
       subtitle = "Number of member states, 1995-2024",
       x = NULL, y = "Number of NATO members",
       caption = "Source: NATO")
ggsave(file.path(PATH_FIG, "geop_fig08_nato_members_count.png"),
       p8, width = 10, height = 6, dpi = 300)


# ---- Section 10 : Fig 9 - Mean IPD by NATO pair type ----------------------

log_step("Section 10 : Fig 9 - mean IPD by pair_nato.")

ipd_nato_yr <- panel_pair[!is.na(ipd) & !is.na(pair_nato),
                          .(ipd_mean = mean(ipd), n = .N),
                          by = .(year, pair_nato)]
ipd_nato_yr[, pair_nato := factor(pair_nato, levels = c("intra", "inter", "non"))]

p9 <- ggplot(ipd_nato_yr, aes(year, ipd_mean, color = pair_nato)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.2) +
  scale_color_manual(values = pal_nato, name = "Pair") +
  add_events() +
  labs(title = "Average IPD by NATO Pair Type",
       subtitle = "intra (both NATO), inter (one NATO), non (neither)",
       x = NULL, y = "Mean IPD",
       caption = "Source: Bailey, Strezhnev & Voeten (2017), NATO")
ggsave(file.path(PATH_FIG, "geop_fig09_ipd_by_nato.png"),
       p9, width = 10, height = 6, dpi = 300)


# ---- Section 11 : Fig 10 - Between vs within IPD ---------------------------

log_step("Section 11 : Fig 10 - between-within IPD variation.")

pair_stats <- panel_pair[!is.na(ipd), .(
  n_yrs           = .N,
  ipd_between     = mean(ipd),
  ipd_within_sd   = sd(ipd),
  pair_nato_modal = pair_mode(pair_nato[!is.na(pair_nato)])
), by = .(exp_iso3, imp_iso3)]
pair_stats <- pair_stats[n_yrs >= 10 & !is.na(ipd_within_sd)]
pair_stats[, pair_nato_modal := factor(pair_nato_modal,
                                       levels = c("intra", "inter", "non"))]

cat("  - Paires retenues (>=10 ans) :", nrow(pair_stats), "\n")

p10 <- ggplot(pair_stats, aes(ipd_between, ipd_within_sd)) +
  geom_point(aes(color = pair_nato_modal), alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", color = "black", se = FALSE,
              size = 0.6, lty = 2) +
  scale_color_manual(values = pal_nato, name = "Pair (modal)") +
  labs(title = "Between vs. Within Variation of IPD by Country Pair",
       subtitle = "Within variation identifies the PPML coefficient under pair fixed effects",
       x = "Between-pair mean IPD", y = "Within-pair SD of IPD",
       caption = "Source: Bailey, Strezhnev & Voeten (2017), NATO")
ggsave(file.path(PATH_FIG, "geop_fig10_ipd_within_between.png"),
       p10, width = 10, height = 6, dpi = 300)


# ---- Section 12 : Tab 1 - IPD summary statistics ---------------------------

log_step("Section 12 : Tab 1 - IPD summary stats.")

summarize_ipd <- function(dt) {
  vars <- list(ipd = dt$ipd, `ipd^2` = dt$ipd^2)
  out <- rbindlist(lapply(names(vars), function(v) {
    x <- vars[[v]]
    data.table(
      Variable = v,
      N        = sum(!is.na(x)),
      Mean     = mean(x, na.rm = TRUE),
      SD       = sd(x, na.rm = TRUE),
      Min      = min(x, na.rm = TRUE),
      P10      = quantile(x, 0.10, na.rm = TRUE),
      Median   = quantile(x, 0.50, na.rm = TRUE),
      P90      = quantile(x, 0.90, na.rm = TRUE),
      Max      = max(x, na.rm = TRUE)
    )
  }))
  # Ligne supplementaire : within-pair SD moyenne (calcul sur ipd uniquement)
  wp <- dt[!is.na(ipd), .(sd = sd(ipd)), by = .(exp_iso3, imp_iso3)
          ][!is.na(sd), mean(sd)]
  out <- rbind(out, data.table(
    Variable = "Within-pair SD of IPD",
    N = NA, Mean = wp, SD = NA, Min = NA, P10 = NA,
    Median = NA, P90 = NA, Max = NA
  ), fill = TRUE)
  out
}

tab1a <- summarize_ipd(panel_pair)
tab1b <- summarize_ipd(panel_pair[trade_value > 0])
tab1a[, Panel := "(A) All pair-years"]
tab1b[, Panel := "(B) Pair-years with trade > 0"]
tab1 <- rbind(tab1a, tab1b)
setcolorder(tab1, c("Panel", "Variable"))
fwrite(tab1, file.path(PATH_TAB, "geop_tab01_ipd_summary.csv"))

fmt <- function(v) {
  ifelse(is.na(v), "",
         format(v, big.mark = ",", scientific = FALSE,
                nsmall = 3, digits = 4))
}
fmt_table <- function(x) {
  data.table(Variable = x$Variable,
             N      = ifelse(is.na(x$N), "", format(x$N, big.mark = ",")),
             Mean   = fmt(x$Mean), SD = fmt(x$SD),
             Min    = fmt(x$Min),  P10 = fmt(x$P10),
             Median = fmt(x$Median), P90 = fmt(x$P90), Max = fmt(x$Max))
}

tex1 <- kbl(fmt_table(tab1a), format = "latex", booktabs = TRUE,
            caption = "IPD summary statistics - All pair-years",
            label = "tab:ipd_summary_all") |>
  kable_styling(latex_options = c("scale_down")) |>
  pack_rows("Panel A: All pair-years", 1, nrow(tab1a))
tex2 <- kbl(fmt_table(tab1b), format = "latex", booktabs = TRUE,
            caption = "IPD summary statistics - trade > 0",
            label = "tab:ipd_summary_pos") |>
  kable_styling(latex_options = c("scale_down")) |>
  pack_rows("Panel B: Pair-years with trade > 0", 1, nrow(tab1b))
writeLines(c(as.character(tex1), "", as.character(tex2)),
           file.path(PATH_TAB, "geop_tab01_ipd_summary.tex"))


# ---- Section 13 : Tab 2 - Mean IPD by period and NATO category -----------

log_step("Section 13 : Tab 2 - IPD by period x NATO.")

panel_pair[, period := fcase(
  year <= 2004, "1995-2004",
  year <= 2014, "2005-2014",
  default      = "2015-2024"
)]

tab2_all  <- panel_pair[!is.na(ipd),
                        .(all = mean(ipd)), by = period]
tab2_by   <- dcast(panel_pair[!is.na(ipd) & !is.na(pair_nato),
                              .(m = mean(ipd)), by = .(period, pair_nato)],
                   period ~ pair_nato, value.var = "m")
tab2 <- merge(tab2_all, tab2_by, by = "period")
setnames(tab2,
         c("period", "all", "intra", "inter", "non"),
         c("Period", "Mean IPD (all)", "Intra-NATO", "Inter-NATO", "Non-NATO"))
setorder(tab2, Period)
fwrite(tab2, file.path(PATH_TAB, "geop_tab02_ipd_by_period_nato.csv"))

tex_tab2 <- kbl(tab2, format = "latex", booktabs = TRUE, digits = 3,
                caption = "Mean IPD by sub-period and NATO pair category",
                label = "tab:ipd_period_nato") |>
  kable_styling(latex_options = c("hold_position"))
writeLines(as.character(tex_tab2),
           file.path(PATH_TAB, "geop_tab02_ipd_by_period_nato.tex"))


# ---- Section 14 : Tab 3 - Top movers 2010-2024 -----------------------------

log_step("Section 14 : Tab 3 - top IPD movers 2010-2024.")

ipd_10 <- panel_pair[year == 2010 & !is.na(ipd),
                     .(exp_iso3, imp_iso3, ipd_2010 = ipd)]
ipd_23 <- panel_pair[year == 2024 & !is.na(ipd),
                     .(exp_iso3, imp_iso3, ipd_2024 = ipd)]
movers <- merge(ipd_10, ipd_23, by = c("exp_iso3", "imp_iso3"))
movers[, delta := ipd_2024 - ipd_2010]

nato23 <- unique(panel_pair[year == 2024, .(exp_iso3, imp_iso3,
                                             pair_nato_2024 = pair_nato)])
movers <- merge(movers, nato23, by = c("exp_iso3", "imp_iso3"), all.x = TRUE)
movers[, Pair := paste0(exp_iso3, " - ", imp_iso3)]

top_up <- movers[order(-delta)][1:10,
                  .(Pair, `IPD 2010` = round(ipd_2010, 3),
                   `IPD 2024` = round(ipd_2024, 3),
                   Delta = round(delta, 3),
                   `NATO 2024` = pair_nato_2024)]
top_dn <- movers[order(delta)][1:10,
                  .(Pair, `IPD 2010` = round(ipd_2010, 3),
                   `IPD 2024` = round(ipd_2024, 3),
                   Delta = round(delta, 3),
                   `NATO 2024` = pair_nato_2024)]

tab3 <- rbind(
  cbind(Direction = "Top 10 divergences (Delta > 0)", top_up),
  cbind(Direction = "Top 10 rapprochements (Delta < 0)", top_dn)
)
fwrite(tab3, file.path(PATH_TAB, "geop_tab03_ipd_movers.csv"))

tex_up <- kbl(top_up, format = "latex", booktabs = TRUE,
              caption = "Top 10 pairs with strongest IPD increase, 2010-2024",
              label = "tab:movers_up") |>
  kable_styling(latex_options = c("hold_position"))
tex_dn <- kbl(top_dn, format = "latex", booktabs = TRUE,
              caption = "Top 10 pairs with strongest IPD decrease, 2010-2024",
              label = "tab:movers_dn") |>
  kable_styling(latex_options = c("hold_position"))
writeLines(c(as.character(tex_up), "", as.character(tex_dn)),
           file.path(PATH_TAB, "geop_tab03_ipd_movers.tex"))


# ---- Done -------------------------------------------------------------------

log_step("Termine.")

cat("\nFigures (", PATH_FIG, ") :\n", sep = "")
print(list.files(PATH_FIG))
cat("\nMaps (", PATH_MAP, ") :\n", sep = "")
print(list.files(PATH_MAP))
cat("\nTables (", PATH_TAB, ") :\n", sep = "")
print(list.files(PATH_TAB))

if (length(errors)) {
  cat("\nERREURS / SKIP :\n")
  for (n in names(errors)) cat(" -", n, ":", errors[[n]], "\n")
} else {
  cat("\nAucune erreur, toutes les figures et tables generees.\n")
}
