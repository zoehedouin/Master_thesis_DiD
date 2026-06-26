# =============================================================================
# 06_descriptives.R — fusion of 03a+03b+03c, recentree Russie (feuille de route §1)
# -----------------------------------------------------------------------------
# Script descriptif unifie du memoire (DiD sanctions / votes ONU, centre Russie).
# Fusion FIDELE des trois anciens scripts descriptifs (desormais archives sous
# _archive/legacy_descriptives/) : 03a_desc_trade.R, 03b_desc_geopolitics.R,
# 03c_desc_interaction.R. Aucune logique d'analyse ni aucun calcul numerique n'a
# ete modifie ; seuls le bloc de setup (libraries, chemins, wrappers I/O) a ete
# consolide via source("00_setup.R") et les chemins de sortie reecrits avec les
# helpers out_tab() / out_fig() / out_map() / out_rep().
#
# Le script comporte DEUX blocs :
#
#   (A) SOCLE GENERAL — structure generale du commerce, distribution de l'IPD,
#       cartes d'ouverture/geopolitique. Conserve tel quel depuis 03a/03b/03c ;
#       sert d'introduction et de cadrage (intro/framing) du memoire.
#
#   (B) BLOC RUSSIE — recentre / a ajouter selon la feuille de route §1. NON
#       IMPLEMENTE ici : seule la liste des ajouts figure dans le bloc TODO
#       ci-dessous. Aucun resultat n'est fabrique.
#
# -----------------------------------------------------------------------------
## TODO (feuille de route §1) — Bloc Russie (a AJOUTER, non implemente)
#
#   [ ] Evolution du commerce Russie-partenaires par statut :
#         - sanctionneur (sanctioner)
#         - condamnateur ONU (UN-condemner)
#         - cellule 2x2 (croisement sanction x vote ONU)
#       => series temporelles du commerce bilateral avec la Russie par groupe.
#
#   [ ] Calendrier de traitement (treatment calendar) :
#         - pic d'entree en traitement (onset peak) 2014
#         - intensification 2022
#       => visualiser le timing d'entree des partenaires dans le traitement.
#
#   [ ] Tableau croise 2x2 (cross-tab) :
#         - nombre de partenaires par cellule
#         - poids commercial par cellule
#       => croisement sanctionneur x condamnateur ONU.
#
#   [ ] Distributions des covariables par groupe :
#         - premier regard sur le tri / la selection (first look at sorting)
#       => comparer les distributions des controles entre groupes de traitement.
#
# (Bloc B non code : ne PAS fabriquer de chiffres ; implementation ulterieure.)
# =============================================================================


# ---- Setup consolide --------------------------------------------------------
# Libraries : union des paquets requis par 03a + 03b + 03c (charges une seule
# fois). 00_setup.R fournit chemins, wrappers I/O NFD-safe, log_step/tic/toc.

need <- c("data.table", "ggplot2", "arrow", "scales",
          "rnaturalearth", "rnaturalearthdata", "sf", "kableExtra",
          "patchwork", "fixest")
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
  library(fixest)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})
PART <- "06_descriptives"   # co-localisation des sorties de cette partie (out_*)

# Theme global (commun aux trois scripts d'origine)
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

# Helper : modal pair_nato par paire (utilise par 03a et 03b)
pair_mode <- function(x) {
  t <- table(x)
  names(t)[which.max(t)]
}

# Collecteur d'erreurs (figure skipping ; utilise par 03b/03c)
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


# =============================================================================
# (A) SOCLE GENERAL
# =============================================================================


# ===== [from 03a_desc_trade.R] =====
# Statistiques descriptives sur le commerce mondial :
#   - 9 figures (PNG 300 dpi) ; 3 tables (TeX + CSV)
# Sorties : out_fig("Trade") / out_tab("Trade") / out_map("Trade")

PATH_FIG   <- out_fig("Trade")
PATH_TAB   <- out_tab("Trade")
PATH_MAP   <- out_map("Trade")


# ---- Section 1 : Load + objets reutilisables -------------------------------

log_step("Section 1 : load panel et agreg annuels.")

panel <- read_parquet_safe(PATH_STRATEGIC)
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

log_step("[03a] Termine.")
cat("\nFigures :\n"); print(list.files(PATH_FIG, full.names = FALSE))
cat("\nTables :\n");  print(list.files(PATH_TAB, full.names = FALSE))


# ===== [from 03b_desc_geopolitics.R] =====
# Descriptives geopolitiques :
#   Bloc 1 (Figs 1-6)  : IPD dans le monde
#   Bloc 2 (Figs 7-10) : NATO + IPD x NATO + within-between
#   Bloc 3 (Tabs 1-3)  : summary stats, mean par periode x NATO, movers
# Sorties : out_fig("Geopolitics") / out_tab("Geopolitics") / out_map("Geopolitics")

PATH_FIG  <- out_fig("Geopolitics")
PATH_TAB  <- out_tab("Geopolitics")
PATH_MAP  <- out_map("Geopolitics")


# ---- Section 1 : Load + helpers --------------------------------------------

log_step("Section 1 : load panel.")
panel <- read_parquet_safe(PATH_STRATEGIC)
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

log_step("[03b] Termine.")

cat("\nFigures (", PATH_FIG, ") :\n", sep = "")
print(list.files(PATH_FIG))
cat("\nMaps (", PATH_MAP, ") :\n", sep = "")
print(list.files(PATH_MAP))
cat("\nTables (", PATH_TAB, ") :\n", sep = "")
print(list.files(PATH_TAB))


# ===== [from 03c_desc_interaction.R] =====
# Descriptives trade x geopolitics x NATO :
#   BLOC 1 (Figs 1-3)  : relation IPD x trade (raw + residualise + strategic)
#   BLOC 2 (Figs 4-6)  : event studies 2022 (Ukraine), 2018 (Trade War), 2014 (Crimea)
#   BLOC 3 (Figs 7-10) : quartiles IPD, NATO x strategic, RTA dynamics
#   BLOC 4 (Fig 11)    : matrice de correlation
#   BLOC 5 (Tabs 1-4)  : transition, NATO summary, before/after 2022, partial corr
# Sorties : out_fig("Interaction") / out_tab("Interaction") / out_map("Interaction")

PATH_FIG  <- out_fig("Interaction")
PATH_TAB  <- out_tab("Interaction")
PATH_MAP  <- out_map("Interaction")


# ---- Section 1 : Load -------------------------------------------------------

log_step("Section 1 : load panel.")
panel <- read_parquet_safe(PATH_STRATEGIC)
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

# ---- Fig 1 : binned scatter raw (ESSENTIEL) --------------------------------

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


# ---- Fig 2 : binned scatter residualise (FE) (ESSENTIEL) -------------------

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


# --- [DEGRAISSE] figures d'interaction secondaires : conservees mais a elaguer ---
# Fig 3 : binned scatter strategic vs total (duplique le binscatter de Fig 1).
# Conserve tel quel ; candidat a l'archivage / elagage.

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

# ---- Fig 4 : event study 2022 (Ukraine) (ESSENTIEL) -----------------------

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


# ---- Fig 5 : event study 2022 (strategic) (ESSENTIEL) ---------------------

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


# ---- Fig 6 : event study 2018 (Trade War) (ESSENTIEL) ---------------------

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


# ---- Fig 6b : event study 2018 (strategic) (ESSENTIEL) --------------------

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


# ---- Fig 6c : event study 2014 Crimea (total) (ESSENTIEL) -----------------

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


# ---- Fig 6d : event study 2014 Crimea (strategic) (ESSENTIEL) -------------

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

# --- [DEGRAISSE] figures d'interaction secondaires : conservees mais a elaguer ---
# Figs 7 a 11 (quartiles d'IPD, NATO x strategic, RTA dynamics, matrice de
# correlation) : secondaires pour l'argument central. Conservees telles quelles
# (code intact) mais candidates a l'elagage / archivage des PNG correspondants.

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

# ---- Tab 1 : transition matrix commerce x IPD (ESSENTIEL) ----------------

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

log_step("[03c] Termine.")

cat("\nFigures :\n"); print(list.files(PATH_FIG))
cat("\nTables :\n");  print(list.files(PATH_TAB))

if (length(errors)) {
  cat("\nERREURS / SKIP :\n")
  for (n in names(errors)) cat(" -", n, ":", errors[[n]], "\n")
} else {
  cat("\nAucune erreur, toutes les figures et tables generees.\n")
}

log_step("06_descriptives.R (socle general) termine. Bloc Russie : voir TODO en tete.")
