# =============================================================================
# 11c_dcdh_by_tier.R  -- BLOC A : lecture de l'effet PAR PALIER d'intensite
# -----------------------------------------------------------------------------
# Le dCDH non-normalise de 11 date l'effet au 1er changement de palier (~2014).
# Ici on isole l'effet PAR NIVEAU :
#   (1) NORMALISE (normalized=TRUE) : effet par unite de dose (par cran de palier).
#   (2) dCDH BINAIRE par seuil 1{core >= s} : effet de FRANCHIR le palier s.
#       - s=1 : entree sous sanction core (~2014, onset).
#       - s=2 : palier intermediaire.
#       - s=6 : escalade lourde -> survient en 2022 pour les dyades Russie-Occident.
#         => "le passage au palier 6+ reduit le commerce de X", placebos a l'appui.
#
# Memes conventions que 11 : logs log(trade+1), groupe = paire non ordonnee,
# temps = year, cluster = paire (gid numerique), fenetre 2008-2023, controles
# jamais-traites echantillonnes (contrainte 8 Go RAM, seed 1234).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(DIDmultiplegtDYN)
})

PATH_ROOT <- "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis"
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "EventStudy")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "EventStudy")
log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
read_parquet_safe <- function(path, ...) {
  tmp <- tempfile(fileext = ".parquet")
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(arrow::read_parquet(tmp, ...))
}

YR_MIN <- 2008L; YR_MAX <- 2023L; N_CTRL <- 4000L; EFF <- 4L; PLA <- 2L

log_step("Chargement iv_panel + panel pkey (window 2008-2023).")
d <- read_parquet_safe(file.path(PATH_ROOT, "Data/Clean/iv_panel.parquet"))
d[, pkey := ifelse(exp_iso3 < imp_iso3, paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]
pk0 <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core)), by = .(pkey, year)]
pk0[, log_trade := log(trade_tot + 1)]
pk0[, tier := fcase(n_active_core == 0L, 0L, n_active_core == 1L, 1L,
                    n_active_core <= 5L, 2L, default = 3L)]
rm(d); gc(verbose = FALSE)

# Sous-echantillonnage des controles relatif a un indicateur de traitement :
# garde toutes les paires "ever-traitees" (treat>0 une annee) + N_CTRL paires
# jamais-traitees tirees au sort. gid numerique recree dans le sous-echantillon.
make_sample <- function(dt, treatvar) {
  ever  <- dt[get(treatvar) > 0L, unique(pkey)]
  never <- setdiff(unique(dt$pkey), ever)
  set.seed(1234)
  keep  <- c(ever, sample(never, min(N_CTRL, length(never))))
  s <- dt[pkey %in% keep]
  s[, gid := .GRP, by = pkey]
  list(s = s, n_ever = length(ever), n_ctrl = length(keep) - length(ever))
}

# Extrait placebos + effets + ATE d'un objet dCDH en table longue tidy.
tidy_dcdh <- function(m, label) {
  ef <- as.data.table(m$results$Effects,  keep.rownames = "term")
  pl <- as.data.table(m$results$Placebos, keep.rownames = "term")
  at <- as.data.table(m$results$ATE,      keep.rownames = "term")
  fix <- function(x){ setnames(x, c("LB CI","UB CI"), c("lb","ub"), skip_absent = TRUE); x }
  ef <- fix(ef); pl <- fix(pl); at <- fix(at)
  ef[, rel := as.integer(sub("Effect_","",term))]
  pl[, rel := -as.integer(sub("Placebo_","",term))]
  out <- rbind(
    pl[, .(model = label, term, rel, estimate = Estimate, se = SE, lb, ub)],
    data.table(model = label, term = "Ref_0", rel = 0L, estimate = 0, se = NA, lb = NA, ub = NA),
    ef[, .(model = label, term, rel, estimate = Estimate, se = SE, lb, ub)],
    at[, .(model = label, term = "ATE", rel = NA_integer_, estimate = Estimate, se = SE, lb, ub)],
    use.names = TRUE)
  out[]
}

results <- list()

# ---- (1) NORMALISE : effet par unite de dose (palier) ----------------------
log_step("(1) dCDH NORMALISE (per-dose). Patienter.")
smp <- make_sample(pk0, "tier")
cat("    groupes:", uniqueN(smp$s$pkey), "(ever", smp$n_ever, "+ ctrl", smp$n_ctrl, ")\n")
m_norm <- did_multiplegt_dyn(df = smp$s, outcome = "log_trade", group = "gid",
  time = "year", treatment = "tier", effects = 4, placebo = 2,
  normalized = TRUE, cluster = "gid", graph_off = TRUE)
results[["normalized_per_tier"]] <- tidy_dcdh(m_norm, "normalized_per_tier")
saveRDS(m_norm, file.path(PATH_TAB, "_dcdh_normalized_obj.rds")); rm(m_norm, smp); gc(verbose = FALSE)

# ---- (2) BINAIRE par seuil 1{core >= s} ------------------------------------
for (s in c(1L, 2L, 6L)) {
  lab <- sprintf("cross_ge%d", s)
  log_step(sprintf("(2) dCDH BINAIRE seuil core>=%d (%s). Patienter.", s, lab))
  pk0[, dthr := as.integer(n_active_core >= s)]
  smp <- make_sample(pk0, "dthr")
  cat("    groupes:", uniqueN(smp$s$pkey), "(ever", smp$n_ever, "+ ctrl", smp$n_ctrl, ")\n")
  m <- did_multiplegt_dyn(df = smp$s, outcome = "log_trade", group = "gid",
    time = "year", treatment = "dthr", effects = 4, placebo = 2,
    cluster = "gid", graph_off = TRUE)
  results[[lab]] <- tidy_dcdh(m, lab)
  rm(m, smp); gc(verbose = FALSE)
}

# ---- Sauvegarde table longue ------------------------------------------------
by_tier <- rbindlist(results, use.names = TRUE)
fwrite(by_tier, file.path(PATH_TAB, "tab_dcdh_by_tier.csv"))
log_step("Ecrit tab_dcdh_by_tier.csv. Apercu :")
print(by_tier[term != "Ref_0", .(model, term, rel,
      estimate = round(estimate,4), se = round(se,4),
      lb = round(lb,4), ub = round(ub,4))])

# ---- Figure : contraste onset (core>=1, ~2014) vs escalade lourde (>=6, 2022)
suppressPackageStartupMessages(library(ggplot2))
es <- by_tier[model %in% c("cross_ge1","cross_ge6") & term != "ATE"]
es[, Palier := factor(fifelse(model=="cross_ge1",
        "Onset : core >= 1 (~2014)", "Escalade lourde : core >= 6 (2022 Russie-Occ.)"),
        levels = c("Onset : core >= 1 (~2014)", "Escalade lourde : core >= 6 (2022 Russie-Occ.)"))]
p <- ggplot(es, aes(rel, estimate, color = Palier, fill = Palier)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = 0.5, lty = 3, color = "grey60") +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.12, color = NA, na.rm = TRUE) +
  geom_line(linewidth = 0.6, na.rm = TRUE) + geom_point(size = 2.4, na.rm = TRUE) +
  scale_x_continuous(breaks = min(es$rel):max(es$rel)) +
  scale_color_manual(values = c("#2166AC","#B2182B")) +
  scale_fill_manual(values = c("#2166AC","#B2182B")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=10, color="grey40"),
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA)) +
  labs(title = "Effet des sanctions PAR PALIER d'intensite (dCDH binaire par seuil)",
       subtitle = "log(trade+1). Cluster paire. rel<0 = placebos (pre-tendances). k=0 = annee du franchissement.",
       x = "Temps relatif au franchissement du palier (annees)",
       y = "Effet sur log(commerce)",
       caption = "de Chaisemartin & D'Haultfoeuille. Controles jamais-traites echantillonnes (8 Go). Seuil 6+ : 147 paires.")
ggsave(file.path(PATH_FIG, "es_fig02_dcdh_tiers.png"), p, width = 10, height = 6, dpi = 300)
log_step("Ecrit es_fig02_dcdh_tiers.png")
