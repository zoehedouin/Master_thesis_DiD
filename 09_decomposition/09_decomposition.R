# =============================================================================
# 09_decomposition.R   (etape 5 — decomposition strategique / non-strategique)
# -----------------------------------------------------------------------------
# A FAIRE UNE FOIS un resultat propre obtenu sur le commerce TOTAL (07/08).
# Re-tourne les MEILLEURES specs (event study PPML de 07_ppml.R + AVSQ paliers
# de 08_ols.R) sur les deux outcomes decomposes du panel strategique (02) :
#   * strategic_trade_value      (commerce strategique, HS6 — Aiyar et al. 2024)
#   * non_strategic_trade        (= trade_value - strategic_trade_value)
#
# Hypothese de mecanisme (feuille de route §5) : l'effet se concentre sur le
# NON-STRATEGIQUE / hors-energie (l'energie est largement exemptee et reroutee
# vers Chine/Inde). C'est aussi le test de TAUTOLOGIE : si l'effet deborde sur le
# commerce non directement vise par l'embargo, c'est de la vraie fragmentation.
# Contribution propre vs GSDB-R4 (qui declare ne pas pouvoir identifier les
# secteurs touches par les sanctions commerciales partielles — possible ici via HS6).
#
# Output : 09_decomposition/tables/ , 09_decomposition/figures/
# Entrees : master_panel_with_strategic.parquet (02), sanctions_panel.parquet (03).
# Chemins / wrappers I/O / helpers : 00_setup.R.
# =============================================================================

# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "DIDmultiplegtDYN", "ggplot2")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) message("Packages manquants (a installer) : ",
                          paste(miss, collapse = ", "))

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(ggplot2)
})

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})  # PATH_*, wrappers, out_tab/out_fig, log_step/tic/toc
PART <- "09_decomposition"   # co-localisation des sorties de cette partie (out_*)

PATH_TAB <- out_tab("Decomposition")
PATH_FIG <- out_fig("Decomposition")

log_step("09_decomposition : setup OK.")

YR_MIN <- 2008L; YR_MAX <- 2023L
wtab <- function(dt, base) { fwrite(dt, file.path(PATH_TAB, paste0(base, ".csv"))); invisible(dt) }
# Dictionnaires : buckets (colonnes DEJA calculees en 02) + directions.
BUCKETS <- c(total = "trade_value", embargo = "embargo_trade_value",
             strategic_ne = "strategic_nonembargo_trade_value",
             nonstrat_ne = "nonstrategic_nonembargo_trade_value")
DIRS    <- c("RUS_exportateur", "RUS_importateur")
# Exclusion reporting-gap auditable (ancre BLR_RUS, cf. 08_sunab_ols.R) -- niveau PAIRE.
pairs_reporting_gap <- c("BLR_RUS")


# ---- Section 1 : panel Russie-dirige + buckets (02) + traitement (07) --------

log_step("Section 1 : panel strategique (buckets 02) + traitement dirige (07) + cellule 2x2.")
df <- read_parquet_safe(PATH_STRATEGIC)
stopifnot(all(BUCKETS %in% names(df)))          # buckets DEJA presents (NE PAS recalculer)
df <- df[year >= YR_MIN & year <= YR_MAX]

# Dose d'intensite (paliers, cf. 08) : sanc_n_active_core depuis sanctions_panel.
sp <- read_parquet_safe(PATH_SANCTIONS_PANEL)
sp <- sp[year >= YR_MIN & year <= YR_MAX, .(exp_iso3, imp_iso3, year, sanc_n_active_core)]
df <- merge(df, sp, by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
df[is.na(sanc_n_active_core), sanc_n_active_core := 0L]
if (!"rta" %in% names(df)) df[, rta := 0L]      # securite (rta normalement present)

# RUSSIE-CENTRE + structure DIRIGEE (jamais collapse : la direction EST le design).
df <- df[exp_iso3 == "RUS" | imp_iso3 == "RUS"]
df[, partner   := fifelse(exp_iso3 == "RUS", imp_iso3, exp_iso3)]
df[, pair      := paste(exp_iso3, imp_iso3, sep = "_")]          # paire DIRIGEE (FE)
df[, pkey      := fifelse(exp_iso3 < imp_iso3, paste(exp_iso3, imp_iso3, sep = "_"),
                          paste(imp_iso3, exp_iso3, sep = "_"))] # non-ordonnee (cluster/exclusion)
df[, direction := fifelse(exp_iso3 == "RUS", "RUS_exportateur", "RUS_importateur")]
n_gap <- df[pkey %in% pairs_reporting_gap, uniqueN(pair)]
df <- df[!(pkey %in% pairs_reporting_gap)]      # exclut BLR_RUS (les 2 sens)
cat(sprintf("  - panel RUS dirige : %d obs | %d partenaires | reporting-gap exclu : %d paire(s) dirigees\n",
            nrow(df), uniqueN(df$partner), n_gap))

# --- Traitement DIRIGE partenaire->RUS non-commercial (VERBATIM 07_ppml.R) ----
master_iso3 <- sort(unique(c(df$exp_iso3, df$imp_iso3)))
g <- as.data.table(read_dta_safe(file.path(PATH_IV, "gsdb_v4", "GSDB_V4_dyadic.dta")))
g <- g[year >= YR_MIN & year <= YR_MAX &
       sanctioning_state_iso3 %in% master_iso3 & sanctioned_state_iso3 %in% master_iso3]
g <- g[as.character(case_id) != "1519"]         # KAZ Option B
to_rus <- g[sanctioned_state_iso3 == "RUS"]
stopifnot(to_rus[sanctioning_state_iso3 == "RUS", .N] == 0L)   # garde-fou Russie-emettrice
to_rus[, nt := as.integer(arms == 1 | military == 1 | financial == 1 | travel == 1 | other == 1)]
dir_py <- to_rus[, .(rus_nt = as.integer(any(nt == 1))), by = .(partner = sanctioning_state_iso3, year)]
onset  <- dir_py[rus_nt == 1L, .(onset_year = min(year)), by = partner]
df <- merge(df, onset, by = "partner", all.x = TRUE)
df[, ever         := !is.na(onset_year)]
df[, rel_time     := fifelse(ever, year - onset_year, NA_integer_)]
df[, treated_post := as.integer(ever & year >= onset_year)]
# Paliers d'intensite (cale 08 : 0 / 1 / 2-5 / 6+).
df[, tier := fcase(sanc_n_active_core == 0L, 0L, sanc_n_active_core == 1L, 1L,
                   sanc_n_active_core <= 5L, 2L, default = 3L)]
cat(sprintf("  - partenaires traites (non-commercial) : %d | cohortes : %s\n",
            df[ever == TRUE, uniqueN(partner)],
            paste(sort(unique(df[ever == TRUE, onset_year])), collapse = ",")))

# --- Cellule 2x2 (MEME source que 07 : covariables 05) ------------------------
cov <- read_parquet_safe(PATH_COVARIATES)
pp_cell <- unique(cov[, .(partner = partner_iso3, cell_2022 = as.character(cell_2022_static))])[!is.na(cell_2022)]
df <- merge(df, pp_cell, by = "partner", all.x = TRUE)
df[is.na(cell_2022), cell_2022 := "d_neither"]  # RUS sans cellule -> reference (rare)
df[, cell_2022 := droplevels(factor(cell_2022,
     levels = c("d_neither", "a_both", "b_condemn_only", "c_sanction_only")))]
df[, post2022 := as.integer(year >= 2022L)]

suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))   # accents figures


# ---- Section 2 : TIER 1 — event study PPML par bucket x direction -----------

log_step("Section 2 : TIER 1 event study PPML (Sun-Abraham) par bucket x direction.")
# Spec FE IDENTIFIEE en mono-direction : sur un sous-panel RUS_exportateur, exp=RUS
# est constant -> exp^year degenere en FE annee ; imp^year (= partenaire^year)
# absorberait le traitement (partenaire x temps) -> COLINEAIRE. La spec identifiee
# est donc pkey + year (TWFE etage), cluster paire. On TESTE la colinearite ci-dessous.
for (d_ in DIRS) {
  dd <- df[direction == d_ & ever == TRUE]
  m_test <- tryCatch(fepois(trade_value ~ treated_post | partner^year, data = df[direction == d_],
                            cluster = ~ pkey), error = function(e) NULL)
  dropped <- is.null(m_test) || is.na(coef(m_test)["treated_post"]) ||
             !"treated_post" %in% names(coef(m_test))
  cat(sprintf("  - [%s] FE partner^year : treated_post %s -> spec retenue = pkey + year\n",
              d_, if (dropped) "ABSORBE (colineaire)" else "estimable (mais on garde pkey+year pour homogeneite)"))
}

es_one <- function(ycol, lab_b, lab_d) {
  d <- df[direction == lab_d]
  d <- d[!(ever & onset_year < YR_MIN + 1L)]            # drop left-censored (onset 2008)
  d[, coh := fifelse(ever, onset_year, 10000L)]
  if (d[ever == TRUE, uniqueN(coh)] < 1L) { cat("   skip (0 cohorte) :", lab_b, lab_d, "\n"); return(NULL) }
  rg <- range(d[ever == TRUE, year - onset_year]); blo <- max(-5L, rg[1]); bhi <- min(5L, rg[2])
  fml <- as.formula(sprintf(
    "%s ~ sunab(coh, year, bin.rel = list('%d' = %d:%d, '%d' = %d:%d)) + rta | pkey + year",
    ycol, blo, rg[1], blo, bhi, bhi, rg[2]))
  m <- tryCatch(fepois(fml, data = d, cluster = ~ pkey),
                error = function(e) { cat("   ES skip", lab_b, lab_d, ":", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)
  es <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(es, 2:5, c("estimate","se","stat","p"))
  es <- es[grepl("year::", term)]
  es[, rel_time := as.integer(sub(".*year::(-?[0-9]+).*", "\\1", term))]
  es[, `:=`(bucket = lab_b, direction = lab_d, ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se)]
  att <- as.data.table(summary(m, agg = "att")$coeftable, keep.rownames = "term")
  setnames(att, 2:5, c("estimate","se","stat","p")); att <- att[term == "ATT"]
  list(es = setorder(es, rel_time),
       att = data.table(bucket = lab_b, direction = lab_d,
                        att = att$estimate, se = att$se, p = att$p))
}

t1_es <- list(); t1_att <- list(); t1_leads <- list()
for (d_ in DIRS) for (b_ in names(BUCKETS)) {
  r <- es_one(BUCKETS[[b_]], b_, d_)
  if (is.null(r)) next
  t1_es[[paste(d_, b_)]]  <- r$es
  t1_att[[paste(d_, b_)]] <- r$att
  leads <- r$es[rel_time < 0]
  drift <- leads[abs(estimate) > 0.15 & p < 0.10]
  t1_leads[[paste(d_, b_)]] <- data.table(direction = d_, bucket = b_,
     n_leads = nrow(leads), max_abs_lead = max(abs(leads$estimate)),
     pretrend_flag = if (nrow(drift)) "DRIFT" else "ok")
}
t1_es_dt <- rbindlist(t1_es, use.names = TRUE)
t1_att_dt <- rbindlist(t1_att, use.names = TRUE)
t1_leads_dt <- rbindlist(t1_leads, use.names = TRUE)
wtab(t1_es_dt, "tab_t1_eventstudy_by_bucket_direction")
wtab(t1_att_dt, "tab_t1_att_by_bucket_direction")
cat("\n  --- TIER 1 : ATT par bucket x direction ---\n")
print(t1_att_dt[, .(direction, bucket, att = round(att,4), se = round(se,4), p = round(p,4))])
cat("\n  --- GATE pre-tendances (leads) par bucket x direction ---\n")
print(t1_leads_dt)

# Figure : facettes 3 buckets (col) x 2 directions (ligne), hors total pour lisibilite
if (nrow(t1_es_dt)) {
  fig <- t1_es_dt[bucket != "total"]
  fig[, bucket := factor(bucket, levels = c("embargo","strategic_ne","nonstrat_ne"))]
  p1 <- ggplot(fig, aes(rel_time, estimate)) +
    geom_hline(yintercept = 0, lty = 2, color = "grey50") +
    geom_vline(xintercept = -0.5, lty = 3, color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166AC") +
    geom_line(color = "#2166AC") + geom_point(color = "#2166AC", size = 1.4) +
    facet_grid(direction ~ bucket, scales = "free_y") +
    labs(title = "Decomposition §5 — event study PPML par bucket x direction",
         subtitle = "Sun-Abraham, FE pkey + year, cluster paire, zeros gardes. Leads visibles = test de pre-tendance.",
         x = "Temps relatif a l'onset (annees)", y = "Effet (semi-elasticite)",
         caption = "Buckets MECE (02). Direction = sous-panel RUS exportateur / importateur. Exclusion BLR_RUS.") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold", size = 8),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
  ggsave(file.path(PATH_FIG, "fig_t1_eventstudy.png"), p1, width = 11, height = 6, dpi = 300)
  cat("  - ecrit fig_t1_eventstudy.png\n")
}


# ---- Section 3 : TIER 2 — dCDH AVSQ cible (buckets gras par direction) -------

log_step("Section 3 : TIER 2 dCDH (paliers) cible : embargo|RUS_exp, nonstrat_ne|RUS_imp.")
have_dcdh <- requireNamespace("DIDmultiplegtDYN", quietly = TRUE)
if (!have_dcdh) cat("  !! DIDmultiplegtDYN indisponible -> TIER 2 saute.\n") else {
  suppressPackageStartupMessages(library(DIDmultiplegtDYN))
  tidy_dcdh <- function(m, label) {
    f <- function(x) { setnames(x, c("LB CI","UB CI"), c("lb","ub"), skip_absent = TRUE); x }
    ef <- f(as.data.table(m$results$Effects,  keep.rownames = "term")); ef[, rel := as.integer(sub("Effect_","",term))]
    pl <- f(as.data.table(m$results$Placebos, keep.rownames = "term")); pl[, rel := -as.integer(sub("Placebo_","",term))]
    at <- f(as.data.table(m$results$ATE,      keep.rownames = "term"))
    rbind(pl[, .(model = label, term, rel, estimate = Estimate, se = SE, lb, ub)],
          ef[, .(model = label, term, rel, estimate = Estimate, se = SE, lb, ub)],
          at[, .(model = label, term = "ATE", rel = NA_integer_, estimate = Estimate, se = SE, lb, ub)],
          use.names = TRUE)
  }
  run_dcdh <- function(lab_d, ycol, transform, label) {
    d <- copy(df[direction == lab_d])
    d[, y := if (transform == "ihs") asinh(get(ycol)) else log(get(ycol) + 1)]
    d[, gid := .GRP, by = pair]
    set.seed(1234)
    m <- tryCatch(did_multiplegt_dyn(df = d, outcome = "y", group = "gid", time = "year",
                    treatment = "tier", effects = 4, placebo = 2, cluster = "gid", graph_off = TRUE),
                  error = function(e) { cat("   dCDH skip", label, ":", conditionMessage(e), "\n"); NULL })
    if (is.null(m)) return(NULL)
    out <- tidy_dcdh(m, label); out[, `:=`(direction = lab_d, transform = transform)][]
  }
  t2 <- list()
  # Buckets GRAS (principal) : log+1 + controle IHS
  for (tr in c("log1p","ihs")) {
    t2[[paste("emb_exp", tr)]] <- run_dcdh("RUS_exportateur", "embargo_trade_value", tr, "embargo|RUS_exp")
    t2[[paste("nse_imp", tr)]] <- run_dcdh("RUS_importateur", "nonstrategic_nonembargo_trade_value", tr, "nonstrat_ne|RUS_imp")
  }
  # strategic_ne : EXPLORATOIRE (IC larges, ne pas mettre au meme rang)
  t2[["sne_imp_explo"]] <- run_dcdh("RUS_importateur", "strategic_nonembargo_trade_value", "log1p", "strategic_ne|RUS_imp_EXPLO")
  t2_dt <- rbindlist(Filter(Negate(is.null), t2), use.names = TRUE, fill = TRUE)
  if (nrow(t2_dt)) {
    wtab(t2_dt, "tab_t2_dcdh_by_bucket_direction")
    cat("\n  --- TIER 2 : ATE dCDH (log+1 vs IHS) ---\n")
    print(t2_dt[term == "ATE", .(model, direction, transform,
          ate = round(estimate,4), se = round(se,4), lb = round(lb,4), ub = round(ub,4))])
  } else cat("  !! TIER 2 : aucun modele estimable.\n")
}


# ---- Section 4 : TIER 3 — PPML statique 2x2 par cellule x bucket x direction -

log_step("Section 4 : TIER 3 PPML statique 2x2 (condamne x sanctionne) par bucket x direction.")
stat_one <- function(ycol, lab_b, lab_d) {
  d <- df[direction == lab_d]
  if (d[, uniqueN(cell_2022)] < 2L) return(NULL)
  m <- tryCatch(fepois(as.formula(sprintf(
         "%s ~ i(cell_2022, post2022, ref = 'd_neither') + rta | pkey + year", ycol)),
         data = d, cluster = ~ pkey),
       error = function(e) { cat("   2x2 skip", lab_b, lab_d, ":", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)
  ct <- as.data.table(coeftable(m), keep.rownames = "term"); setnames(ct, 2:5, c("estimate","se","stat","p"))
  ct <- ct[grepl("cell_2022", term)]
  ct[, cell := sub(".*cell_2022::([a-z_]+).*", "\\1", term)]
  ct[, `:=`(bucket = lab_b, direction = lab_d, ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se)]
  ct[, .(direction, bucket, cell, estimate, se, p, ci_lo, ci_hi)]
}
t3 <- list()
for (d_ in DIRS) for (b_ in names(BUCKETS)) {
  r <- stat_one(BUCKETS[[b_]], b_, d_); if (!is.null(r)) t3[[paste(d_,b_)]] <- r
}
t3_dt <- rbindlist(t3, use.names = TRUE)
if (nrow(t3_dt)) {
  wtab(t3_dt, "tab_t3_static_2x2_by_bucket_direction")
  cat("\n  --- TIER 3 : 2x2 statique (effet cellule x post2022) ---\n")
  print(t3_dt[, .(direction, bucket, cell, est = round(estimate,4), se = round(se,4), p = round(p,4))])
  fig3 <- t3_dt[bucket != "total" & cell %in% c("a_both","b_condemn_only")]
  if (nrow(fig3)) {
    fig3[, bucket := factor(bucket, levels = c("embargo","strategic_ne","nonstrat_ne"))]
    p3 <- ggplot(fig3, aes(estimate, cell, color = cell)) +
      geom_vline(xintercept = 0, lty = 2, color = "grey50") +
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2) + geom_point(size = 2.4) +
      facet_grid(direction ~ bucket, scales = "free_x") +
      scale_color_manual(values = c(a_both = "#2166AC", b_condemn_only = "#B2182B"), guide = "none") +
      labs(title = "Decomposition §5 — 2x2 statique (vote ONU) par bucket x direction",
           subtitle = "PPML statique, i(cellule, post2022, ref = Neither), FE pkey + year, cluster paire.",
           x = "Effet (semi-elasticite)", y = NULL,
           caption = "a_both = condamne+sanctionne ; b_condemn_only = condamne seul. Buckets MECE (02).") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold", size = 8),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA))
    ggsave(file.path(PATH_FIG, "fig_t3_static_2x2.png"), p3, width = 11, height = 6, dpi = 300)
    cat("  - ecrit fig_t3_static_2x2.png\n")
  }
} else cat("  !! TIER 3 : aucun modele estimable.\n")


# ---- Section 5 : recapitulatif -----------------------------------------------
log_step("Section 5 : recapitulatif.")
cat("\n== Tables ==\n"); print(list.files(PATH_TAB, pattern = "^tab_t"))
cat("== Figures ==\n"); print(list.files(PATH_FIG, pattern = "^fig_t"))
log_step("09_decomposition.R termine.")
