# =============================================================================
# 05c_gravity_iv_exploration.R
# -----------------------------------------------------------------------------
# Test exhaustif d'instruments alternatifs construits a partir de sources DE
# VARIATION DIFFERENTES des ideal points (UNGA). Permet de tester si le
# coefficient IPD reste stable independamment du choix d'instrument.
#
#   IV-1 : spatial lag (IPD moyen des voisins contigus, cote exporter)
#   IV-2 : spatial lag symetrique (moyenne des deux cotes)
#   IV-3 : distance geo aux poles x events (2014, 2018, 2022, joint)
#   IV-4 : leave-one-out mean IPD
#   IV-5 : distance de GDP per capita (proxy bloc developpement)
#   IV-6 : rappel - alignment lag 2 (script 05) pour comparaison
#
# Output : figure + table de synthese + rapport markdown auto-genere.
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "ggplot2", "scales")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
  library(ggplot2);    library(scales)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_DATA <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_FIG  <- file.path(PATH_ROOT, "Output", "Figures", "Estimation_IV")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables",  "Estimation_IV")
dir.create(PATH_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)
log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

theme_memoir <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
theme_set(theme_memoir)

log_step("Chargement panel.")
df <- as.data.table(read_parquet(PATH_DATA))
df[, pair      := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year  := paste(exp_iso3, year,     sep = "_")]
df[, imp_year  := paste(imp_iso3, year,     sep = "_")]

poles <- c("USA", "CHN", "RUS")
df_iv <- df[!(exp_iso3 %in% poles) & !(imp_iso3 %in% poles) & !is.na(ipd)]
cat("  - Echantillon IV (hors poles) :", nrow(df_iv), "obs\n")

# PPML reference
log_step("PPML de reference (no IV).")
ppml_ref <- fepois(trade_value ~ ipd + rta | exp_year + imp_year + pair,
                   data = df_iv, vcov = ~pair)
print(summary(ppml_ref))

# Helper d'extraction
get_p_for <- function(model, term) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  ct[term == ..term, p][1]
}
get_pvhat <- function(model) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  row <- ct[grepl("v_hat", term)]
  if (nrow(row) == 0) NA_real_ else row$p[1]
}
get_ipd_coef <- function(model) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, 2:5, c("estimate", "se", "stat", "p"))
  row <- ct[term == "ipd"]
  if (nrow(row) == 0) list(c = NA, s = NA, p = NA)
  else list(c = row$estimate[1], s = row$se[1], p = row$p[1])
}

all_results <- list()
add_result <- function(name, model, F_stat, N, corr_iv_ipd) {
  ic <- get_ipd_coef(model)
  all_results[[name]] <<- list(
    ipd_coef = ic$c, ipd_se = ic$s, ipd_pval = ic$p,
    vhat_pval = get_pvhat(model),
    F_stat = F_stat, N = N, corr_iv_ipd = corr_iv_ipd
  )
}


# =============================================================================
# IV-1 : Spatial lag (IPD moyen des voisins de exp avec imp)
# =============================================================================

log_step("IV-1 : spatial lag (voisins de exp).")

neighbors <- unique(df[contig == 1 & exp_iso3 != imp_iso3,
                       .(country = exp_iso3, neighbor = imp_iso3)])
cat("  - Liens contiguite :", nrow(neighbors),
    "| pays avec >=1 voisin :", uniqueN(neighbors$country), "\n")

ipd_table <- unique(df[!is.na(ipd),
                       .(iso_a = exp_iso3, iso_b = imp_iso3, year, ipd)])

years_iv <- sort(unique(df_iv$year))
tic()
spatial_iv <- rbindlist(lapply(years_iv, function(yr) {
  ipd_yr <- ipd_table[year == yr]
  pairs_yr <- df_iv[year == yr, .(exp_iso3, imp_iso3)]
  m <- merge(pairs_yr, neighbors, by.x = "exp_iso3", by.y = "country",
             allow.cartesian = TRUE)
  m <- merge(m, ipd_yr[, .(iso_a, iso_b, ipd_neighbor = ipd)],
             by.x = c("neighbor", "imp_iso3"),
             by.y = c("iso_a", "iso_b"), all.x = TRUE)
  m[!is.na(ipd_neighbor),
    .(iv_spatial = mean(ipd_neighbor), n_nb = .N),
    by = .(exp_iso3, imp_iso3)][, year := yr]
}))
cat("  - Spatial IV construit en", toc(), "s,", nrow(spatial_iv), "obs\n")

df_iv <- merge(df_iv, spatial_iv,
               by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
cat(sprintf("  - Obs avec iv_spatial : %d (%.1f%%)\n",
            sum(!is.na(df_iv$iv_spatial)),
            100 * mean(!is.na(df_iv$iv_spatial))))
corr_sp <- cor(df_iv$iv_spatial, df_iv$ipd, use = "complete.obs")
cat(sprintf("  - Corr(iv_spatial, ipd) : %.3f\n", corr_sp))

df_sp <- df_iv[!is.na(iv_spatial)]
fs_sp <- feols(ipd ~ iv_spatial + rta | exp_year + imp_year + pair,
               data = df_sp, vcov = ~pair, notes = FALSE)
print(summary(fs_sp))
if (nobs(fs_sp) != nrow(df_sp)) {
  df_sp <- df_sp[!is.na(predict(fs_sp, newdata = df_sp))]
  fs_sp <- feols(ipd ~ iv_spatial + rta | exp_year + imp_year + pair,
                 data = df_sp, vcov = ~pair, notes = FALSE)
}
df_sp[, v_hat_sp := residuals(fs_sp)]

ct_fs <- as.data.table(coeftable(fs_sp), keep.rownames = "term")
setnames(ct_fs, 2:5, c("estimate", "se", "stat", "p"))
f_sp <- ct_fs[term == "iv_spatial", stat^2]
cat(sprintf("  - First-stage F : %.1f\n", f_sp))

iv1 <- fepois(trade_value ~ ipd + v_hat_sp + rta
              | exp_year + imp_year + pair,
              data = df_sp, vcov = ~pair)
print(summary(iv1))
add_result("IV-1: Spatial lag (exp neighbors)", iv1,
           f_sp, nobs(iv1), corr_sp)


# =============================================================================
# IV-2 : Spatial lag symetrique
# =============================================================================

log_step("IV-2 : spatial lag symetrique.")

tic()
spatial_iv_imp <- rbindlist(lapply(years_iv, function(yr) {
  ipd_yr <- ipd_table[year == yr]
  pairs_yr <- df_iv[year == yr, .(exp_iso3, imp_iso3)]
  m <- merge(pairs_yr, neighbors, by.x = "imp_iso3", by.y = "country",
             allow.cartesian = TRUE)
  m <- merge(m, ipd_yr[, .(iso_a, iso_b, ipd_neighbor = ipd)],
             by.x = c("exp_iso3", "neighbor"),
             by.y = c("iso_a", "iso_b"), all.x = TRUE)
  m[!is.na(ipd_neighbor),
    .(iv_spatial_imp = mean(ipd_neighbor)),
    by = .(exp_iso3, imp_iso3)][, year := yr]
}))
cat("  - Spatial imp IV en", toc(), "s\n")

df_iv <- merge(df_iv, spatial_iv_imp,
               by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
df_iv[, iv_spatial_sym := (iv_spatial + iv_spatial_imp) / 2]
corr_sym <- cor(df_iv$iv_spatial_sym, df_iv$ipd, use = "complete.obs")
cat(sprintf("  - Corr(iv_spatial_sym, ipd) : %.3f\n", corr_sym))

df_sym <- df_iv[!is.na(iv_spatial_sym)]
fs_sym <- feols(ipd ~ iv_spatial_sym + rta | exp_year + imp_year + pair,
                data = df_sym, vcov = ~pair, notes = FALSE)
if (nobs(fs_sym) != nrow(df_sym)) {
  df_sym <- df_sym[!is.na(predict(fs_sym, newdata = df_sym))]
  fs_sym <- feols(ipd ~ iv_spatial_sym + rta | exp_year + imp_year + pair,
                  data = df_sym, vcov = ~pair, notes = FALSE)
}
df_sym[, v_hat_sym := residuals(fs_sym)]
ct_fs2 <- as.data.table(coeftable(fs_sym), keep.rownames = "term")
setnames(ct_fs2, 2:5, c("estimate", "se", "stat", "p"))
f_sym <- ct_fs2[term == "iv_spatial_sym", stat^2]
cat(sprintf("  - First-stage F : %.1f\n", f_sym))

iv2 <- fepois(trade_value ~ ipd + v_hat_sym + rta
              | exp_year + imp_year + pair,
              data = df_sym, vcov = ~pair)
print(summary(iv2))
add_result("IV-2: Spatial lag symmetric", iv2,
           f_sym, nobs(iv2), corr_sym)


# =============================================================================
# IV-3 : Distance geo aux poles x events
# =============================================================================

log_step("IV-3 : geo distance to poles x events.")

geo_to_poles <- rbindlist(lapply(poles, function(p) {
  d <- unique(df[imp_iso3 == p & !is.na(dist),
                 .(iso3 = exp_iso3, dist_to_pole = dist)])
  d[, pole := p]; d[]
}))
geo_wide <- dcast(geo_to_poles, iso3 ~ pole, value.var = "dist_to_pole")
setnames(geo_wide, poles, c("geodist_usa", "geodist_chn", "geodist_rus"))

df_iv <- merge(df_iv, geo_wide, by.x = "exp_iso3", by.y = "iso3", all.x = TRUE)
setnames(df_iv, c("geodist_usa", "geodist_chn", "geodist_rus"),
                 c("exp_gd_usa", "exp_gd_chn", "exp_gd_rus"))
df_iv <- merge(df_iv, geo_wide, by.x = "imp_iso3", by.y = "iso3", all.x = TRUE)
setnames(df_iv, c("geodist_usa", "geodist_chn", "geodist_rus"),
                 c("imp_gd_usa", "imp_gd_chn", "imp_gd_rus"))

df_iv[, geodist_poles := sqrt(
  (log(exp_gd_usa) - log(imp_gd_usa))^2 +
  (log(exp_gd_chn) - log(imp_gd_chn))^2 +
  (log(exp_gd_rus) - log(imp_gd_rus))^2)]

df_iv[, post_2014 := as.integer(year >= 2014)]
df_iv[, post_2018 := as.integer(year >= 2018)]
df_iv[, post_2022 := as.integer(year >= 2022)]
df_iv[, iv_geo_2014 := geodist_poles * post_2014]
df_iv[, iv_geo_2018 := geodist_poles * post_2018]
df_iv[, iv_geo_2022 := geodist_poles * post_2022]

for (event in c("2014", "2018", "2022")) {
  iv_name <- paste0("iv_geo_", event)
  vhat_name <- paste0("v_hat_geo_", event)
  df_sub <- df_iv[!is.na(get(iv_name))]
  fs <- feols(as.formula(paste0("ipd ~ ", iv_name,
                " + rta | exp_year + imp_year + pair")),
              data = df_sub, vcov = ~pair, notes = FALSE)
  if (nobs(fs) != nrow(df_sub)) {
    df_sub <- df_sub[!is.na(predict(fs, newdata = df_sub))]
    fs <- feols(as.formula(paste0("ipd ~ ", iv_name,
                  " + rta | exp_year + imp_year + pair")),
                data = df_sub, vcov = ~pair, notes = FALSE)
  }
  df_sub[, (vhat_name) := residuals(fs)]
  ct_fs3 <- as.data.table(coeftable(fs), keep.rownames = "term")
  setnames(ct_fs3, 2:5, c("estimate", "se", "stat", "p"))
  f_val <- ct_fs3[term == iv_name, stat^2]
  cat(sprintf("  Event %s : F=%.2f\n", event, f_val))
  corr_val <- cor(df_sub[[iv_name]], df_sub$ipd, use = "complete.obs")
  cf <- fepois(as.formula(paste0("trade_value ~ ipd + ", vhat_name,
                " + rta | exp_year + imp_year + pair")),
               data = df_sub, vcov = ~pair)
  print(summary(cf))
  add_result(paste0("IV-3: Geo x post_", event), cf, f_val, nobs(cf), corr_val)
}

# Joint : 3 events ensemble
df_geo_all <- df_iv[!is.na(geodist_poles)]
fs_geo_all <- feols(ipd ~ iv_geo_2014 + iv_geo_2018 + iv_geo_2022 + rta
                          | exp_year + imp_year + pair,
                    data = df_geo_all, vcov = ~pair, notes = FALSE)
if (nobs(fs_geo_all) != nrow(df_geo_all)) {
  df_geo_all <- df_geo_all[!is.na(predict(fs_geo_all, newdata = df_geo_all))]
  fs_geo_all <- feols(ipd ~ iv_geo_2014 + iv_geo_2018 + iv_geo_2022 + rta
                            | exp_year + imp_year + pair,
                      data = df_geo_all, vcov = ~pair, notes = FALSE)
}
df_geo_all[, v_hat_geo_all := residuals(fs_geo_all)]
joint_F <- tryCatch(
  wald(fs_geo_all, c("iv_geo_2014", "iv_geo_2018", "iv_geo_2022"))$stat,
  error = function(e) NA_real_
)
cat(sprintf("  Joint F (3 events) : %.2f\n", joint_F))

iv3_all <- fepois(trade_value ~ ipd + v_hat_geo_all + rta
                  | exp_year + imp_year + pair,
                  data = df_geo_all, vcov = ~pair)
print(summary(iv3_all))
add_result("IV-3: Geo x 3 events (joint)", iv3_all, joint_F, nobs(iv3_all), NA)


# =============================================================================
# IV-4 : Leave-one-out mean IPD
# =============================================================================

log_step("IV-4 : leave-one-out mean IPD.")

mean_ipd_exp <- df[!is.na(ipd), .(sum_ipd_e = sum(ipd), n_e = .N),
                   by = .(exp_iso3, year)]
df_iv <- merge(df_iv, mean_ipd_exp, by = c("exp_iso3", "year"), all.x = TRUE)
df_iv[, iv_loo_exp := (sum_ipd_e - ipd) / (n_e - 1)]

mean_ipd_imp <- df[!is.na(ipd), .(sum_ipd_i = sum(ipd), n_i = .N),
                   by = .(imp_iso3, year)]
df_iv <- merge(df_iv, mean_ipd_imp, by = c("imp_iso3", "year"), all.x = TRUE)
df_iv[, iv_loo_imp := (sum_ipd_i - ipd) / (n_i - 1)]

df_iv[, iv_loo := (iv_loo_exp + iv_loo_imp) / 2]
corr_loo <- cor(df_iv$iv_loo, df_iv$ipd, use = "complete.obs")
cat(sprintf("  - Corr(iv_loo, ipd) : %.3f\n", corr_loo))

df_loo <- df_iv[!is.na(iv_loo)]
fs_loo <- feols(ipd ~ iv_loo + rta | exp_year + imp_year + pair,
                data = df_loo, vcov = ~pair, notes = FALSE)
if (nobs(fs_loo) != nrow(df_loo)) {
  df_loo <- df_loo[!is.na(predict(fs_loo, newdata = df_loo))]
  fs_loo <- feols(ipd ~ iv_loo + rta | exp_year + imp_year + pair,
                  data = df_loo, vcov = ~pair, notes = FALSE)
}
df_loo[, v_hat_loo := residuals(fs_loo)]
ct_fs4 <- as.data.table(coeftable(fs_loo), keep.rownames = "term")
setnames(ct_fs4, 2:5, c("estimate", "se", "stat", "p"))
f_loo <- ct_fs4[term == "iv_loo", stat^2]
cat(sprintf("  - First-stage F : %.1f\n", f_loo))

iv4 <- fepois(trade_value ~ ipd + v_hat_loo + rta
              | exp_year + imp_year + pair,
              data = df_loo, vcov = ~pair)
print(summary(iv4))
add_result("IV-4: Leave-one-out mean IPD", iv4, f_loo, nobs(iv4), corr_loo)


# =============================================================================
# IV-5 : GDP per capita distance
# =============================================================================

log_step("IV-5 : GDP per capita distance.")

df_iv[, gdppc_exp := exp_gdp_real / exp_pop]
df_iv[, gdppc_imp := imp_gdp_real / imp_pop]
df_iv[, iv_gdppc_dist := abs(log(gdppc_exp) - log(gdppc_imp))]
corr_gdp <- cor(df_iv$iv_gdppc_dist, df_iv$ipd, use = "complete.obs")
cat(sprintf("  - Corr(iv_gdppc_dist, ipd) : %.3f\n", corr_gdp))

df_gdp <- df_iv[!is.na(iv_gdppc_dist)]
fs_gdp <- feols(ipd ~ iv_gdppc_dist + rta | exp_year + imp_year + pair,
                data = df_gdp, vcov = ~pair, notes = FALSE)
if (nobs(fs_gdp) != nrow(df_gdp)) {
  df_gdp <- df_gdp[!is.na(predict(fs_gdp, newdata = df_gdp))]
  fs_gdp <- feols(ipd ~ iv_gdppc_dist + rta | exp_year + imp_year + pair,
                  data = df_gdp, vcov = ~pair, notes = FALSE)
}
df_gdp[, v_hat_gdp := residuals(fs_gdp)]
ct_fs5 <- as.data.table(coeftable(fs_gdp), keep.rownames = "term")
setnames(ct_fs5, 2:5, c("estimate", "se", "stat", "p"))
f_gdp <- ct_fs5[term == "iv_gdppc_dist", stat^2]
cat(sprintf("  - First-stage F : %.1f\n", f_gdp))

iv5 <- fepois(trade_value ~ ipd + v_hat_gdp + rta
              | exp_year + imp_year + pair,
              data = df_gdp, vcov = ~pair)
print(summary(iv5))
add_result("IV-5: GDP per capita distance", iv5, f_gdp, nobs(iv5), corr_gdp)


# =============================================================================
# IV-6 : Rappel - alignement lag 2 (ideal-points based)
# =============================================================================

log_step("IV-6 : rappel alignement lag 2 (ideal points).")

align <- df[imp_iso3 %in% poles & !is.na(ipd),
            .(iso3 = exp_iso3, pole = imp_iso3, year, ipd)] |> unique()
align_wide <- dcast(align, iso3 + year ~ pole, value.var = "ipd")
setnames(align_wide, poles, c("ipd_usa", "ipd_chn", "ipd_rus"))

al_l2 <- copy(align_wide)
al_l2[, year_target := year + 2]
setnames(al_l2, c("ipd_usa", "ipd_chn", "ipd_rus"),
                c("ipd_usa_l2", "ipd_chn_l2", "ipd_rus_l2"))
al_l2[, year := NULL]

df_iv <- merge(df_iv, al_l2, by.x = c("exp_iso3", "year"),
               by.y = c("iso3", "year_target"), all.x = TRUE)
setnames(df_iv, c("ipd_usa_l2", "ipd_chn_l2", "ipd_rus_l2"),
                 c("exp_l2_usa", "exp_l2_chn", "exp_l2_rus"))
df_iv <- merge(df_iv, al_l2, by.x = c("imp_iso3", "year"),
               by.y = c("iso3", "year_target"), all.x = TRUE)
setnames(df_iv, c("ipd_usa_l2", "ipd_chn_l2", "ipd_rus_l2"),
                 c("imp_l2_usa", "imp_l2_chn", "imp_l2_rus"))

df_iv[, iv_align_l2 := sqrt(
  (exp_l2_usa - imp_l2_usa)^2 +
  (exp_l2_chn - imp_l2_chn)^2 +
  (exp_l2_rus - imp_l2_rus)^2)]

corr_l2 <- cor(df_iv$iv_align_l2, df_iv$ipd, use = "complete.obs")
cat(sprintf("  - Corr(iv_align_l2, ipd) : %.3f\n", corr_l2))

df_l2 <- df_iv[!is.na(iv_align_l2)]
fs_l2 <- feols(ipd ~ iv_align_l2 + rta | exp_year + imp_year + pair,
               data = df_l2, vcov = ~pair, notes = FALSE)
if (nobs(fs_l2) != nrow(df_l2)) {
  df_l2 <- df_l2[!is.na(predict(fs_l2, newdata = df_l2))]
  fs_l2 <- feols(ipd ~ iv_align_l2 + rta | exp_year + imp_year + pair,
                 data = df_l2, vcov = ~pair, notes = FALSE)
}
df_l2[, v_hat_l2 := residuals(fs_l2)]
ct_fs6 <- as.data.table(coeftable(fs_l2), keep.rownames = "term")
setnames(ct_fs6, 2:5, c("estimate", "se", "stat", "p"))
f_l2 <- ct_fs6[term == "iv_align_l2", stat^2]
cat(sprintf("  - First-stage F : %.1f\n", f_l2))

iv6 <- fepois(trade_value ~ ipd + v_hat_l2 + rta
              | exp_year + imp_year + pair,
              data = df_l2, vcov = ~pair)
print(summary(iv6))
add_result("IV-6: Alignment lag 2 (ideal pts)", iv6, f_l2, nobs(iv6), corr_l2)


# =============================================================================
# SECTION FINALE : Synthese
# =============================================================================

log_step("Synthese et rapport.")

# Construire le tableau de synthese
synthesis <- rbindlist(lapply(names(all_results), function(nm) {
  r <- all_results[[nm]]
  data.table(instrument = nm,
             ipd_coef    = r$ipd_coef,
             ipd_se      = r$ipd_se,
             ipd_pval    = r$ipd_pval,
             vhat_pval   = r$vhat_pval,
             F_stat      = r$F_stat,
             N           = r$N,
             corr_iv_ipd = r$corr_iv_ipd)
}))

# Ajouter le PPML reference en premiere ligne
ic_ref <- get_ipd_coef(ppml_ref)
ref_row <- data.table(instrument = "PPML reference (no IV)",
                      ipd_coef   = ic_ref$c,
                      ipd_se     = ic_ref$s,
                      ipd_pval   = ic_ref$p,
                      vhat_pval  = NA_real_,
                      F_stat     = NA_real_,
                      N          = nobs(ppml_ref),
                      corr_iv_ipd = NA_real_)
synthesis <- rbind(ref_row, synthesis)
synthesis[, ci_lo := ipd_coef - 1.96 * ipd_se]
synthesis[, ci_hi := ipd_coef + 1.96 * ipd_se]
synthesis[, sign := ifelse(ipd_coef < 0, "NEG", "POS")]

print(synthesis)
fwrite(synthesis, file.path(PATH_TAB, "tab_iv_all_synthesis.csv"))


# Figure : tous les coefs
synthesis[, iv_type := fcase(
  instrument == "PPML reference (no IV)", "Reference",
  grepl("ideal pts", instrument),         "Ideal-points based",
  default                                 = "Alternative source"
)]
synthesis[, instrument := factor(instrument, levels = rev(synthesis$instrument))]

pal_iv <- c("Reference" = "black",
            "Ideal-points based" = "#E41A1C",
            "Alternative source" = "#2166AC")

p_synth <- ggplot(synthesis, aes(ipd_coef, instrument, color = iv_type)) +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25) +
  geom_point(size = 3) +
  scale_color_manual(values = pal_iv, name = "IV type") +
  labs(title = "IPD Coefficient Across All IV Strategies",
       subtitle = paste("Control function approach with three-way FE.",
                        "95% CI cluster-robust by pair."),
       x = "IPD coefficient", y = NULL,
       caption = "Source: BACI-CEPII, Bailey et al. (2017)")
ggsave(file.path(PATH_FIG, "iv_fig_all_strategies.png"),
       p_synth, width = 12, height = 8, dpi = 300)


# ---- Rapport markdown auto-genere ------------------------------------------

n_neg <- sum(synthesis$ipd_coef < 0 & synthesis$instrument != "PPML reference (no IV)",
             na.rm = TRUE)
n_pos <- sum(synthesis$ipd_coef > 0 & synthesis$instrument != "PPML reference (no IV)",
             na.rm = TRUE)
n_sig <- sum(synthesis$ipd_pval < 0.05 & synthesis$instrument != "PPML reference (no IV)",
             na.rm = TRUE)
alt_rows <- synthesis[iv_type == "Alternative source"]
n_alt_neg <- sum(alt_rows$ipd_coef < 0, na.rm = TRUE)
n_alt_pos <- sum(alt_rows$ipd_coef > 0, na.rm = TRUE)
n_alt_sig <- sum(alt_rows$ipd_pval < 0.05, na.rm = TRUE)

report <- c(
  "# IV Exploration Report",
  "",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Reference: PPML without IV",
  sprintf("- IPD coefficient: **%.4f** (SE: %.4f, p: %.4f)",
          ic_ref$c, ic_ref$s, ic_ref$p),
  sprintf("- N observations: %s", format(nobs(ppml_ref), big.mark = ",")),
  "",
  "## Results by instrument",
  ""
)

for (i in seq_len(nrow(synthesis))) {
  row <- synthesis[i]
  if (row$instrument == "PPML reference (no IV)") next
  report <- c(report,
    sprintf("### %s", as.character(row$instrument)),
    sprintf("- IPD coefficient: **%.4f** (SE: %.4f, p: %.4f)",
            row$ipd_coef, row$ipd_se, row$ipd_pval),
    sprintf("- Sign: **%s** %s",
            row$sign,
            ifelse(row$ipd_pval < 0.05, "(significant at 5%)", "(not significant)")),
    sprintf("- First-stage F: %s %s",
            ifelse(is.na(row$F_stat), "n/a",
                   sprintf("%.1f", row$F_stat)),
            ifelse(!is.na(row$F_stat) & row$F_stat < 10, "(WEAK)", "(OK)")),
    sprintf("- Endogeneity test (v_hat p): %s %s",
            ifelse(is.na(row$vhat_pval), "n/a",
                   sprintf("%.4f", row$vhat_pval)),
            ifelse(!is.na(row$vhat_pval) & row$vhat_pval < 0.05,
                   "-> ENDOGENEITY", "-> No endogeneity")),
    sprintf("- Corr(instrument, IPD): %s",
            ifelse(is.na(row$corr_iv_ipd), "n/a",
                   sprintf("%.3f", row$corr_iv_ipd))),
    sprintf("- N: %s", format(row$N, big.mark = ",")),
    ""
  )
}

report <- c(report,
  "## Summary statistics",
  sprintf("- Total IV strategies tested: %d", nrow(synthesis) - 1),
  sprintf("- Negative IPD coefficient: %d / %d", n_neg, nrow(synthesis) - 1),
  sprintf("- Positive IPD coefficient: %d / %d", n_pos, nrow(synthesis) - 1),
  sprintf("- Significant at 5%%: %d / %d", n_sig, nrow(synthesis) - 1),
  "",
  "### Alternative-source IVs only",
  sprintf("- Alternative-source IVs tested: %d", nrow(alt_rows)),
  sprintf("- Negative: %d", n_alt_neg),
  sprintf("- Positive: %d", n_alt_pos),
  sprintf("- Significant at 5%%: %d", n_alt_sig),
  "",
  "## Interpretation guidance",
  "- If most alternative-source IVs give NEGATIVE coefficients:",
  "  the PPML negative result is supported; endogeneity bias is small.",
  "- If most alternative-source IVs give POSITIVE coefficients:",
  "  the positive sign from ideal-points-based IVs is likely real,",
  "  OR all instruments share an unobserved confound.",
  "- If results are MIXED across IV types:",
  "  IV identification is inconclusive for this question;",
  "  rely on three-way FE PPML + event-study evidence."
)

writeLines(report, file.path(PATH_TAB, "iv_synthesis_report.md"))
cat("\nReport :", file.path(PATH_TAB, "iv_synthesis_report.md"), "\n")


# ---- Console summary -------------------------------------------------------

cat("\n==========================================================\n")
cat("         RESUME COMPLET - TOUTES LES IV                   \n")
cat("==========================================================\n\n")

print(synthesis[, .(instrument,
                    ipd_coef = round(ipd_coef, 4),
                    ipd_pval = round(ipd_pval, 4),
                    F_stat   = round(F_stat, 1),
                    sign,
                    corr     = round(corr_iv_ipd, 3))])

cat(sprintf("\nTotal IV: %d | Negatifs: %d | Positifs: %d | Significatifs: %d\n",
            nrow(synthesis) - 1, n_neg, n_pos, n_sig))
cat(sprintf("Alternative-source : Neg=%d, Pos=%d, Sig=%d (sur %d)\n",
            n_alt_neg, n_alt_pos, n_alt_sig, nrow(alt_rows)))
cat("==========================================================\n")

log_step("Termine.")
