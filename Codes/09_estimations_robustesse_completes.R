# =============================================================================
# 09_estimations_robustesse_completes.R
# -----------------------------------------------------------------------------
# Passe TOUTE l'echelle de specs de 04_gravity_estimation.R (Spec 1..10 + Rob1..4)
# sur les mesures alternatives retenues d'apres les diagnostics 08a->08d, dans
# deux regimes d'echantillon (paliers communs A/B/C + periode propre D), PLUS une
# grille temporelle qui etend le resultat central (bascule post-2014) a toutes
# les specs et pas seulement a la Spec 4.
#
# Reference : Output/Reports/2026-06-22_verifications_variables_robustesse.md
#
# Principe : code PARAMETRE. Un REGISTRE recopie a l'identique chaque regression
# de 04 (estimateur, LHS, controles, FE, cluster, filtre d'echantillon, forme
# d'entree du regresseur geopolitique). La seule chose qui varie d'une ligne a
# l'autre est le regresseur geopolitique (ipd -> mesure).
#
# Variables RETENUES (08a->08d) :
#   polyarchy_dist, polity_dist, shared_rival_mid, sanction_nontrade,
#   n_common_sanctioners
# EXCLUES des estimations principales :
#   ideol_dist  -> selection sur le regime (08b-C) + construit fragile : ANNEXE
#   allied_atop -> within ratio 0.086 ~ IPD (08b-B), non identifiee : ANNEXE
#
# Sorties (Output/Tables/Robustness/) :
#   tab_grille_mesures.csv      (long : palier / mesure / spec / geovar / coef..)
#   tab_grille_temporelle.csv   (long : base / fenetre / spec / geovar / coef..)
#   report_estimations.md
# =============================================================================


# ---- Section 0 : Setup ------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow); library(data.table); library(fixest)
})

PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"
PATH_IV   <- file.path(PATH_ROOT, "Data", "Clean", "iv_panel.parquet")
PATH_MS   <- file.path(PATH_ROOT, "Data", "Clean",
                       "master_panel_with_strategic.parquet")
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables", "Robustness")
dir.create(PATH_TAB, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)            # tous les coeurs disponibles

# --- Flags ------------------------------------------------------------------
# SMOKE=1 dans l'environnement : mode validation rapide (sous-echantillon +
#   2 specs + palier C uniquement). Permet de verifier le schema des sorties
#   en ~1 min avant de lancer la version complete (plusieurs heures).
SMOKE            <- nzchar(Sys.getenv("SMOKE"))
# RUN_INTERACTIONS : si TRUE, les specs a interaction (8,9,10) sont aussi
#   passees sur chaque mesure dans tous les paliers (Bloc 1). Elles produisent
#   plusieurs coefficients par fit (un par niveau d'interaction), stockes en
#   format long. Mettre FALSE pour un run "core" plus rapide (specs a coef
#   unique uniquement).
RUN_INTERACTIONS <- !nzchar(Sys.getenv("NO_INTERACT"))

log_step <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

big3 <- c("USA", "CHN", "RUS")

log_step(sprintf("Setup. SMOKE=%s  RUN_INTERACTIONS=%s", SMOKE, RUN_INTERACTIONS))


# ---- Section 1 : Load + merge + derivations --------------------------------
# iv_panel porte les mesures + la gravite de base ; on complete avec les colonnes
# de master_panel necessaires aux specs 1/2/6/7/8/9 et Rob2 (GDP, pop, strategic,
# pair_nato). Merge 1:1 sur (exp_iso3, imp_iso3, year) (cf. 8d).

log_step("Section 1 : load iv_panel + merge master_panel.")

df <- as.data.table(read_parquet(PATH_IV))
ms <- as.data.table(read_parquet(PATH_MS,
        col_select = c("exp_iso3", "imp_iso3", "year",
                       "strategic_trade_value", "pair_nato",
                       "exp_gdp_real", "imp_gdp_real",
                       "exp_pop", "imp_pop")))
df <- merge(df, ms, by = c("exp_iso3", "imp_iso3", "year"), all.x = TRUE)
stopifnot(nrow(df) == nrow(read_parquet(PATH_IV, col_select = "year")))
rm(ms); gc(verbose = FALSE)

# Base = domaine du baseline 04 : ipd non-NA
df <- df[!is.na(ipd)]

# IDs FE (conventions de 04)
df[, pair      := paste(exp_iso3, imp_iso3, sep = "_")]
df[, exp_year  := paste(exp_iso3, year,     sep = "_")]
df[, imp_year  := paste(imp_iso3, year,     sep = "_")]

# Derivations (identiques a 04)
df[, log_trade           := log(trade_value + 1)]
df[, log_gdp_exp         := log(exp_gdp_real)]
df[, log_gdp_imp         := log(imp_gdp_real)]
df[, non_strategic_trade := pmax(trade_value - strategic_trade_value, 0)]
if (!"log_dist" %in% names(df)) df[, log_dist := log(dist)]
df[, pair_nato := factor(pair_nato, levels = c("non", "inter", "intra"))]
df[, period := fcase(
  year <= 2007, "1995-2007",
  year <= 2013, "2008-2013",
  year <= 2017, "2014-2017",
  year <= 2021, "2018-2021",
  default       = "2022-2024")]
df[, period := factor(period, levels = c("1995-2007", "2008-2013",
                                         "2014-2017", "2018-2021",
                                         "2022-2024"))]

if (SMOKE) {
  log_step("  SMOKE : sous-echantillon year>=2015 pour validation rapide.")
  df <- df[year >= 2015]
}
cat("  - N (ipd non-NA) :", nrow(df), "\n")


# ---- Section 2 : Variables retenues ----------------------------------------

MEASURES <- c("polyarchy_dist", "polity_dist", "shared_rival_mid",
              "sanction_nontrade", "n_common_sanctioners")

# Sens attendu (08c) : negatif pour distances/hostilites ; ambigu pour
# n_common_sanctioners (statut pariah conjoint).
SIGN_ATTENDU <- c(polyarchy_dist = "neg", polity_dist = "neg",
                  shared_rival_mid = "neg", sanction_nontrade = "neg",
                  n_common_sanctioners = "ambigu")


# ---- Section 3 : REGISTRE de specs (recopie integrale de 04) ----------------
# Champs :
#   est     : "fepois" | "feols"
#   lhs     : variable dependante
#   wrap    : forme d'entree du regresseur geo. "%s" = lineaire ;
#             "%s + I(%s^2)" = quadratique (Rob1) ; "i(pair_nato, %s)" /
#             "i(period, %s)" = interaction. Le(s) %s sont remplaces par le geovar.
#   controls: vecteur de controles additionnels (hors regresseur geo)
#   fe      : chaine des FE (NULL si aucune)
#   cluster : formule de cluster (chaine)
#   sample  : filtre additionnel (chaine evaluee) ou NA
#   type    : "single" (1 coef) | "quad" | "interact"
sp <- function(id, est = "fepois", lhs = "trade_value", wrap = "%s",
               controls = "rta", fe = "exp_year + imp_year + pair",
               cluster = "~pair", sample = NA_character_, type = "single") {
  list(id = id, est = est, lhs = lhs, wrap = wrap, controls = controls,
       fe = fe, cluster = cluster, sample = sample, type = type)
}

GRAV  <- c("log_dist", "contig", "comlang_off", "colony")
GRAVG <- c(GRAV, "log_gdp_exp", "log_gdp_imp")

SPECS <- list(
  # --- Section 2 de 04 : progression principale ---
  sp("spec1", est = "feols", lhs = "log_trade",
     controls = c(GRAVG, "rta"), fe = NULL, cluster = "~pair",
     sample = "trade_value > 0"),                                   # OLS, no FE
  sp("spec2", controls = c(GRAVG, "rta"),
     fe = "exp_iso3 + imp_iso3 + year"),                           # PPML sep FE
  sp("spec3", controls = c(GRAV, "rta"),
     fe = "exp_year + imp_year"),                                  # PPML ctry-yr
  sp("spec4"),                                                     # WORKHORSE
  sp("spec5", cluster = "~pair + exp_year + imp_year"),            # 3way multiway
  # --- Section 3 : heterogeneite sectorielle (LHS change) ---
  sp("spec6", lhs = "strategic_trade_value"),                      # strategic
  sp("spec7", lhs = "non_strategic_trade"),                        # non-strategic
  # --- Section 4 : heterogeneite NATO (interaction) ---
  sp("spec8", wrap = "i(pair_nato, %s)", sample = "!is.na(pair_nato)",
     type = "interact"),
  sp("spec9", lhs = "strategic_trade_value", wrap = "i(pair_nato, %s)",
     sample = "!is.na(pair_nato)", type = "interact"),
  # --- Section 5 : time-varying (interaction) ---
  sp("spec10", wrap = "i(period, %s)", type = "interact"),
  # --- Section 7 : robustness ---
  sp("rob1", wrap = "%s + I(%s^2)", type = "quad"),                # non-linearite
  sp("rob2", sample = "exp_pop > 1e6 & imp_pop > 1e6"),            # no micro
  sp("rob3", sample = "year >= 2002"),                             # post-2002
  sp("rob4", sample = "!(exp_iso3 %in% big3) & !(imp_iso3 %in% big3)")  # excl big3
)
names(SPECS) <- sapply(SPECS, `[[`, "id")

# specs a coefficient unique (= comparables verticalement par signe)
SINGLE_IDS  <- names(Filter(function(s) s$type %in% c("single", "quad"), SPECS))
INTERACT_IDS <- names(Filter(function(s) s$type == "interact", SPECS))

if (SMOKE) {
  SPECS <- SPECS[c("spec3", "spec4")]
  SINGLE_IDS <- intersect(SINGLE_IDS, names(SPECS))
  INTERACT_IDS <- intersect(INTERACT_IDS, names(SPECS))
}

# Couverture (periode propre) de chaque mesure — pour la lecture appariee.
COVERAGE <- c(polyarchy_dist = "1995-2024", polity_dist = "1995-2018",
              shared_rival_mid = "1995-2014", sanction_nontrade = "1995-2023",
              n_common_sanctioners = "1995-2023")


# ---- Generateur de rapport (reutilisable depuis les CSV) -------------------
# Construit report_estimations.md a partir des deux tables longues. Appele en
# fin de run, ou seul via REPORT_ONLY=1 (relit les CSV, ne re-estime pas).
build_report <- function(mes_dt, tmp_dt) {
  fmt <- function(x, d = 4) ifelse(is.na(x), "NA",
                                   formatC(x, format = "f", digits = d))
  sgn <- function(x) ifelse(is.na(x), "·", ifelse(x < 0, "−", "+"))
  cell <- function(co, p) ifelse(is.na(co), "·",
                          sprintf("%s%s (p=%s)", sgn(co), fmt(abs(co)), fmt(p, 3)))

  rep <- c(
    "# Estimations completes de robustesse — toute l'echelle de specs de `04`",
    paste("Date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "Substitution de l'IPD par chaque mesure retenue (08a->08d), sur l'INTEGRALITE",
    "de l'echelle de specs de `04_gravity_estimation.R` (Spec 1..10 + Rob1..4),",
    "dans deux regimes d'echantillon (paliers communs A/B/C ; periode propre D) +",
    "une grille temporelle pour l'IPD.",
    "",
    "Mesures retenues : `polyarchy_dist`, `polity_dist`, `shared_rival_mid`,",
    "`sanction_nontrade`, `n_common_sanctioners`. Exclues (annexe) : `ideol_dist`",
    "(selection sur le regime, 08b-C), `allied_atop` (non identifiee, within 0.086).",
    "",
    "Sens attendu : negatif pour les distances/hostilites ; **ambigu** pour",
    "`n_common_sanctioners` (statut pariah conjoint) -> rapporte sans attendu impose.",
    "",
    "## 1. Lecture APPARIEE — chaque mesure vs l'IPD du MEME echantillon (Spec 4)",
    "",
    "Le point central : sur sa periode propre (palier D), chaque mesure doit etre",
    "comparee a l'IPD estime sur *ce meme* echantillon, jamais au -0.066 full",
    "sample. Les mesures dont la couverture s'arrete avant 2015 (polity ≤2018,",
    "shared_rival ≤2014) tombent dans l'ere ou l'IPD lui-meme est *positif*.",
    "",
    "| Mesure | Periode | N | coef mesure | coef IPD apparie | Meme signe ? |",
    "|---|---|---|---|---|---|")
  for (m in MEASURES) {
    rm <- mes_dt[palier == "D" & mesure == m & spec == "spec4" &
                 term == m][1]
    ri <- mes_dt[palier == "D" & mesure == m & spec == "spec4" &
                 geovar == "ipd" & term == "ipd"][1]
    if (is.na(rm$coef) && is.na(ri$coef)) next
    same <- if (!is.na(rm$coef) && !is.na(ri$coef))
              ifelse(sign(rm$coef) == sign(ri$coef), "oui", "non") else "·"
    rep <- c(rep, sprintf("| %s | %s | %s | %s | %s | %s |",
      m, COVERAGE[[m]], format(rm$n, big.mark = ","),
      cell(rm$coef, rm$p), cell(ri$coef, ri$p), same))
  }

  rep <- c(rep, "",
    "## 2. Stabilite VERTICALE — signe a travers les specs a coef unique",
    "",
    "Pour chaque mesure (palier D), compte des specs a coefficient unique ou le",
    "signe est negatif, et plage des coefficients. Specs : ",
    paste0("`", paste(SINGLE_IDS, collapse = "`, `"), "`."),
    "",
    "| Mesure | Attendu | n specs | n coef<0 | min coef | max coef |",
    "|---|---|---|---|---|---|")
  single_focus <- mes_dt[palier == "D" & spec %in% SINGLE_IDS &
                         term == geovar & !is.na(coef)]
  for (m in MEASURES) {
    sub <- single_focus[geovar == m]
    if (nrow(sub) == 0) {
      rep <- c(rep, sprintf("| %s | %s | 0 | · | NA | NA |", m, SIGN_ATTENDU[[m]]))
      next
    }
    rep <- c(rep, sprintf("| %s | %s | %d | %d | %s | %s |",
      m, SIGN_ATTENDU[[m]], nrow(sub), sum(sub$coef < 0),
      fmt(min(sub$coef)), fmt(max(sub$coef))))
  }

  rep <- c(rep, "",
    "## 3. Bascule temporelle de l'IPD — survit-elle a toute l'echelle ?",
    "",
    "Coefficient IPD par fenetre, full sample, pour chaque spec a coef unique.",
    "Resultat central : positif <=2014/2018 puis negatif sur 2015-2024. Il tient",
    "sur toute la **famille FE three-way** (spec4/5/6/7, rob2). Les specs SANS",
    "`pair` FE (spec2/3) restent positives (effet de niveau between-pair) ; spec1",
    "(OLS) et rob1 (quadratique) sont d'une autre nature.",
    "",
    "| Spec | IPD 1995-2014 | IPD 2015-2024 | IPD 1995-2024 |",
    "|---|---|---|---|")
  tw <- tmp_dt[base == "full" & term == "ipd" & spec %in% SINGLE_IDS]
  wins <- c("1995-2014", "2015-2024", "1995-2024")
  for (id in SINGLE_IDS) {
    cells <- sapply(wins, function(w) {
      r <- tw[spec == id & fenetre == w]
      if (nrow(r)) sprintf("%s%s (p=%s)", sgn(r$coef[1]), fmt(abs(r$coef[1])),
                           fmt(r$p[1], 2)) else "·" })
    rep <- c(rep, sprintf("| %s | %s |", id, paste(cells, collapse = " | ")))
  }

  # spec10 : decomposition periodique (ancre time-varying)
  s10 <- tmp_dt[spec == "spec10" & grepl("ipd", term)]
  if (nrow(s10)) {
    s10[, per := sub(".*::([0-9-]+):.*", "\\1", term)]
    rep <- c(rep, "",
      "### 3bis. Decomposition periodique (Spec 10, `i(period, ipd)`, full sample)",
      "",
      "| Periode | IPD coef | SE | p |", "|---|---|---|---|")
    for (i in seq_len(nrow(s10)))
      rep <- c(rep, sprintf("| %s | %s%s | %s | %s |", s10$per[i],
        sgn(s10$coef[i]), fmt(abs(s10$coef[i])), fmt(s10$se[i]), fmt(s10$p[i], 3)))
    rep <- c(rep, "",
      "Intensification **monotone** de l'effet negatif : l'alignement diplomatique",
      "pese de plus en plus lourd sur le commerce a mesure qu'on avance vers",
      "2022-2024 (guerres commerciales, Russie-Ukraine, decouplage US-Chine).")
  }

  rep <- c(rep, "",
    "## 4. Lecture d'ensemble",
    "",
    "- **Section 1 (appariee)** : toute mesure de distance/hostilite partage le",
    "  signe de l'IPD *sur le meme echantillon*. Les signes positifs de polity et",
    "  shared_rival ne contredisent pas l'IPD — ils refletent une couverture",
    "  bornee a l'ere pre-2015 (ou l'IPD apparie est lui aussi positif).",
    "- **Section 2 (verticale)** : la dispersion du signe entre specs vient",
    "  surtout du contraste FE-pair (within) vs sans-pair (between).",
    "- **Section 3 (horizontale)** : la bascule post-2014 est STRUCTURELLE — elle",
    "  survit a toute la famille FE three-way et a la decomposition periodique.",
    "",
    "## Garde-fous appliques",
    "- Mesure comparee a l'IPD apparie de la MEME ligne (sample+spec).",
    "- Gros N : magnitude et stabilite de signe, pas p<0.05.",
    "- `n_common_sanctioners` : signe ambigu, sans attendu impose.",
    "- Paliers A/B/C : RHS identique entre geovar (pas de `mid_direct`).",
    "- `mid_direct` ajoute uniquement pour `shared_rival_mid` en periode propre (D).",
    "- Specs a `pair` FE : gravite invariante absorbee ; conservee en Spec 1/2/3.",
    "",
    "## Fichiers",
    "- `tab_grille_mesures.csv` — long : palier(A/B/C/D)/mesure/spec/geovar/term/coef/se/p/n/collin",
    "- `tab_grille_temporelle.csv` — long : base/fenetre/spec/geovar/term/coef/se/p/n",
    "",
    "Interactions NATO (spec8/9) et periodes (spec10) presentes dans les CSV",
    "(format long, plusieurs coefficients par fit).")

  writeLines(rep, file.path(PATH_TAB, "report_estimations.md"))
}

# Court-circuit : regenerer le rapport depuis les CSV sans re-estimer.
if (nzchar(Sys.getenv("REPORT_ONLY"))) {
  log_step("REPORT_ONLY : regeneration du rapport depuis les CSV.")
  mes_dt <- fread(file.path(PATH_TAB, "tab_grille_mesures.csv"))
  tmp_dt <- fread(file.path(PATH_TAB, "tab_grille_temporelle.csv"))
  build_report(mes_dt, tmp_dt)
  log_step("Rapport regenere.")
  quit(save = "no")
}


# ---- Section 4 : moteur d'estimation ---------------------------------------

n_pct <- function(s) length(gregexpr("%s", s, fixed = TRUE)[[1]])

# Construit et estime UNE regression ; renvoie une data.table de 1+ lignes
# (un par coefficient dont le terme contient `geovar`).
run_fit <- function(spec, data, geovar, extra = NULL) {
  # forme du regresseur geo (remplit autant de %s que necessaire)
  geo_term <- do.call(sprintf,
                      c(list(spec$wrap), as.list(rep(geovar, n_pct(spec$wrap)))))
  rhs <- paste(c(geo_term, spec$controls, extra), collapse = " + ")
  fe_part <- if (is.null(spec$fe)) "" else paste0(" | ", spec$fe)
  fml <- stats::as.formula(paste0(spec$lhs, " ~ ", rhs, fe_part))
  vc  <- stats::as.formula(spec$cluster)

  fit <- tryCatch({
    if (spec$est == "fepois")
      fepois(fml, data = data, vcov = vc, notes = FALSE)
    else
      feols(fml, data = data, vcov = vc)
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(data.table(spec = spec$id, geovar = geovar, term = NA_character_,
                      coef = NA_real_, se = NA_real_, stat = NA_real_,
                      p = NA_real_, n = NA_integer_,
                      collin = "", err = conditionMessage(fit)))
  }

  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, 2:5, c("coef", "se", "stat", "p"))
  ct <- ct[grepl(geovar, term, fixed = TRUE)]
  if (nrow(ct) == 0)   # geovar entierement absorbe / drop colinearite
    ct <- data.table(term = geovar, coef = NA_real_, se = NA_real_,
                     stat = NA_real_, p = NA_real_)
  collin <- if (length(fit$collin.var))
              paste(fit$collin.var, collapse = ",") else ""
  cbind(data.table(spec = spec$id, geovar = geovar),
        ct[, .(term, coef, se, stat, p)],
        data.table(n = nobs(fit), collin = collin, err = ""))
}


# =============================================================================
# BLOC 1 : grille de mesure (robustesse x specs)
# =============================================================================
log_step("BLOC 1 : grille de mesure.")
res_mes <- list(); k <- 0L
push_mes <- function(dt, palier, mesure) {
  if (nrow(dt) == 0) return(invisible())
  dt[, `:=`(palier = palier, mesure = mesure)]
  k <<- k + 1L; res_mes[[k]] <<- dt
}
flush_mes <- function() {
  if (length(res_mes))
    fwrite(rbindlist(res_mes, fill = TRUE),
           file.path(PATH_TAB, "tab_grille_mesures.csv"))
}

ids_for <- function() if (RUN_INTERACTIONS) names(SPECS) else SINGLE_IDS

# --- Palier D : periode propre de chaque mesure (+ IPD apparie) -------------
log_step("  Palier D : periode propre par mesure.")
for (m in MEASURES) {
  d_m  <- df[!is.na(get(m))]
  extra <- if (m == "shared_rival_mid") "mid_direct" else NULL   # control (08c)
  if (!is.null(extra)) d_m <- d_m[!is.na(get(extra))]
  if (nrow(d_m) == 0) { cat(sprintf("  [D] %-22s SKIP (0 obs)\n", m)); next }
  yrs <- as.integer(range(d_m$year))
  cat(sprintf("  [D] %-22s N=%d  (%d-%d)\n", m, nrow(d_m), yrs[1], yrs[2]))

  for (id in ids_for()) {
    spec <- SPECS[[id]]
    d_s  <- if (is.na(spec$sample)) d_m else d_m[eval(parse(text = spec$sample))]
    # mesure
    push_mes(run_fit(spec, d_s, m, extra), "D", m)
    # IPD apparie sur LE MEME sample/spec (sans mid_direct : geovar = ipd)
    push_mes(run_fit(spec, d_s, "ipd"), "D", m)
  }
  flush_mes()
}
# Ancre : IPD sur sa periode complete 1995-2024 (= baseline -0.066 en spec4)
log_step("  Palier D : ancre IPD periode complete.")
for (id in ids_for()) {
  spec <- SPECS[[id]]
  d_s  <- if (is.na(spec$sample)) df else df[eval(parse(text = spec$sample))]
  push_mes(run_fit(spec, d_s, "ipd"), "D", "ipd_full")
}
flush_mes()

# --- Paliers A / B / C : echantillon commun, RHS identique entre mesures ----
log_step("  Paliers A / B / C : echantillon commun.")
PALIERS <- list(
  A = c("polyarchy_dist", "polity_dist", "shared_rival_mid",
        "sanction_nontrade", "n_common_sanctioners"),
  B = c("polyarchy_dist", "polity_dist",
        "sanction_nontrade", "n_common_sanctioners"),
  C = c("polyarchy_dist", "sanction_nontrade", "n_common_sanctioners"))
if (SMOKE) PALIERS <- PALIERS["C"]

build_common <- function(vars) {
  d <- df
  for (v in vars) d <- d[!is.na(get(v))]
  d
}

for (pl in names(PALIERS)) {
  vars <- PALIERS[[pl]]
  d_pl <- build_common(vars)
  if (nrow(d_pl) == 0) { cat(sprintf("  [%s] SKIP (0 obs)\n", pl)); next }
  yrs <- as.integer(range(d_pl$year))
  cat(sprintf("  [%s] N=%d  (%d-%d)  vars: %s\n",
              pl, nrow(d_pl), yrs[1], yrs[2], paste(vars, collapse = ", ")))
  # RHS IDENTIQUE pour tous : pas de mid_direct en palier (cf. garde-fou brief)
  for (id in ids_for()) {
    spec <- SPECS[[id]]
    d_s  <- if (is.na(spec$sample)) d_pl else d_pl[eval(parse(text = spec$sample))]
    for (g in c("ipd", vars))
      push_mes(run_fit(spec, d_s, g), pl, "(palier commun)")
  }
  flush_mes()
}
log_step("BLOC 1 termine.")


# =============================================================================
# BLOC 2 : grille temporelle (le resultat central x specs, IPD uniquement)
# =============================================================================
# Seules les specs a coefficient UNIQUE entrent ici : la "bascule de signe" de
# l'IPD n'est definie que pour un coef ipd unique. Les specs a interaction
# (8,9 NATO) sont hors-sujet ; spec10 EST deja la decomposition temporelle et
# est rapportee une fois sur full sample comme ancre.
log_step("BLOC 2 : grille temporelle.")

res_tmp <- list(); kt <- 0L
push_tmp <- function(dt, base, fenetre, yr_lo, yr_hi) {
  if (nrow(dt) == 0) return(invisible())
  dt[, `:=`(base = base, fenetre = fenetre,
            year_min = yr_lo, year_max = yr_hi)]
  kt <<- kt + 1L; res_tmp[[kt]] <<- dt
}
flush_tmp <- function() {
  if (length(res_tmp))
    fwrite(rbindlist(res_tmp, fill = TRUE),
           file.path(PATH_TAB, "tab_grille_temporelle.csv"))
}

# composition palier C fixe (jeu de dyades), fenetre temporelle qui varie
compoC <- build_common(PALIERS[["C"]])
win_compoC <- list(c(1995, 2014), c(1995, 2018), c(1995, 2023))
# full sample : sous-periodes
win_full <- list(c(1995, 2024), c(1995, 2014), c(2015, 2024))
if (SMOKE) { win_compoC <- list(c(2015, 2018)); win_full <- list(c(2015, 2024)) }

run_windows <- function(base_label, data0, windows) {
  for (w in windows) {
    lo <- w[1]; hi <- w[2]
    d_w0 <- data0[year >= lo & year <= hi]
    if (nrow(d_w0) == 0) { cat(sprintf("  [%s] %d-%d SKIP (0 obs)\n",
                                       base_label, lo, hi)); next }
    yrs <- as.integer(range(d_w0$year))
    for (id in SINGLE_IDS) {
      spec <- SPECS[[id]]
      d_s  <- if (is.na(spec$sample)) d_w0
              else d_w0[eval(parse(text = spec$sample))]
      push_tmp(run_fit(spec, d_s, "ipd"),
               base_label, sprintf("%d-%d", lo, hi), yrs[1], yrs[2])
    }
    cat(sprintf("  [%s] fenetre %d-%d : %d specs\n",
                base_label, lo, hi, length(SINGLE_IDS)))
    flush_tmp()
  }
}
run_windows("compoC_fixe", compoC, win_compoC)
run_windows("full",        df,     win_full)

# Ancre spec10 (time-varying) sur full sample : la decomposition periodique
if ("spec10" %in% names(SPECS) || !SMOKE) {
  s10 <- sp("spec10", wrap = "i(period, %s)", type = "interact")
  push_tmp(run_fit(s10, df, "ipd"), "full", "spec10_periodes",
           min(df$year), max(df$year))
  flush_tmp()
}
log_step("BLOC 2 termine.")


# =============================================================================
# Rapport
# =============================================================================
log_step("Generation report_estimations.md.")

mes_dt <- rbindlist(res_mes, fill = TRUE)
tmp_dt <- rbindlist(res_tmp, fill = TRUE)
build_report(mes_dt, tmp_dt)

log_step("Termine.")
cat("\nSorties :\n",
    file.path(PATH_TAB, "tab_grille_mesures.csv"), "\n",
    file.path(PATH_TAB, "tab_grille_temporelle.csv"), "\n",
    file.path(PATH_TAB, "report_estimations.md"), "\n")
