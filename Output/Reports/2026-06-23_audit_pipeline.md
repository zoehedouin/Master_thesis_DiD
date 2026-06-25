# Audit du pipeline — Codes, Données, Sorties

*Date : 2026-06-23. Rapport descriptif construit par lecture effective de
`Codes/`, `Output/`, `Data/`. Les valeurs citées sont lues dans les fichiers
sources ; chaque affirmation renvoie à un chemin. Aucune interprétation des
résultats.*

---

## 1. Vue d'ensemble

### 1.1 Question et stratégie (telles qu'elles ressortent du code et des rapports)

Question (en-têtes des scripts et `Output/Reports/2026-06-16_recap_analyse.md`) :
effet de la **distance géopolitique** — IPD, *Ideal Point Distance* de Bailey-
Strezhnev-Voeten, valeur absolue de la différence des ideal points de vote à
l'Assemblée générale de l'ONU — sur le **commerce bilatéral** (exports BACI).
Spécification principale : gravité **PPML** à effets fixes *three-way*
`exp_iso3×year + imp_iso3×year + pair`, clustering par paire (script
`04_gravity_estimation.R`, Spec 4). Une stratégie **IV / control-function** a
été explorée (scripts `05`, `05c`, `07`, `07b`, `07c`) ; une phase de
**robustesse de mesure** a suivi (`08a`–`8d`, `09`).

### 1.2 Arborescence (annotée)

```
Master_thesis/
├── Codes/                  17 scripts R (01 → 09 ; voir §3)
├── Data/
│   ├── Raw/                sources brutes (BACI, Gravity, IPD, IV/*, Map, DESTA)
│   └── Clean/              panels construits (.parquet + .csv) + caches + README
├── Output/
│   ├── Figures/            {Trade, Geopolitics, Interaction, Estimation,
│   │                        Estimation_IV} — 38 PNG
│   ├── Maps/               {Trade(1), Geopolitics(3)} — 4 PNG
│   ├── Tables/             {Trade, Geopolitics, Interaction, Estimation,
│   │                        Estimation_IV, Estimation_IV_alternative,
│   │                        Robustness(+_archive)} — CSV + TeX + .md
│   └── Reports/            VARIABLES.md, 2026-06-16_recap_analyse.md,
│                           2026-06-22_verifications_variables_robustesse.md,
│                           2026-06-23_audit_pipeline.md (ce fichier)
└── Info/                   2 PDF (mémos méthodo gravity PPML / IRF local proj.)
```

### 1.3 Sources de données principales (`Data/Raw/`)

| Source | Chemin racine | Usage |
|---|---|---|
| BACI HS92 V202601 | `Data/Raw/BACI_HS92_V202601/` | flux bilatéraux HS6, 1995–2024 (30 fichiers annuels) |
| CEPII Gravity V202211 | `Data/Raw/Gravity/Gravity_V202211.csv` | distance, contiguïté, langue, colonie (time-invariant) |
| IPD Bailey et al. 1946–2025 | `Data/Raw/IPD/IdealPointDyads1946-2025.csv` | `AbsIdealDiff` → `ipd` |
| DESTA | `Data/Raw/desta_list_of_treaties_02_03_dyads.xlsx` | RTA dyadiques |
| World Bank WDI | API (`WDI`, pas de fichier) | PIB, pop, inflation, déflateur |
| V-Dem v16 | `Data/Raw/IV/V-Dem/V-Dem-CY-Full+Others-v16.csv` | `v2x_polyarchy` |
| DPI 2023 | `Data/Raw/IV/the-database-of-political-institutions/DPI 2023 (CSV Version)/DPI 2023 CSV Version.csv` | `execrlc` |
| Polity5 v2018 | `Data/Raw/IV/Polity5.xls` (sheet `p5v2018`) | `polity2` |
| ATOP v5.1 | `Data/Raw/IV/ATOP/atop5_1ddyr.csv` | `atopally` |
| Dyadic MID v4.03 | `Data/Raw/IV/dyadic_mid/dyadic_mid_4.03.csv` | MID dyad-année |
| GSDB v4 | `Data/Raw/IV/gsdb_v4/GSDB_V4_dyadic.dta` | sanctions |
| Natural Earth 50m | `Data/Raw/Map/ne_50m_admin_0_countries.shp` | fonds de carte |

---

## 2. Pipeline de données

### 2.1 Construction des panels

**`01_build_master_panel.R`** → `Data/Clean/master_panel.parquet` (+ `.csv`,
+ `README.md`). Lit BACI (agrégation HS6 → paire-année, somme de `v`), crosswalk
ISO num→ISO3 via `country_codes_V202601.csv`, **expansion cartésienne** sur les
pays présents ≥ 5 ans (`MIN_YEARS_ACTIVE <- 5L`) pour densifier les zéros, merge
Gravity (collapse *last non-NA* par paire), IPD (symétrisé undirected), DESTA
(`entry_type %in% c("base_treaty","accession")`, `rta := as.integer(!is.na(rta_start) & year >= rta_start)`),
NATO (liste hardcodée 32 membres), WDI (5 indicateurs). Cache :
`Data/Clean/_baci_agg_cache.parquet`.

**`02_build_strategic_panel.R`** → `Data/Clean/master_panel_with_strategic.parquet`
(+ `.csv`, + `README_strategic.md`). Relit BACI au niveau HS6, filtre 186 codes
HS6 « stratégiques » (6 secteurs hardcodés : semiconductors 19, telecom_5g 13,
green_transition 23, pharmaceuticals 41, critical_minerals 44, defense 49 ; 3
doublons `852691, 852692, 854140`), agrège par paire-année, merge sur le master.
`strategic_trade_value` (NA→0), `strategic_trade_share = strategic_trade_value/trade_value`
(NA si `trade_value==0`). Cache : `Data/Clean/_baci_strategic_cache.parquet`.

**`06_build_geopol_measures.R`** → `Data/Clean/iv_panel.parquet`. Squelette =
master réduit (`exp_iso3, imp_iso3, year, trade_value, ipd, rta, dist, contig,
comlang_off, colony`), enrichi de 12 mesures dyadiques (voir
`Output/Reports/VARIABLES.md`).

### 2.2 Dimensions du panel final (lues dans `Data/Clean/README.md` et `_strategic.md`)

| Fichier | Obs | Années | Pays | Paires dir. | trade>0 | trade=0 | Cols |
|---|---|---|---|---|---|---|---|
| `master_panel(.parquet)` | 1 593 900 | 1995–2024 | 231×231 | 53 130 | 870 753 (54.6%) | 723 147 (45.4%) | 28 |
| `master_panel_with_strategic` | 1 593 900 | 1995–2024 | 231×231 | 53 130 | idem | idem | 30 |
| `iv_panel.parquet` | 1 593 900 | 1995–2024 | 231×231 | 53 130 | idem | idem | 23 |

`master_panel_with_strategic` : `strategic_trade_value > 0` = 434 806 (27.3 %)
(`README_strategic.md`). Sur le panel, `ipd` non-NA = 1 031 492 (log de
`09_estimations_robustesse_completes.R` ; cf. `tab_diagnostics.csv` : N estimé
Spec 4 après FE = 965 872).

### 2.3 Variables construites (par script)

- **`01`** : `trade_value`, `ipd`, `rta`, `exp_nato`, `imp_nato`, `pair_nato`
  (`intra`/`inter`/`non`), `dist`, `distw_harm`, `dist_cap`, `contig`,
  `comlang_off`, `comlang_eth`, `colony`, `comcol`, `comrelig`, et
  `exp_/imp_{gdp_nominal,gdp_real,inflation,deflator,pop}` (WDI :
  `NY.GDP.MKTP.CD`, `NY.GDP.MKTP.KD`, `FP.CPI.TOTL.ZG`, `NY.GDP.DEFL.KD.ZG`,
  `SP.POP.TOTL`).
- **`02`** : `strategic_trade_value`, `strategic_trade_share` (réf. Aiyar et al.
  2024, IMF SDN/2023/001).
- **`06`** : `polyarchy_dist`, `joint_dem_vdem`, `ideol_dist`, `polity_dist`,
  `allied_atop`, `shared_ally_atop`, `mid_direct`, `shared_rival_mid`,
  `sanction_any`, `sanction_trade`, `sanction_nontrade`, `n_common_sanctioners`.
  Définitions/formules verbatim : `Output/Reports/VARIABLES.md`.

---

## 3. Inventaire des scripts (ordre d'exécution)

Numérotation observée : `01, 02, 03a, 03b, 03c, 04, 05, 05c, 06, 07, 07b, 07c,
08a, 08b, 08c, 8d, 09`. Aucun `05b`, aucun `08d` (le fichier est `8d_*`, sans
zéro initial — cf. §7). Aucun script n'est dans un dossier `_archive/`.

### 3.1 `01_build_master_panel.R`
- **Rôle** : construit le panel directionnel master (BACI × Gravity × IPD × DESTA × NATO × WDI).
- **Inputs** : `Data/Raw/BACI_HS92_V202601/country_codes_V202601.csv` ; `Data/Raw/BACI_HS92_V202601/BACI_HS92_Y*_V202601.csv` ; `Data/Raw/Gravity/Gravity_V202211.csv` ; `Data/Raw/Gravity/Countries_V202211.csv` ; `Data/Raw/IPD/IdealPointDyads1946-2025.csv` ; `Data/Raw/desta_list_of_treaties_02_03_dyads.xlsx` ; WDI (API).
- **Outputs** : `Data/Clean/master_panel.parquet`, `Data/Clean/master_panel.csv`, `Data/Clean/README.md`, `Data/Clean/_baci_agg_cache.parquet`.
- **Décisions clés** : `MIN_YEARS_ACTIVE <- 5L` (expansion cartésienne) ; `YEAR_MIN <- 1995L`, `YEAR_MAX <- 2024L` ; Gravity `last non-NA` par paire ; IPD symétrisé ; DESTA `entry_type %in% c("base_treaty","accession")` ; NATO hardcodé (32 membres, années d'adhésion 1949→2024) ; self-flows exclus ; zéros imputés `trade_value := 0`.

### 3.2 `02_build_strategic_panel.R`
- **Rôle** : ajoute `strategic_trade_value` / `strategic_trade_share` (186 codes HS6 stratégiques) au master.
- **Inputs** : `Data/Clean/master_panel.parquet` ; `Data/Raw/BACI_HS92_V202601/BACI_HS92_Y*_V202601.csv` ; `country_codes_V202601.csv`.
- **Outputs** : `Data/Clean/master_panel_with_strategic.parquet` (+ `.csv`), `Data/Clean/README_strategic.md`, `Data/Clean/_baci_strategic_cache.parquet`.
- **Décisions clés** : 6 secteurs hardcodés ; padding `sprintf("%06d", k)` ; NA→0 ; `share` NA si `trade_value==0`.

### 3.3 `03a_desc_trade.R`
- **Rôle** : descriptives commerce (9 figures + 3 tables).
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet` ; `rnaturalearth::ne_countries(scale=50)`.
- **Outputs** : `Output/Figures/Trade/fig0{1-5,7-9}*.png`, `Output/Maps/Trade/fig06_map_openness.png`, `Output/Tables/Trade/tab0{1,2,3}*.csv/.tex`.
- **Méthode** : aucune estimation. Déflateur USA base 2015=100 (cumul de taux) ; périodes `1995-2004 / 2005-2014 / 2015-2024`.

### 3.4 `03b_desc_geopolitics.R`
- **Rôle** : descriptives IPD + NATO (4 figures + 3 cartes + 3 tables).
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet` ; `ne_countries(scale=50)`.
- **Outputs** : `Output/Figures/Geopolitics/geop_fig0{1,2,3,4,8,9,10}*.png` ; `Output/Maps/Geopolitics/geop_fig0{5,6,7}*.png` ; `Output/Tables/Geopolitics/geop_tab0{1,2,3}*.csv/.tex`.
- **Méthode** : aucune estimation (un `hclust` ward.D2 pour ordonner le heatmap ; `geom_smooth(lm)` fig 10). `panel_pair <- panel[exp_iso3 < imp_iso3]` (dédup undirected).

### 3.5 `03c_desc_interaction.R`
- **Rôle** : descriptives interaction commerce × géopolitique × NATO (11 figures + 4 tables).
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet`.
- **Outputs** : `Output/Figures/Interaction/inter_fig0{1-11}*.png` ; `Output/Tables/Interaction/inter_tab0{1-4}*.csv/.tex`. (`PATH_MAP` déclaré, aucune carte écrite.)
- **Méthode (calls verbatim)** : résidualisation `feols(log_trade ~ 1 | exp_year + imp_year + pair)` et `feols(ipd ~ 1 | exp_year + imp_year + pair)` (fig 2) ; `lm(resid_trade ~ resid_ipd)` ; tab04 = 4 spécifications de corrélation partielle (raw ; conditional dist+GDP ; within-pair ; within-pair-year). Event-studies indexées à 2022/2018/2014, split médian de l'IPD pré-événement.

### 3.6 `04_gravity_estimation.R`
- **Rôle** : estimation principale PPML 3-way FE + hétérogénéités + robustesses.
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet`.
- **Outputs** : `Output/Tables/Estimation/tab_{main_progression, diagnostics, strategic_hetero, nato_hetero, timevarying, robustness, se_comparison}.{csv,tex}` ; `Output/Figures/Estimation/est_fig01_ipd_timevarying.png`.
- **Spécifications exactes (verbatim)** :
  - Spec 1 : `feols(log_trade ~ ipd + log_dist + contig + comlang_off + colony + log_gdp_exp + log_gdp_imp + rta, data = df[trade_value>0 & !is.na(ipd)], vcov=~pair)`
  - Spec 2 : `fepois(trade_value ~ ipd + log_dist + contig + comlang_off + colony + log_gdp_exp + log_gdp_imp + rta | exp_iso3 + imp_iso3 + year, vcov=~pair)`
  - Spec 3 : `fepois(trade_value ~ ipd + log_dist + contig + comlang_off + colony + rta | exp_year + imp_year, vcov=~pair)`
  - Spec 4 : `fepois(trade_value ~ ipd + rta | exp_year + imp_year + pair, vcov=~pair)`
  - Spec 5 : Spec 4 avec `vcov = ~pair + exp_year + imp_year`
  - Spec 6 : `fepois(strategic_trade_value ~ ipd + rta | exp_year+imp_year+pair, vcov=~pair)`
  - Spec 7 : `fepois(non_strategic_trade ~ ipd + rta | exp_year+imp_year+pair, vcov=~pair)` (`non_strategic_trade := pmax(trade_value - strategic_trade_value, 0)`)
  - Spec 8 : `fepois(trade_value ~ i(pair_nato, ipd) + rta | exp_year+imp_year+pair, vcov=~pair)`
  - Spec 9 : `fepois(strategic_trade_value ~ i(pair_nato, ipd) + rta | ..., vcov=~pair)`
  - Spec 10 : `fepois(trade_value ~ i(period, ipd) + rta | ..., vcov=~pair)` (`period` = 1995-2007/2008-2013/2014-2017/2018-2021/2022-2024)
  - Rob 1 : `fepois(trade_value ~ ipd + ipd_sq + rta | ...)` ; Rob 2 : `... data=df[... & exp_pop>1e6 & imp_pop>1e6]` ; Rob 3 : `... data=df[... & year>=2002]` ; Rob 4 : `... data=df[... & !(exp_iso3 %in% c("USA","CHN","RUS")) & !(imp_iso3 %in% ...)]`.
- **Section 1** : diagnostic de variance within de l'IPD : `feols(ipd ~ 1 | exp_year + imp_year + pair)`, ratio within/total.

### 3.7 `05_gravity_iv.R`
- **Rôle** : control-function IV (2SRI), instrument = distance euclidienne d'alignement aux pôles USA/CHN/RUS laggée (spec principale lag 2), paires impliquant un pôle exclues.
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet`.
- **Outputs** : `Output/Tables/Estimation_IV/tab_iv_{first_stage_lags, main, coef_by_lag, strategic, nato, timevarying, instrument_robustness}.{csv,tex}` ; `Output/Figures/Estimation_IV/iv_fig0{0,1,2}*.png`.
- **Spécification (verbatim)** : 1er stage `feols(ipd ~ instrument_l2 + rta | exp_year+imp_year+pair, vcov=~pair)` → `v_hat := residuals(...)` ; 2nd stage `fepois(trade_value ~ ipd + v_hat + rta | ..., vcov=~pair)`. Échantillon : `df_iv <- df[!(exp_iso3 %in% poles) & !(imp_iso3 %in% poles) & !is.na(ipd)]`, `poles <- c("USA","CHN","RUS")`. Lags testés `c(0,1,2,3,5)`. Instrument : `sqrt(Σ_pole (IPD(exp,pole,t-l) − IPD(imp,pole,t-l))²)`. SE non corrigées du régresseur généré (note dans `tab_iv_main.tex`).

### 3.8 `05c_gravity_iv_exploration.R`
- **Rôle** : teste 9 instruments alternatifs (mêmes 2SRI) pour vérifier la stabilité du coefficient IPD selon la source de variation.
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet`.
- **Outputs** : `Output/Tables/Estimation_IV/tab_iv_all_synthesis.csv`, `Output/Tables/Estimation_IV/iv_synthesis_report.md`, `Output/Figures/Estimation_IV/iv_fig_all_strategies.png`.
- **Instruments** : IV-1 spatial lag (voisins de l'exportateur) ; IV-2 spatial lag symétrique ; IV-3 geo-distance aux pôles × {post_2014, post_2018, post_2022, joint} ; IV-4 leave-one-out mean IPD ; IV-5 distance de PIB/hab ; IV-6 alignement lag 2 (= `instrument_l2` de `05`).

### 3.9 `06_build_geopol_measures.R`
- **Rôle** : construit `iv_panel.parquet` (mesures institutional + strategic + sanctions).
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet` ; `Data/Raw/IV/V-Dem/V-Dem-CY-Full+Others-v16.csv` ; `Data/Raw/IV/the-database-of-political-institutions/DPI 2023 (CSV Version)/DPI 2023 CSV Version.csv` ; `Data/Raw/IV/Polity5.xls` ; `Data/Raw/IV/ATOP/atop5_1ddyr.csv` ; `Data/Raw/IV/dyadic_mid/dyadic_mid_4.03.csv` ; `Data/Raw/IV/gsdb_v4/GSDB_V4_dyadic.dta`.
- **Outputs** : `Data/Clean/iv_panel.parquet`.
- **Décisions clés** : harmonisation COW→ISO3 (`cow_to_iso3`, 12 custom matches) ; recode `execrlc` Right=1/Center=2/Left=3 ; fenêtres NA imposées (`year>2018`→ATOP/shared_ally NA ; `year>2014`→MID/shared_rival NA ; `year>2023`→sanctions NA) ; `n_common_sanctioners` par **signature de coalition** (set trié des senders → `coalition_id`). `instrument_l2` **non** construit ici.

### 3.10 `07_estimation_iv_alternative.R`
- **Rôle** : diagnostics de premier stage (familles institutional / strategic / combined / Polity) — s'arrête avant 2nd stage/bootstrap.
- **Inputs** : `Data/Clean/master_panel_with_strategic.parquet` + `Data/Clean/iv_panel.parquet` (merge).
- **Outputs** : `Output/Tables/Estimation_IV_alternative/tab_iv_alt_first_stage_{diagnostics.csv, diagnostics.tex, coefs.csv}`.
- **Décision clé / artefact** : l'IV auxiliaire de diagnostic utilise **LHS = `log(trade_value + 1)`** (`feols(... | ... | ipd ~ insts)`), pas `trade_value` (commentaire l.92-94 « PPML non dispo en IV-fixest »).

### 3.11 `07b_first_stage_diagnostics.R`
- **Rôle** : diagnostics 1er stage propres (instrument seul = F effectif t² ; familles jointes ; comparaison FE `pair_full`/`no_pair`/`pair_region` ; inspection `shared_ally_atop` ; intervalle Anderson-Rubin). `set.seed(123)`.
- **Inputs** : `Data/Clean/iv_panel.parquet`.
- **Outputs** : `Output/Tables/Estimation_IV_alternative/diagnostics/first_stage_{per_instrument.csv, joint_family.csv}`. (Réconciliation IV, AR, inspection : console seulement.)
- **Note** : réconciliation IV-feols sur **LHS = `trade_value`** (level) ; AR sur les deux LHS (`trade_value` et `log(trade+1)`).

### 3.12 `07c_estimation_iv_clean.R`
- **Rôle** : CF-PPML 2SRI propre, estimations ponctuelles (Partie A, aucun bootstrap).
- **Inputs** : `Data/Clean/iv_panel.parquet`.
- **Outputs** : `Output/Tables/Estimation_IV_alternative/tab_07c_{point_estimates.csv, first_stage_signs.csv}`.
- **Spécification (verbatim)** : 1er stage `feols(ipd ~ <insts> + <ctrls> | exp_year+imp_year+pair, vcov=~pair)` → `v_hat` ; 2nd stage `fepois(trade_value ~ ipd + v_hat + <ctrls> | exp_year+imp_year+pair, vcov=~pair)`. `instrument_l2` reconstruit inline (lag 2). Specs : S7 `polyarchy_dist` ; S7b `ideol_dist` ; S7c `allied_atop` ; S7d `shared_rival_mid` ; S7e `polity_dist` ; S8ter `instrument_l2` ; S8a `polyarchy_dist+ideol_dist` ; S8b `allied_atop+shared_rival_mid` (ctrl `rta+mid_direct`) ; S8c 4 instruments ; S8bis `polity_dist+ideol_dist`. Échantillon commun (114 346 obs) pour les 6 specs juste-identifiées. Hansen J calculé manuellement (cluster-robust), over-id seulement (k≥2).

### 3.13 `08a_sample_attrition.R`
- **Rôle** : décompose l'attrition de chaque mesure (troncature temporelle vs couverture pays quadratique).
- **Inputs** : `Data/Clean/iv_panel.parquet`.
- **Outputs** : `Output/Tables/Robustness/_archive/tab_sample_attrition.csv`.

### 3.14 `08b_identifiability_checks.R`
- **Rôle** : (A) part de valeur retenue vs dyades ; (B) ratio within après FE 3-way (`demean`) ; (C) sélection de `ideol_dist` sur le niveau de démocratie.
- **Inputs** : `Data/Clean/iv_panel.parquet`.
- **Outputs** : `Output/Tables/Robustness/_archive/tab_identifiability.csv`, `tab_ideol_selection.csv`.

### 3.15 `08c_robustness_measure.R`
- **Rôle** : substitution de l'IPD par chaque mesure (Spec 4) ; paliers communs A/B/C ; synthèse temporelle.
- **Inputs** : `Data/Clean/iv_panel.parquet`.
- **Outputs** : `Output/Tables/Robustness/tab_own_sample.csv`, `tab_palier_{A,B,C}.csv`, `tab_robustness_synthesis.csv`, `report_robustness.md`. (Versions antérieures dans `_archive/`.)
- **Spécification (verbatim)** : `fepois(trade_value ~ X + rta | exp_year + imp_year + pair, vcov=~pair)`, `mid_direct` ajouté comme contrôle quand `X = shared_rival_mid` (sample propre).

### 3.16 `8d_common_sample_diagnosis.R`
- **Rôle** : diagnostic du retournement de signe de l'IPD sur l'échantillon commun (couverture, équilibre des covariables SMD, ancrage full vs commun).
- **Inputs** : `Data/Clean/iv_panel.parquet` + `Data/Clean/master_panel_with_strategic.parquet` (merge GDP/pop).
- **Outputs** : `Output/Tables/Robustness/_archive/tab_8d_{coverage, covariate_balance, anchor}.csv`.

### 3.17 `09_estimations_robustesse_completes.R`
- **Rôle** : rejoue l'intégralité de l'échelle de specs de `04` (Spec 1–10 + Rob1–4, registre paramétré) sur les 5 mesures retenues, paliers A/B/C/D + grille temporelle.
- **Inputs** : `Data/Clean/iv_panel.parquet` + `Data/Clean/master_panel_with_strategic.parquet` (merge `strategic_trade_value, pair_nato, exp_gdp_real, imp_gdp_real, exp_pop, imp_pop`).
- **Outputs** : `Output/Tables/Robustness/tab_grille_mesures.csv`, `tab_grille_temporelle.csv`, `report_estimations.md`. Flags : `SMOKE`, `NO_INTERACT`, `REPORT_ONLY`. Log : `Output/Tables/Robustness/_archive/_run_09.log`.
- **Mesures retenues** : `polyarchy_dist, polity_dist, shared_rival_mid, sanction_nontrade, n_common_sanctioners` (exclues : `ideol_dist`, `allied_atop`).

---

## 4. Inventaire des sorties

### 4.1 Tables d'estimation (`Output/Tables/Estimation/`, source : `04`)
- `tab_main_progression.csv` (.tex) : Specs 1–5, coefficients de tous les régresseurs (voir §5.1).
- `tab_diagnostics.csv` (.tex) : N, coef/SE/p de l'IPD, pseudo-R² pour les 10 specs.
- `tab_strategic_hetero.csv` (.tex) : Specs Total/Strategic/NonStrategic.
- `tab_nato_hetero.csv` (.tex) : Specs 8/9 (interactions `pair_nato`).
- `tab_timevarying.csv` (.tex) : Spec 10 (5 périodes, coef + IC).
- `tab_robustness.csv` (.tex) : Baseline, IPD², No_micro, Post_2002, Excl_US_CN_RU.
- `tab_se_comparison.csv` : SE pair vs multiway (ratio IPD 1.2587, RTA 1.0580).

### 4.2 Tables IV première stratégie (`Output/Tables/Estimation_IV/`, source : `05`, `05c`)
- `tab_iv_main.csv` (.tex) ; `tab_iv_first_stage_lags.csv` (.tex) ; `tab_iv_coef_by_lag.csv` ; `tab_iv_strategic.csv` (.tex) ; `tab_iv_nato.csv` (.tex) ; `tab_iv_timevarying.csv` (.tex) ; `tab_iv_instrument_robustness.csv` (.tex) ; `tab_iv_lag_sensitivity.tex` ; `tab_iv_all_synthesis.csv` ; `iv_synthesis_report.md`.

### 4.3 Tables IV alternative (`Output/Tables/Estimation_IV_alternative/`, source : `07`, `07b`, `07c`)
- `tab_07c_point_estimates.csv` (20 lignes : baselines, S7–S8bis, common) ; `tab_07c_first_stage_signs.csv` ; `tab_iv_alt_first_stage_diagnostics.csv` (.tex) ; `tab_iv_alt_first_stage_coefs.csv` ; `diagnostics/first_stage_per_instrument.csv` ; `diagnostics/first_stage_joint_family.csv`.

### 4.4 Tables robustesse (`Output/Tables/Robustness/`, source : `08a`–`09`)
- Racine : `tab_own_sample.csv`, `tab_palier_{A,B,C}.csv`, `tab_robustness_synthesis.csv`, `report_robustness.md` (08c) ; `tab_grille_mesures.csv` (571 lignes, dont 263 lignes d'interaction), `tab_grille_temporelle.csv` (77 lignes), `report_estimations.md` (09).
- `_archive/` : `tab_sample_attrition.csv` (08a) ; `tab_identifiability.csv`, `tab_ideol_selection.csv` (08b) ; `tab_8d_{coverage,covariate_balance,anchor}.csv` (8d) ; versions antérieures 08c (`tab_robustness_{own_sample, common_sample, full_windows, time_isolation, tiers_anchor, tiers_zscored, ideol_dist}.csv`) ; `_run_09.log`.

### 4.5 Tables descriptives (`Output/Tables/{Trade,Geopolitics,Interaction}/`)
- Trade : `tab01_summary_stats`, `tab02_evolution_decades`, `tab03_by_nato` (.csv/.tex).
- Geopolitics : `geop_tab01_ipd_summary`, `geop_tab02_ipd_by_period_nato`, `geop_tab03_ipd_movers`.
- Interaction : `inter_tab01_transition_matrix`, `inter_tab02_nato_strategic_summary`, `inter_tab03_before_after_2022`, `inter_tab04_partial_correlations`.

### 4.6 Figures (38 PNG dans `Output/Figures/`, 4 dans `Output/Maps/`)
- **Trade (9)** : `fig01` séries temporelles commerce mondial (nominal/réel) ; `fig02` ratio commerce/PIB ; `fig03` part stratégique ; `fig04` aire empilée stratégique/non ; `fig05` top-15 corridors 2020-24 ; `fig06` carte ouverture ; `fig07` commerce par `pair_nato` ; `fig08` part stratégique par NATO ; `fig09` paires actives.
- **Geopolitics (7 fig + 3 cartes)** : `geop_fig01` IPD moyen (pondéré/non) ; `geop_fig02` IPD 8 paires clés ; `geop_fig03` heatmap IPD (2000/2024) ; `geop_fig04` densité IPD (2000/2014/2024) ; `geop_fig05` carte IPD-USA 2024 ; `geop_fig06` carte variation IPD-USA 2010→2024 ; `geop_fig07` carte NATO ; `geop_fig08` nombre membres NATO/an ; `geop_fig09` IPD par `pair_nato` ; `geop_fig10` within-SD vs between-mean IPD.
- **Interaction (11)** : `inter_fig01` binscatter brut IPD-commerce ; `inter_fig02` binscatter résidualisé ; `inter_fig03` binscatter stratégique ; `inter_fig04/05` event-study 2022 (total/strat.) ; `inter_fig06/06b` event-study 2018 ; `inter_fig06c/06d` event-study 2014 (Crimée) ; `inter_fig07` croissance par quartile IPD ; `inter_fig08` part stratégique par quartile IPD × période ; `inter_fig09` commerce NATO×stratégique 2024 ; `inter_fig10` RTA par NATO ; `inter_fig11` matrice de corrélation.
- **Estimation (1)** : `est_fig01_ipd_timevarying.png` (Spec 10).
- **Estimation_IV (4)** : `iv_fig00_fstat_by_lag`, `iv_fig01_coef_by_lag`, `iv_fig02_timevarying_comparison`, `iv_fig_all_strategies`.

### 4.7 Rapports markdown (`Output/Reports/`) — contenu tel qu'écrit
- `VARIABLES.md` : documentation des 12 mesures de `06` (mesure, source, formule, couverture, harmonisation). Indique en cadrage que « la stratégie IV ne converge pas » et que le résultat principal reste le PPML 3-way FE Spec 4.
- `2026-06-16_recap_analyse.md` : récapitulatif (question, Spec 4 = IPD −0.0663, SE 0.0318, p 0.037, N 965 872 ; exploration IV abandonnée ; pivot robustesse). Tableaux de coefficients IV repris du `tab_07c_point_estimates.csv`. Section 6 mise à jour 2026-06-22 (pivot robustesse « fait »).
- `2026-06-22_verifications_variables_robustesse.md` : restitution des contrôles `08a`–`8d` + section 5bis sur `09`. Chiffres clés : within-ratio IPD 0.083 ; attrition ; SMD 8d < 0.17 ; bascule temporelle.
- `Output/Tables/Estimation_IV/iv_synthesis_report.md` : 9 IV, ref PPML +0.0283 (p 0.110) ; 3 sig./9 (tous positifs) ; 3 nég./9.
- `Output/Tables/Robustness/report_robustness.md` (08c) et `report_estimations.md` (09) : voir §5.4.

---

## 5. Résultats (chiffrés)

### 5.1 Estimation principale — `tab_main_progression.csv` / `tab_diagnostics.csv` (`04`)

Coefficient IPD (et RTA), par spec :

| Spec | Estimateur / FE | IPD coef | SE | p | N |
|---|---|---|---|---|---|
| 1_OLS | OLS, sans FE, trade>0 | −0.030158 | 0.014309 | 0.03507 | 696 295 |
| 2_PPML_sep | PPML, `exp_iso3+imp_iso3+year` | +0.035222 | 0.029716 | 0.23590 | 989 296 |
| 3_PPML_ctry_yr | PPML, `exp_year+imp_year` | +0.051254 | 0.032372 | 0.11336 | 1 019 488 |
| 4_PPML_3way | PPML, `exp_year+imp_year+pair` | **−0.066298** | 0.031823 | 0.03722 | 965 872 |
| 5_PPML_3way_MW | Spec 4, multiway cluster | −0.066298 | 0.040054 | 0.09788 | 965 872 |
| 6_Strategic | LHS `strategic_trade_value` | −0.012574 | 0.044033 | 0.77521 | 733 288 |
| 7_NonStrategic | LHS `non_strategic_trade` | −0.072828 | 0.032263 | 0.02399 | 965 118 |

RTA (Spec 4) : +0.114143 (SE 0.023492, p 1.18e-06). Pseudo-R² Spec 4 = 0.99024.
Spec 1 RTA +0.435817 ; `log_dist` −1.201809 ; `log_gdp_exp` +1.201457 ;
`log_gdp_imp` +0.932032 (`tab_main_progression.csv`).

### 5.2 Hétérogénéités et robustesses (`04`)

**NATO** (`tab_nato_hetero.csv`, Spec 8) : `non:ipd` −0.042252 (p 0.304) ;
`inter:ipd` −0.056712 (p 0.133) ; `intra:ipd` −0.152874 (p 0.000404). Spec 9
(strategic) : non −0.004228, inter −0.015513, intra −0.020846 (tous NS).

**Time-varying** (`tab_timevarying.csv`, Spec 10) : 1995-2007 +0.001652 ;
2008-2013 −0.040542 ; 2014-2017 −0.094278 ; 2018-2021 −0.144027 ; 2022-2024
−0.212354.

**Robustesse** (`tab_robustness.csv`) : IPD² → ipd −0.137862 (p 0.000614),
ipd_sq +0.028841 (p 0.00169) ; No_micro ipd −0.067743 (p 0.0350) ; Post_2002 ipd
−0.122889 (p 0.000944) ; Excl_US_CN_RU ipd +0.028271 (p 0.1099).

### 5.3 IV (`05`, `05c`, `07`, `07b`, `07c`)

**`05` (`tab_iv_main.csv`)** : PPML même échantillon ipd +0.009217 (p 0.620) ;
CF-IV lag 2 ipd +0.124419 (p 0.00145), v_hat −0.143414 (p 1.63e-05). First-stage
F (`tab_iv_first_stage_lags.csv`) : lag0 417 934 ; lag1 61 737 ; lag2 28 406 ;
lag3 13 867 ; lag5 4 514. Coef CF-IV croissant avec le lag
(`tab_iv_coef_by_lag.csv`) : lag0 +0.0225 → lag5 +0.4557. Instrument robustness
(`tab_iv_instrument_robustness.csv`) : Euclidean_L2 +0.1244 ; Three_IVs +0.1545 ;
Pre_sample_1995 ipd −1.2447 (v_hat_pre +1.2644).

**`05c` (`tab_iv_all_synthesis.csv` / `iv_synthesis_report.md`)** : ref PPML
+0.0283 (p 0.110, N 932 992). IV-1 +0.1011 (p 0.0062) ; IV-2 +0.0432 (p 0.199) ;
IV-3 post_2014 −0.4188 (p 0.710) ; post_2018 +0.0272 (p 0.962) ; post_2022
−0.0615 (p 0.874) ; joint +0.0217 (p 0.957) ; IV-4 +0.0497 (p 0.0047) ; IV-5
−0.3012 (p 0.626) ; IV-6 +0.1244 (p 0.0015). Synthèse rapportée : 3 sig./9, 3
nég./9, 6 pos./9.

**`07` (`tab_iv_alt_first_stage_diagnostics.csv`, LHS = log(trade+1))** :
institutional KP rk F 60.35, Effective F 4.64, Hansen J p 0.00183 ; strategic KP
274.59, EffF 359.62, Hansen p 1.77e-20 ; combined EffF 6.55, Hansen p 1.13e-14 ;
Polity KP 73.20, EffF 3.61, Hansen p 0.766.

**`07b` (`first_stage_per_instrument.csv`, `first_stage_joint_family.csv`)** —
F effectif par instrument seul, `pair_full` : polyarchy_dist 721.23 ; ideol_dist
114.99 ; allied_atop 212.03 ; shared_ally_atop 557.56 ; shared_rival_mid 167.86.
`no_pair` : valeurs ×10–×40 (polyarchy 6592, etc.). Familles jointes `pair_full` :
institutional 60.35, strategic 273.85 ; `no_pair` : 1333.98 / 2726.49 ;
`pair_region` : 884.41 / 696.14.

**`07c` (`tab_07c_point_estimates.csv`)** — CF-PPML 2SRI, coef IPD (p), p(v_hat),
Hansen J p :

| Spec | insts | N | IPD coef | p | p(v_hat) | Hansen J p |
|---|---|---|---|---|---|---|
| S0 baseline full | — | 965 872 | −0.066298 | 0.0372 | — | — |
| S0 baseline commun | — | 114 346 | +0.011407 | 0.7483 | — | — |
| S7 | polyarchy_dist | 805 994 | −0.308727 | 0.3054 | 0.4074 | — |
| S7b | ideol_dist | 241 402 | −0.146889 | 0.6409 | 0.7772 | — |
| S7c | allied_atop | 752 739 | +0.324940 | 0.3503 | 0.4151 | — |
| S7d | shared_rival_mid | 607 569 | −0.554750 | 0.00489 | 0.00189 | — |
| S7e | polity_dist | 534 884 | +1.450836 | 0.01136 | 0.01380 | — |
| S8ter | instrument_l2 | 864 035 | +0.124419 | 0.00145 | 1.63e-05 | — |
| S8a | polyarchy+ideol | 217 675 | −0.258132 | 0.4580 | 0.5616 | 0.5518 |
| S8b | allied+shared_rival | 607 569 | −0.069077 | 0.7148 | 0.5480 | 0.00547 |
| S8c | 4 instruments | 152 280 | +0.025149 | 0.9247 | 0.9759 | 4.30e-11 |
| S8bis | polity+ideol | 155 676 | +0.233889 | 0.2962 | 0.3605 | 0.3652 |

Échantillon commun (114 346) : S7 −2.024798 (p 0.00248) ; S7b −0.108789 ;
S7c +0.957216 ; S7d +0.720251 ; S7e +5.420919 (p 0.01529) ; S8ter +0.078696.
Signes du 1er stage (`tab_07c_first_stage_signs.csv`) : `shared_rival_mid`
négatif (−0.0398, S7d) ; tous les autres positifs.

### 5.4 Robustesse de mesure (`08a`–`09`)

**`08a` (`tab_sample_attrition.csv`)** : % du panel et couverture par mesure —
polyarchy_dist 56.2 % (couv. pays 75.3 %, perte intra-fenêtre 100 %) ; ideol_dist
16.1 % ; polity_dist 36.6 % ; allied_atop 80.0 % (perte temporelle 100 %) ;
shared_rival_mid 66.7 % ; sanction_nontrade / n_common_sanctioners 96.7 %.

**`08b` (`tab_identifiability.csv`)** : within_ratio après FE 3-way —
allied_atop 0.0864 ; polity_dist 0.1489 ; polyarchy_dist 0.1569 ;
sanction_nontrade 0.1621 ; n_common_sanctioners 0.2661 ; ideol_dist 0.6558 ;
shared_rival_mid 0.6875. `tab_ideol_selection.csv` : % `ideol_dist` manquant
décroît de 82.6 % (démocratie jointe basse) à 27.2 % (haute).

**`08c` / `09`** — substitution Spec 4 (`tab_own_sample.csv`, identique à
`09` `tab_grille_mesures.csv`) : ipd (full) −0.066298 ; polyarchy_dist −0.040763
(p 0.509) ; polity_dist +0.005158 (p 0.0108) ; allied_atop +0.022787 (p 0.359) ;
shared_rival_mid +0.022183 (p 0.0075) ; sanction_nontrade −0.080491 (p 4.9e-05) ;
n_common_sanctioners +0.073262 (p 2.4e-05). Paliers communs (`tab_palier_*` /
`tab_robustness_synthesis.csv`) : A (≤2014, N 476 778) ipd +0.042203 (p 0.022) ;
B (≤2018, N 563 942) ipd +0.040955 (p 0.00855) ; C (≤2023, N 810 230) ipd
−0.023090 (p 0.290). Synthèse temporelle full sample : ≤2014 +0.042633 ; ≤2018
+0.042825 ; ≤2023 −0.022544 ; full +0.066298(−) ; 2015-2024 **−0.198341**
(p 3.1e-06).

**`8d`** — ancrage (`tab_8d_anchor.csv`) : full ipd −0.066298 (p 0.0372, N
965 872) ; commun +0.042203 (p 0.022, N 446 171). Couverture
(`tab_8d_coverage.csv`) : commun 476 778 (46.2 % dyades, 51.2 % valeur, 26.2 %
zéros) vs reste 53.8 %. SMD (`tab_8d_covariate_balance.csv`) : |SMD| max
log_dist −0.169 ; tous < 0.17 ; ipd +0.065.

**`09` (`report_estimations.md`)** — bascule IPD par fenêtre × spec à coef
unique, full sample (extraits) : Spec 4 2015-2024 −0.1983 ; Spec 6 −0.1745 ;
Spec 7 −0.2035 ; Rob2 −0.1996 ; Rob3 −0.1983 ; Spec 2 +0.1037 (1995-2014) puis
−0.0216 ; Spec 3 +0.1175 puis −0.0190 ; Spec 1 −0.0486/−0.0358. Spec 10
décomposition périodique : +0.0017 → −0.0405 → −0.0943 → −0.1440 → −0.2124.

### 5.5 Descriptives (extraits chiffrés)

**Trade** (`tab02_evolution_decades.csv`) : commerce (Bn USD) 63 742 (1995-2004) →
152 667 (2005-2014) → 195 591 (2015-2024) ; part stratégique 5.37 % → 5.74 % →
6.49 % ; IPD moyen 0.973 → 0.879 → 0.844. `tab03_by_nato.csv` : trade moyen (k
USD) intra 5 655 045 / inter 517 979 / non 114 583 ; IPD moyen 0.305 / 1.444 /
0.739.

**Geopolitics** (`geop_tab01_ipd_summary.csv`) : IPD all pair-years (N 515 746)
mean 0.896, SD 0.723, median 0.730, max 5.145 ; within-pair SD 0.269.
`geop_tab02` : IPD inter-NATO 1.523→1.391→1.442, intra 0.356→0.305→0.285, non
0.854→0.722→0.640.

**Interaction** (`inter_tab04_partial_correlations.csv`) : corr log(trade)–IPD
raw +0.112 ; conditionnel dist+GDP −0.0141 ; within-pair −0.0655 ;
within-pair-year −0.0004 (N 696 295). `inter_tab03_before_after_2022.csv` :
variation commerce 2019-21→2022-24 par quartile IPD : Q1 +24.9 %, Q2 +26.1 %,
Q3 +17.3 %, Q4 +23.5 %.

---

## 6. Décisions méthodologiques (telles que consignées)

| Décision | Où consignée | Raison documentée |
|---|---|---|
| PPML 3-way FE (Spec 4) = spécification principale | `recap_analyse.md` §2, `VARIABLES.md` | Santos Silva & Tenreyro 2006 cité (recap §8) ; absorption MRT Anderson-van Wincoop |
| Clustering par paire ; multiway en robustesse (Spec 5) | `04`, `recap_analyse.md` §8 | « standard dans la gravity literature » (recap) |
| Densification cartésienne pour les zéros | `01` Section 2bis, `README.md` | « sans zéros, le PPML est biaisé » (recap §8.4) |
| Gravity `last non-NA` par paire | `01` Section 3 | raison non documentée explicitement |
| IPD symétrisé | `01` Section 4, `README.md` | IPD undirected (commenté) |
| IV abandonné comme identification principale | `recap_analyse.md` §5, `VARIABLES.md`, `iv_synthesis_report.md` | instruments crédibles non sig. sur v_hat / instruments sig. non excluables / estimations instables (recap §4) |
| `mid_direct` = contrôle, pas instrument | `06`, `07c`, `VARIABLES.md` | capte l'effet mécanique du conflit direct (VARIABLES.md) |
| `sanction_trade` à éviter comme régresseur | `06` (commentaire), `VARIABLES.md` | « tautologique en gravité » |
| `ideol_dist` reléguée hors paliers principaux (09) | `report_estimations.md`, `verifications…md` §2 | sélection sur le régime (08b-C) |
| `allied_atop` exclue des paliers principaux (09) | `report_estimations.md` | within-ratio 0.086 ≈ IPD (08b) |
| `n_common_sanctioners` par signature de coalition | `06` Section 6bis, `VARIABLES.md` | évite la décomposition multilatérale / fragmentation `case_id` |
| Paliers A/B/C bornés par MID/ATOP/DPI/GSDB | `08c`, `VARIABLES.md` §choix d'échantillon | couvertures temporelles des sources |
| `07` arrêté avant 2nd stage ; `07c` Partie A sans bootstrap | en-têtes `07`, `07c` | « ARRET PROVISOIRE » / « bootstrap différé à validation manuelle » |

---

## 7. État actuel & reproductibilité

### 7.1 Ordre de ré-exécution (du brut au résultat)

```
01_build_master_panel.R        (Raw → master_panel.parquet)         [WDI : accès réseau]
02_build_strategic_panel.R     (master_panel + BACI → _with_strategic)
03a / 03b / 03c                (descriptives ; indépendants entre eux)
04_gravity_estimation.R        (estimation principale)
05_gravity_iv.R / 05c          (IV 1re stratégie)
06_build_geopol_measures.R     (Raw/IV + _with_strategic → iv_panel.parquet)
07 / 07b / 07c                 (IV alternative ; requièrent iv_panel)
08a / 08b / 08c / 8d           (robustesse ; requièrent iv_panel ; 8d requiert aussi _with_strategic)
09_estimations_robustesse_completes.R  (requiert iv_panel + _with_strategic)
```

Dépendances de fichiers : `01`→`02`→{`03*`,`04`,`05`,`05c`,`06`} ;
`06`→{`07`,`07b`,`07c`,`08a`,`08b`,`08c`,`8d`,`09`}. Les scripts `04`/`05`/`05c`
lisent `master_panel_with_strategic.parquet` (donc après `02`). `8d` et `09`
relisent aussi `master_panel_with_strategic.parquet` (merge GDP/pop/strategic/
pair_nato).

### 7.2 Incohérences de nommage / numérotation (factuelles)

- **`8d_common_sample_diagnosis.R`** : seul script sans zéro initial (les autres
  de la série sont `08a`, `08b`, `08c`). Il n'existe pas de fichier `08d_*`.
- En-têtes de **`07`** et **`07b`** : référencent `06_build_iv_alternative.R`
  comme producteur de `iv_panel.parquet` ; le fichier réellement présent est
  `06_build_geopol_measures.R` (même output `Data/Clean/iv_panel.parquet`).
- **`README.md`** (Data/Clean) décrit le filtre DESTA comme `base_treaty == 1`,
  alors que `01` filtre `entry_type %in% c("base_treaty","accession")` (inclut
  `accession`).

### 7.3 Variables construites mais non utilisées en estimation

- `joint_dem_vdem` (06) : utilisée comme instrument uniquement dans `07` (famille
  institutional, 3 instruments) ; absente de `07c` (S8a = `polyarchy+ideol`) et de
  `07b` (`INST$institutional = polyarchy_dist, ideol_dist`).
- `sanction_any`, `sanction_trade`, `sanction_nontrade`, `n_common_sanctioners`
  (06) : absentes des specs IV `07`/`07b`/`07c` ; `sanction_nontrade` et
  `n_common_sanctioners` apparaissent en robustesse (`08c`, `09`).
- `shared_ally_atop`, `comlang_eth`, `comcol`, `comrelig`, `distw_harm`,
  `dist_cap` : présentes dans les panels, non utilisées dans les specs
  principales de `04` (gravité réduite à `log_dist, contig, comlang_off, colony`).
- `instrument_l2` : **non** présent dans `iv_panel.parquet` ; reconstruit inline
  dans `05` et `07c`.

### 7.4 Hétérogénéité de LHS dans la chaîne IV (factuelle)

- `log(trade_value + 1)` : diagnostics IV de `07` ; Anderson-Rubin de `07b`.
- `trade_value` (niveau) : réconciliation IV-feols de `07b` ; 2nd stage PPML de
  `07c`. Le `recap_analyse.md` §4 qualifie la version LHS-log de `07`
  d'« artefact » (Effective F 4.6, Hansen p 0.002 « trompeurs »), réconcilié par
  `07b`/`07c`.

### 7.5 Sorties / fichiers non référencés ou archivés

- `Output/Tables/Robustness/_archive/` : versions antérieures de `08c` (7 CSV),
  diagnostics `08a`/`08b`/`8d`, et `_run_09.log`.
- `07_estimation_iv_alternative.R` : marqué « ARRET PROVISOIRE » (Section 6) ;
  ses diagnostics LHS-log sont remplacés par `07b`/`07c`.
- `Info/` : 2 PDF de référence (`memo_specification_gravity_PPML.pdf`,
  `memo_IRF_local_projections_gravity.pdf`), non lus par les scripts.
- Caches : `Data/Clean/_baci_agg_cache.parquet`, `_baci_strategic_cache.parquet`
  (réutilisés par `01`/`02` s'ils existent).

### 7.6 Ce qui casserait une ré-exécution *from scratch*

- **`01`** dépend de l'API WDI (`WDI(country="all", ...)`) → accès réseau requis ;
  sinon échec à la Section 5.
- Suppression des caches `_baci_*` → relecture intégrale des 30 fichiers BACI
  annuels (≈ 9 Go bruts).
- `06` requiert toutes les sources `Data/Raw/IV/*` (V-Dem 406 Mo, GSDB .dta 101
  Mo, etc.) ; chemins hardcodés absolus `PATH_ROOT <- "/Users/zoe/Desktop/Master_thesis"`
  dans tous les scripts.
- `09` : single-thread `fixest` sur cette machine (OpenMP absent, cf.
  `_run_09.log`) ; exécution complète longue (log : 18:57→19:43, ≈ 47 min).
- `instrument_l2` n'étant pas persisté, `05`/`07c` le reconstruisent à chaque run.

---

## Résumé factuel

Question : effet de la distance géopolitique (IPD, votes ONU) sur le commerce
bilatéral (BACI), via gravité PPML 3-way FE. Pipeline : `01` construit
`master_panel` (1 593 900 obs dir., 231×231 pays, 1995–2024, 45.4 % de zéros) ;
`02` ajoute le commerce stratégique (186 codes HS6) ; `06` construit `iv_panel`
(12 mesures dyadiques alternatives). Estimation principale (`04`, Spec 4) : IPD
**−0.0663** (SE 0.0318, p 0.037, N 965 872) ; RTA +0.114 ; Spec 10 montre un
coefficient passant de +0.002 (1995-2007) à −0.212 (2022-2024). Exploration IV
(`05`/`05c`/`07`/`07c`) : CF-IV lag 2 IPD +0.124 (p 0.0015) ; sur 9 instruments
alternatifs 3 sig. (positifs) ; CF-PPML par mesure (`07c`) instable (S7 −0.309
NS, S7d −0.555, S7e +1.451, sur échantillon commun S7 −2.025) ; Hansen J rejette
pour S8b/S8c, non rejeté pour S8a/S8bis ; IV documenté comme abandonné comme
identification principale (`recap_analyse.md`, `iv_synthesis_report.md`).
Robustesse (`08a`–`09`) : substitution de l'IPD par 5 mesures + grille
temporelle ; IPD positif sur ≤2014/≤2018 (paliers A/B : +0.042/+0.041) et négatif
sur 2015-2024 (−0.198, p 3.1e-06) à travers la famille FE three-way. État :
17 scripts présents et exécutés ; sorties dans `Output/Tables`, `Output/Figures`,
`Output/Maps`, `Output/Reports` ; incohérences de nommage notées (`8d` vs `08*`,
en-têtes `07`/`07b` citant `06_build_iv_alternative.R`, README DESTA
`base_treaty` vs code) ; `01` requiert l'API WDI ; chemins absolus hardcodés.
