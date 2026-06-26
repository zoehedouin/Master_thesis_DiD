# MIGRATION_LOG — réorganisation `reorg-did`

Trace exhaustive de la réorganisation du dépôt (séparation des deux époques
« IPD/IV legacy » et « sanctions/DiD »). **Aucun fichier n'a été supprimé** :
tout ce qui sort du pipeline actif est *déplacé* en `_archive/`.

- Branche : `reorg-did` (depuis `master`, commit `first push`).
- Racine du dépôt git : `Master_thesis_DiD/` (contient `Codes/` + `Output/`).
- `Data/` (11 Go, BACI brut) est **hors dépôt** (`/Users/zoe/Thesis/Data`),
  détecté par chemin via `00_setup.R`.
- Feuille de route de référence : `../recap_methodo_DiD.docx`.

## Contexte / divergences vs prompt de réorg
- Le dépôt était **déjà** versionné (pas de `git init` nécessaire) ; le snapshot
  pré-réorg est le commit `first push`.
- Le layout réel sépare `Data/` (un niveau au-dessus de `Codes/`) de `Output/`
  (sous `Master_thesis_DiD/`). `00_setup.R` détecte donc **deux** racines :
  `DATA_ROOT` (via `Data/Clean/`) et `PROJECT_ROOT` (via `Output/`).
- Les chemins en dur historiques (`/Users/zoe/Desktop/Master_thesis` legacy,
  OneDrive accentué pour 06/10/11) sont conservés comme **replis** dans
  `00_setup.R` uniquement.

---

## 1. Créations (NEW)

| Fichier | Nature |
|---|---|
| `.gitignore` | `.DS_Store`, tempfiles R |
| `Codes/00_setup.R` | Socle : détection racines, `PATH_*`, wrappers I/O NFD-safe, `out_tab/fig/map/rep`, `log_step/tic/toc`, parallélisme |
| `Codes/04_build_un_votes.R` | Squelette — codage votes ONU ES-11/1 + 68/262 (feuille de route, étape préliminaire) |
| `Codes/05_build_covariates.R` | Squelette — dépendance énergétique, exposition pré-2014, 2×2 |
| `Codes/07_validity.R` | Squelette — balance SMD, pré-tendances, HonestDiD (§2) |
| `Codes/10_decomposition.R` | Squelette — stratégique / non-stratégique (§5) |
| `Codes/11_robustness.R` | Squelette — placebos, fenêtres, triangulation, sender (§6) |
| `Codes/_archive/README.md` | Index de l'archive (quoi/pourquoi par fichier) |
| `Output/Reports/README_pipeline.md` | Nouvelle carte du pipeline (remplace l'audit) |
| `Output/Reports/annexe_ipd_iv.md` | Annexe : IV-impasse + hétérogénéité temporelle IPD |
| `MIGRATION_LOG.md` | Ce fichier |

## 2. Fusions (NEW, concaténation fidèle des sources archivées)

| Nouveau | Sources fusionnées | §FdR |
|---|---|---|
| `Codes/06_descriptives.R` | `03a_desc_trade` + `03b_desc_geopolitics` + `03c_desc_interaction` | §1 |
| `Codes/08_ppml.R` | `04_gravity_estimation` + `10_event_study_sanctions` | §3 |
| `Codes/09_dcdh.R` | `11_intensity_dcdh` + `11b` + `11c` + `11d` (+ squelette `dist_lag_het`) | §4 |

Règles appliquées : logique numérique **inchangée** (bannières
`# ===== [from X.R] =====`), suppression des blocs setup locaux au profit de
`source("00_setup.R")`, réécriture mécanique des chemins de sortie vers
`out_tab/out_fig/out_map`. `09_dcdh.R` expose un paramètre `SAMPLE_CONTROLS`
(TRUE = comportement legacy ; FALSE = plein échantillon sur machine plus grosse).

## 3. Renommages / reframes (logique conservée)

| Avant | Après | Modification |
|---|---|---|
| `01_build_master_panel.R` | `01_build_master_panel.R` | en-tête + `source("00_setup.R")` ; DESTA comment corrigé |
| `02_build_strategic_panel.R` | `02_build_strategic_panel.R` | en-tête + `source("00_setup.R")` |
| `06_build_geopol_measures.R` | `03_build_treatments.R` | **reframe** en constructeur de traitement ; familles IV pures marquées `[LEGACY IV]` ; alias `PATH_RAW=PATH_IV`, `PATH_PANEL=PATH_STRATEGIC` |

## 4. Déplacements vers `Codes/_archive/` (git mv — rien perdu)

- `iv/` : `05_gravity_iv`, `05c_gravity_iv_exploration`, `07_estimation_iv_alternative`,
  `07b_first_stage_diagnostics`, `07c_estimation_iv_clean`
- `ipd_robustness/` : `08a_sample_attrition`, `08b_identifiability_checks`,
  `08c_robustness_measure`, `8d_common_sample_diagnosis`, `09_estimations_robustesse_completes`
- `legacy_descriptives/` : `03a_desc_trade`, `03b_desc_geopolitics`, `03c_desc_interaction`
- `fused_sources/` : `04_gravity_estimation`, `10_event_study_sanctions`,
  `11_intensity_dcdh`, `11b_dcdh_outputs`, `11c_dcdh_by_tier`, `11d_robustness`
- `backups/` : `06_build_geopol_measures.R.bak_20260624`

## 5. Déplacements `Output/` → `Output/_archive/`

- `Figures/Estimation_IV` → `_archive/Figures/Estimation_IV`
- `Figures/Estimation_IV_alternative` (vide) → `_archive/Figures/Estimation_IV_alternative`
- `Tables/Estimation_IV` → `_archive/Tables/Estimation_IV` (inclut `iv_synthesis_report.md`)
- `Tables/Estimation_IV_alternative` → `_archive/Tables/Estimation_IV_alternative`
- `Tables/Robustness/_archive` → `_archive/Tables/Robustness_legacy` (consolidation)
- Dégraissage figures d'interaction → `_archive/Figures/Interaction_degraisse/` :
  `inter_fig03_binscatter_strategic`, `inter_fig07_growth_by_ipd_quartile`,
  `inter_fig08_strategic_share_by_ipd`, `inter_fig09_trade_nato_strategic`,
  `inter_fig10_rta_by_nato`, `inter_fig11_correlation_matrix`

## 6. Déplacements `Output/Reports/` → `Reports/_archive/`

- `2026-06-23_audit_pipeline.md` (remplacé par `README_pipeline.md`)
- `2026-06-16_recap_analyse.md` (distillé dans `annexe_ipd_iv.md`)
- `2026-06-22_verifications_variables_robustesse.md` (distillé dans `annexe_ipd_iv.md`)

Conservés à la racine de `Reports/` (cœur DiD) : `VARIABLES.md`,
`report_eventstudy_phase1.md`, `report_intensity_dcdh_phase3.md`,
`report_sanctions_synthese.md` (+ bannière de correspondance ancien→nouveau script).

## 7. Déplacements hors dépôt — `Data/Clean/_archive/` (`mv`, volumineux)

- `_baci_agg_cache.parquet`, `_baci_strategic_cache.parquet` (caches ; re-générés
  par `01`/`02` dans `Data/Clean/` au prochain run)
- `iv_panel_backup_20260624.parquet`

## 8. Corrections d'incohérences (audit)

- Commentaire DESTA dans `01_build_master_panel.R` et `Data/Clean/README.md`*
  alignés sur le code réel : `entry_type ∈ {base_treaty, accession}`
  (et non « base_treaty == 1 »).
- En-têtes des scripts IV archivés (`07`, `07b`) citant un inexistant
  `06_build_iv_alternative.R` → corrigés en `03_build_treatments.R`
  (ex-`06_build_geopol_measures.R`).
- `8d` : nommage incohérent avec `08*` — désormais archivé sous
  `ipd_robustness/`, noté dans `Codes/_archive/README.md`.

\* `Data/Clean/README.md` est hors du dépôt git (modif sur disque uniquement).

---

# Réorg #2 — passage à une structure « par partie »

Branche `reorg-by-part` (depuis `reorg-did`). Principe : **co-localiser les
sorties de section, centraliser données + synthèse**. La couche `Codes/` et la
couche `Output/` disparaissent ; un dossier `NN_nom/` par partie contient son
script et ses sorties. Tout via `git mv` — rien supprimé.

## A. Mécanique des chemins (`00_setup.R`)
- Détection `PROJECT_ROOT` (via `Output/`) remplacée par `ANALYSIS_ROOT` (via
  `.git`/`00_setup.R`). `DATA_ROOT` (via `Data/Clean/`) inchangé. `PATH_OUT` retiré.
- Helpers `out_tab()/out_fig()/out_map()` redéfinis : routent vers
  `ANALYSIS_ROOT/<PART>/{tables,figures,maps}` (argument-thème ignoré, gardé pour
  compat → **aucun site d'appel modifié**). `out_rep()` → `Reports/` central.
- Chaque script `06`–`11` : ajout de `PART <- "NN_nom"`. Les 12 scripts : le
  `source("00_setup.R")` nu est remplacé par un **bootstrap** qui remonte jusqu'au
  dossier de `00_setup.R` (robuste sous `Rscript` et `source()`).

## B. Déplacements de scripts (Codes/ → dossiers de partie, racine)
`00_setup.R`, `01_build_master_panel.R`→`01_master_panel/`,
`02…`→`02_strategic_panel/`, `03…`→`03_treatments/`, `04…`→`04_un_votes/`,
`05…`→`05_covariates/`, `06…`→`06_descriptives/`, `07…`→`07_validity/`,
`08…`→`08_ppml/`, `09…`→`09_dcdh/`, `10…`→`10_decomposition/`,
`11…`→`11_robustness/`. `Codes/` supprimé (vide). `Codes/_archive/` → `_archive/`.

## C. Déplacements de sorties (Output/ → dossiers de partie)
- `Output/{Figures,Tables}/{Trade,Geopolitics,Interaction}` + `Output/Maps/*`
  → `06_descriptives/{figures,tables,maps}/`.
- `Output/{Figures,Tables}/Estimation` → `08_ppml/{figures,tables}/`.
- **`Output/.../EventStudy` scindé par producteur** :
  `es_fig01_sunab`, `tab_eventstudy_sunab`, `tab_static_did`,
  `tab_treatment_validation(+_meta)` → `08_ppml/` ;
  `es_fig02_dcdh_tiers`, `tab_dcdh_by_tier`, `tab_dcdh_robustness`,
  `tab_russia_cases_by_type` → `09_dcdh/`.

## D. Archives consolidées sous `_archive/` (racine)
- `Output/_archive/{Figures,Tables}` → `_archive/output_legacy/{Figures,Tables}`.
- **Déviation assumée** : `Output/Tables/Robustness` (paliers/grilles IPD,
  `report_estimations.md`, `report_robustness.md`) — **legacy** (aucun script
  actif ne les produit ; ex-`09_estimations_robustesse_completes.R`) → archivé en
  `_archive/output_legacy/Tables/Robustness_ipd/` plutôt que dans le nouveau
  `11_robustness/` (qui aurait recréé la confusion script↔sortie).
- `Output/` supprimé (vide).

## E. Rapports
- `Output/Reports/` → `Reports/` (racine). Restent centraux : `README_pipeline.md`,
  `VARIABLES.md`, `report_sanctions_synthese.md`, `annexe_ipd_iv.md`, `_archive/`.
- Rapports de phase → rapports de partie : `report_eventstudy_phase1.md` →
  `08_ppml/08_report.md` ; `report_intensity_dcdh_phase3.md` → `09_dcdh/09_report.md`
  (bannières/liens internes mis à jour).
- **Stubs** créés : `06_descriptives/06_report.md`, `07_validity/07_report.md`,
  `10_decomposition/10_report.md`, `11_robustness/11_report.md`.

## F. Renvois mis à jour
- En-têtes/TODO des squelettes `07`/`10`/`11` : `Output/{Tables,Figures}/<theme>`
  → `NN_partie/{tables,figures}`. `09_dcdh.R` & `06_descriptives.R` : commentaires
  `Codes/`/`Output/` actualisés.
- Rapports centraux (`README_pipeline.md`, `VARIABLES.md`, `annexe_ipd_iv.md`,
  `report_sanctions_synthese.md`) : chemins `Codes/`/`Output/` → structure par partie.
- `.gitignore` : ajout `*.Rhistory` ; 2 `.Rhistory` traqués par erreur untrackés.

## G. Choix techniques documentés
- Racine git/analytique = `Master_thesis_DiD/` (inchangé). `Data/` reste hors dépôt.
- `01`–`05` sont des **dossiers** de partie (défaut demandé) ne contenant que le
  script ; leurs sorties (panels) restent dans `Data/Clean/` (central).

---

# Réorg #3 — variables actives DiD (modifs scripts 01 / 02 / 03)

Modifications de contenu (pas de déplacement). Logique d'analyse inchangée ;
seuls des ajouts de variables + un flag d'isolation.

- **01** : ajout appartenance UE — table `eu_members` hardcodée (calendrier
  1958→2013 ; RU jusqu'à `GBR_exit_year=2020`), dérive `exp_eu`/`imp_eu`/`pair_eu`
  (miroir NATO). README master mis à jour (auto-généré).
- **02** : (a) `non_strategic_trade` (+ `non_strategic_share`) = complément du
  stratégique ; (b) `exp/imp_energy_dep_rus` = part hydrocarbures russes (BACI
  HS27, exportateur RUS) dans les imports totaux du pays (cache
  `_baci_energy_cache.parquet` ; code RUS lu du crosswalk). README_strategic MAJ.
- **03** : (a) niveau `exp/imp_polyarchy` (V-Dem `v2x_polyarchy`) toujours produit
  (covariable de régime active) ; (b) flag `BUILD_LEGACY_IV` (défaut FALSE) isole
  les familles IV (distances V-Dem/DPI/Polity + ATOP/MID) et saute leurs lectures.
  Repro « 0 diff » des 4 colonnes sanctions intacte (hors flag).
- **Docs** : `Reports/VARIABLES.md` (section variables actives + note flag IV).

---

# Réorg #4 — scission 03 sanctions (actif) / IV (archivé)

- **Renommage** : `03_treatments/03_build_treatments.R` → `03_sanctions/03_build_sanctions.R`.
- **03_build_sanctions.R (ACTIF)** : suppression du flag `BUILD_LEGACY_IV` et de
  TOUT le code IV (lectures DPI/Polity5/ATOP/MID + helpers `cow_to_iso3`/
  `name_to_iso3` + distances + tiers communs). Conserve sanctions (GSDB) + niveaux
  `exp/imp_polyarchy` (V-Dem). Écrit désormais **`sanctions_panel.parquet`**
  (variable interne renommée `iv_panel`→`sanc_panel`).
- **`_archive/iv_legacy/build_iv_panel.R` (NEW, LEGACY)** : lit `sanctions_panel`,
  dérive `polyarchy_dist`/`joint_dem_vdem` des niveaux, reconstruit `ideol_dist`/
  `polity_dist`/`allied_atop`/`shared_ally_atop`/`mid_direct`/`shared_rival_mid`
  depuis DPI/Polity/ATOP/MID (code déplacé, logique inchangée). Écrit
  `Data/Clean/_archive/iv_panel.parquet`. Lu par aucun script actif.
- **00_setup.R** : ajout `PATH_SANCTIONS_PANEL` ; retrait de `PATH_IV_PANEL`
  (plus aucun lecteur actif).
- **Lecteurs aval** : `04`, `05`, `08`, `09` lisent maintenant `PATH_SANCTIONS_PANEL`
  (aucun n'utilisait les distances IV). En-têtes `07/10/11` mis à jour.
- **Données** : `Data/Clean/iv_panel.parquet` périmé déplacé (`mv`) vers
  `Data/Clean/_archive/` ; backup `iv_panel_backup_20260624.parquet` intact.
- Repro « 0 diff » des 4 colonnes sanctions préservée (logique GSDB non touchée).
