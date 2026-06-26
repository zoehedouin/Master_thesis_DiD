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
