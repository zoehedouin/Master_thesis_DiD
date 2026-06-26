# README — Carte du pipeline (réorganisation `reorg-did`)

> Remplace `2026-06-23_audit_pipeline.md` (archivé dans `Reports/_archive/`).
> Le contenu attendu de chaque script est défini par la feuille de route
> `recap_methodo_DiD.docx` (racine du dépôt). Voir aussi `MIGRATION_LOG.md`
> (racine) pour la trace des déplacements et `Codes/_archive/README.md` pour
> le détail de l'archive.

## Principe d'organisation

Un fichier = une partie. Tous les scripts commencent par `source("00_setup.R")`
(chemins, wrappers I/O robustes au chemin accentué NFD, helpers de log). Aucun
chemin n'est codé en dur ailleurs que dans les replis de `00_setup.R`.

**Lancer un script** : depuis `Codes/`, `Rscript 0X_xxx.R` (ou `source()` dans R
avec le répertoire de travail sur `Codes/`). `00_setup.R` détecte `DATA_ROOT`
(via `Data/Clean/`) et `PROJECT_ROOT` (via `Output/`) automatiquement.

## Statut de chaque script

| # | Script | Statut | Étape feuille de route | Sorties principales |
|---|---|---|---|---|
| 00 | `00_setup.R` | **NEW** | — (socle) | (aucune ; sourcé partout) |
| 01 | `01_build_master_panel.R` | gardé | préalable (données) | `Data/Clean/master_panel.*` |
| 02 | `02_build_strategic_panel.R` | gardé | préalable (données) | `Data/Clean/master_panel_with_strategic.*` |
| 03 | `03_build_treatments.R` | **reframe** (ex-`06`) | préalable (traitement) | `Data/Clean/iv_panel.parquet` |
| 04 | `04_build_un_votes.R` | **NEW (squelette)** | préliminaire (votes ONU) | `Data/Clean/un_votes.*` |
| 05 | `05_build_covariates.R` | **NEW (squelette)** | préalable (covariables + 2×2) | `Data/Clean/covariates.*` |
| 06 | `06_descriptives.R` | **fusion** (`03a+03b+03c`) | §1 descriptives | `Output/{Figures,Tables,Maps}/{Trade,Geopolitics,Interaction}` |
| 07 | `07_validity.R` | **NEW (squelette)** | §2 validité (balance, pré-tendances, HonestDiD) | `Output/{Tables,Figures}/Validity` |
| 08 | `08_ppml.R` | **fusion** (ex-`04`+ex-`10`) | §3 PPML (statique, type, 2×2, Sun-Abraham) | `Output/{Tables,Figures}/{Estimation,EventStudy}` |
| 09 | `09_dcdh.R` | **fusion** (`11`+`11b`+`11c`+`11d`) + skel. `dist_lag_het` | §4 intensité dCDH | `Output/{Tables,Figures}/EventStudy` |
| 10 | `10_decomposition.R` | **NEW (squelette)** | §5 stratégique / non-stratégique | `Output/{Tables,Figures}/Decomposition` |
| 11 | `11_robustness.R` | **NEW (squelette)** | §6 placebos, fenêtres, triangulation, sender | `Output/{Tables,Figures}/Robustness` |

**Squelette** = en-tête + `source("00_setup.R")` + lecture des entrées + bloc
`## TODO (feuille de route §X)`. Aucun résultat n'y est fabriqué.

## Ordre d'exécution

```
00 (sourcé)  ->  01  ->  02  ->  03  ->  04  ->  05        (données + traitement)
             ->  06 (descriptives)                          (§1)
             ->  07 (validité)                              (§2)
             ->  08 (PPML)                                  (§3)
             ->  09 (dCDH intensité)                        (§4)
             ->  10 (décomposition)                         (§5, après résultat propre sur le total)
             ->  11 (robustesse)                            (§6)
```

Dépendances de données : `04` (votes) et `05` (covariables/2×2) alimentent le
2×2 de `08` et la balance de `07`. `10` et `11` réutilisent les meilleures specs
de `08`/`09`.

## Matrice traitement × outcome × estimateur (feuille de route)

| Traitement | Outcome | Estimateur | Script |
|---|---|---|---|
| Sanction (dummy) | Total | PPML statique + Sun-Abraham | `08` |
| Type : commercial / non-commercial | Total | PPML statique (réplique GSDB-R4 col. 2) | `08` |
| Intensité (paliers) | Total (logs) | AVSQ `did_multiplegt_dyn` + `dist_lag_het` | `09` |
| Condamnation ONU (dummy) | Total | PPML statique + DiD 2×2 autour de 2022 | `08` |
| 2×2 condamne × sanctionne | Total | PPML statique (interactions) | `08` |
| Sanction non-commerciale | Strat. / non-strat. | Event study PPML + AVSQ | `10` |
| Intensité (paliers) | Strat. / non-strat. | AVSQ + `dist_lag_het` | `10` |

## Archivé (rien supprimé)

- **`Codes/_archive/`** : `iv/` (IV abandonnée), `ipd_robustness/` (`08a`–`8d`,
  `09`), `legacy_descriptives/` (originaux `03a/b/c`), `fused_sources/`
  (originaux `04`, `10`, `11*`), `backups/` (`.bak`). Détail : `Codes/_archive/README.md`.
- **`Output/_archive/`** : `Figures/Estimation_IV(_alternative)`,
  `Tables/Estimation_IV(_alternative)`, `Tables/Robustness_legacy`,
  `Figures/Interaction_degraisse` (figures d'interaction secondaires non reprises).
- **`Reports/_archive/`** : `2026-06-23_audit_pipeline.md` (remplacé par ce
  fichier), `2026-06-16_recap_analyse.md`, `2026-06-22_verifications_variables_robustesse.md`
  (distillés dans `annexe_ipd_iv.md`).
- **`Data/Clean/_archive/`** (hors dépôt git, volumineux) : caches `_baci_*`,
  `iv_panel_backup_*`.

## Rapports conservés (cœur DiD)

`VARIABLES.md`, `report_eventstudy_phase1.md`, `report_intensity_dcdh_phase3.md`,
`report_sanctions_synthese.md`, plus la nouvelle annexe `annexe_ipd_iv.md`.
