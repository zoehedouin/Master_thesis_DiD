# README — Carte du pipeline (organisation **par partie**, réorg #2)

> Remplace `2026-06-23_audit_pipeline.md` (archivé dans `Reports/_archive/`).
> Le contenu attendu de chaque script est défini par la feuille de route
> `recap_methodo_DiD.docx` (racine du dépôt). Voir aussi `MIGRATION_LOG.md`
> (racine) pour la trace des déplacements et `_archive/README.md` pour le détail
> de l'archive de code.

## Principe d'organisation

**Co-localisation des sorties de section, centralisation des données + de la
synthèse.** Le dépôt est organisé **par partie** : un dossier `NN_nom/` par
partie (`01`→`11`), contenant **son script** et **ses** sorties propres
(`figures/`, `tables/`, `maps/`, `NN_report.md`). Ce qui est *partagé* reste
central :
- les **panels** produits par `01`–`05` → `Data/Clean/` (central, hors dépôt) ;
- les **rapports de synthèse / transversaux** → `Reports/` (central) ;
- chaque partie analytique `06`–`11` a **en plus** son rapport `NN_report.md`.

**Mécanique des chemins.** `00_setup.R` (à la racine analytique) détecte
`DATA_ROOT` (via `Data/Clean/`) et `ANALYSIS_ROOT` (via `.git`). Chaque script
`06`–`11` déclare `PART <- "NN_nom"` ; les helpers `out_fig()`/`out_tab()`/
`out_map()` écrivent alors dans `ANALYSIS_ROOT/PART/{figures,tables,maps}`.
Aucun chemin de sortie n'est codé en dur.

**Lancer un script** : depuis la racine analytique, `Rscript NN_nom/NN_xxx.R`
(ou en ayant `cd` dans le dossier de partie). Un *bootstrap* en tête de chaque
script remonte jusqu'au dossier contenant `00_setup.R` et le source — robuste
sous `Rscript` comme en `source()` interactif.

## Statut de chaque script

| # | Dossier / script | Statut | Étape feuille de route | Sorties principales |
|---|---|---|---|---|
| 00 | `00_setup.R` (racine) | **NEW** | — (socle) | (aucune ; sourcé partout) |
| 01 | `01_master_panel/01_build_master_panel.R` | gardé | préalable (données) | `Data/Clean/master_panel.*` |
| 02 | `02_strategic_panel/02_build_strategic_panel.R` | gardé | préalable (données) | `Data/Clean/master_panel_with_strategic.*` |
| 03 | `03_treatments/03_build_treatments.R` | **reframe** (ex-`06`) | préalable (traitement) | `Data/Clean/iv_panel.parquet` |
| 04 | `04_un_votes/04_build_un_votes.R` | **NEW (squelette)** | préliminaire (votes ONU) | `Data/Clean/un_votes.*` |
| 05 | `05_covariates/05_build_covariates.R` | **NEW (squelette)** | préalable (covariables + 2×2) | `Data/Clean/covariates.*` |
| 06 | `06_descriptives/06_descriptives.R` | **fusion** (`03a+03b+03c`) | §1 descriptives | `06_descriptives/{figures,tables,maps}/` |
| 07 | `07_validity/07_validity.R` | **NEW (squelette)** | §2 validité (balance, pré-tendances, HonestDiD) | `07_validity/{figures,tables}/` |
| 08 | `08_ppml/08_ppml.R` | **fusion** (ex-`04`+ex-`10`) | §3 PPML (statique, type, 2×2, Sun-Abraham) | `08_ppml/{figures,tables}/` |
| 09 | `09_dcdh/09_dcdh.R` | **fusion** (`11`+`11b`+`11c`+`11d`) + skel. `dist_lag_het` | §4 intensité dCDH | `09_dcdh/{figures,tables}/` |
| 10 | `10_decomposition/10_decomposition.R` | **NEW (squelette)** | §5 stratégique / non-stratégique | `10_decomposition/{figures,tables}/` |
| 11 | `11_robustness/11_robustness.R` | **NEW (squelette)** | §6 placebos, fenêtres, triangulation, sender | `11_robustness/{figures,tables}/` |

Chaque partie `06`–`11` contient aussi `NN_report.md` (rapport de partie).
**Squelette** = en-tête + bootstrap `source` + `PART` + lecture des entrées +
bloc `## TODO (feuille de route §X)`. Aucun résultat n'y est fabriqué.

> **`EventStudy` scindé par producteur** : les sorties Sun-Abraham / statique /
> validation du traitement (`*sunab*`, `*static_did*`, `*treatment_validation*`)
> vont en `08_ppml/` ; les sorties d'intensité dCDH (`*dcdh*`, `*tier*`,
> `*russia_cases*`) vont en `09_dcdh/`. Le routage est automatique via `PART`.

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

## Archivé (rien supprimé) — tout sous `_archive/` à la racine

- **`_archive/`** (code) : `iv/` (IV abandonnée), `ipd_robustness/` (`08a`–`8d`,
  `09`), `legacy_descriptives/` (originaux `03a/b/c`), `fused_sources/`
  (originaux `04`, `10`, `11*`), `backups/` (`.bak`). Détail : `_archive/README.md`.
- **`_archive/output_legacy/`** (sorties legacy, ex-`Output/_archive` + Robustness
  IPD) : `Figures/Estimation_IV(_alternative)`, `Tables/Estimation_IV(_alternative)`,
  `Tables/Robustness_legacy`, `Tables/Robustness_ipd` (ex-`Output/Tables/Robustness`,
  paliers/grilles IPD : aucun script actif ne les produit),
  `Figures/Interaction_degraisse`.
- **`Reports/_archive/`** : `2026-06-23_audit_pipeline.md` (remplacé par ce
  fichier), `2026-06-16_recap_analyse.md`, `2026-06-22_verifications_variables_robustesse.md`
  (distillés dans `annexe_ipd_iv.md`).
- **`Data/Clean/_archive/`** (hors dépôt git, volumineux) : caches `_baci_*`,
  `iv_panel_backup_*`.

## Rapports

- **Central `Reports/`** (synthèse + transversaux) : `README_pipeline.md`,
  `VARIABLES.md`, `report_sanctions_synthese.md`, `annexe_ipd_iv.md`, `_archive/`.
- **Par partie** : `06_descriptives/06_report.md`, `07_validity/07_report.md`,
  `08_ppml/08_report.md` (ex-`report_eventstudy_phase1.md`),
  `09_dcdh/09_report.md` (ex-`report_intensity_dcdh_phase3.md`),
  `10_decomposition/10_report.md`, `11_robustness/11_report.md`.
