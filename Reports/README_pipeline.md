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
| 03 | `03_sanctions/03_build_sanctions.R` | **scindé** (sanctions pur) | préalable (traitement) | `Data/Clean/sanctions_panel.parquet` |
| — | `_archive/iv_legacy/build_iv_panel.R` | **archivé** (hors pipeline) | legacy IV | `Data/Clean/_archive/iv_panel.parquet` |
| 04 | `04_un_votes/04_build_un_votes.R` | **NEW (squelette)** | préliminaire (votes ONU) | `Data/Clean/un_votes.*` |
| 05 | `05_covariates/05_build_covariates.R` | **NEW (squelette)** | préalable (covariables + 2×2) | `Data/Clean/covariates.*` |
| 06 | `06_descriptives/06_descriptives.R` | **fusion** (`03a+03b+03c`) | §1 descriptives (socle général) | `06_descriptives/{figures,tables,maps}/` |
| 06b | `06_descriptives_did/06_descriptives_did.R` | **NEW** | §1 descriptives (bloc DiD Russie) | `06_descriptives_did/{figures,tables,maps}/` (préfixe `did_`) |
| 07 | `07_ppml/07_ppml.R` | **réécrit à neuf** (Russie-centré) | §2-3 PPML : statique, type, 2×2 ONU, Sun-Abraham, pré-tendances cond. énergie, HonestDiD | `07_ppml/{figures,tables}/` |
| 08 | `08_dcdh/08_dcdh.R` | **fusion** (`11`+`11b`+`11c`+`11d`) + skel. `dist_lag_het` | §4 intensité dCDH | `08_dcdh/{figures,tables}/` |
| 09 | `09_decomposition/09_decomposition.R` | **NEW (squelette)** | §5 stratégique / non-stratégique | `09_decomposition/{figures,tables}/` |
| 10 | `10_robustness/10_robustness.R` | **NEW (squelette)** | §6 placebos, fenêtres, triangulation, sender | `10_robustness/{figures,tables}/` |

> **Partie validité dissoute (ex-`07_validity`, squelette jamais implémenté,
> archivé `_archive/validity_skeleton/`)** : les **pré-tendances** et **HonestDiD**
> sont intégrés au PPML (`07_ppml`) ; la **balance/SMD** (love plot + SMD calibrés
> sur la capacité d'absorption) est construite dans `06_descriptives_did`. Tout le reste descend d'un cran
> (ex-`08`→`07`, `09`→`08`, `10`→`09`, `11`→`10`).

Chaque partie `06`–`10` contient aussi `NN_report.md` (rapport de partie).
**Squelette** = en-tête + bootstrap `source` + `PART` + lecture des entrées +
bloc `## TODO (feuille de route §X)`. Aucun résultat n'y est fabriqué.

> **`EventStudy` scindé par producteur** : les sorties Sun-Abraham / statique /
> validation du traitement (`*sunab*`, `*static_did*`, `*treatment_validation*`)
> vont en `07_ppml/` ; les sorties d'intensité dCDH (`*dcdh*`, `*tier*`,
> `*russia_cases*`) vont en `08_dcdh/`. Le routage est automatique via `PART`.

## Ordre d'exécution

```
00 (sourcé)  ->  01  ->  02  ->  03  ->  04  ->  05        (données + traitement)
             ->  06 (descriptives + DiD : 06_descriptives_did)  (§1)
             ->  07 (PPML : statique, type, 2x2, Sun-Abraham, pré-tendances, HonestDiD)  (§2-3)
             ->  08 (dCDH intensité)                        (§4)
             ->  09 (décomposition)                         (§5, après résultat propre sur le total)
             ->  10 (robustesse)                            (§6)
```

Dépendances de données : `04` (votes) et `05` (covariables/2×2) alimentent le
2×2 de `07`. La balance/SMD est en `06_descriptives_did`. `09` et `10`
réutilisent les meilleures specs de `07`/`08`.

## Matrice traitement × outcome × estimateur (feuille de route)

| Traitement | Outcome | Estimateur | Script |
|---|---|---|---|
| Sanction (dummy) | Total | PPML statique + Sun-Abraham | `07` |
| Type : commercial / non-commercial | Total | PPML statique (réplique GSDB-R4 col. 2) | `07` |
| Intensité (paliers) | Total (logs) | AVSQ `did_multiplegt_dyn` + `dist_lag_het` | `08` |
| Condamnation ONU (dummy) | Total | PPML statique + DiD 2×2 autour de 2022 | `07` |
| 2×2 condamne × sanctionne | Total | PPML statique (interactions) | `07` |
| Sanction non-commerciale | Strat. / non-strat. | Event study PPML + AVSQ | `09` |
| Intensité (paliers) | Strat. / non-strat. | AVSQ + `dist_lag_het` | `09` |

## Archivé (rien supprimé) — tout sous `_archive/` à la racine

- **`_archive/`** (code) : `iv/` (IV abandonnée), `ipd_robustness/` (`08a`–`8d`,
  `09`), `legacy_descriptives/` (originaux `03a/b/c`), `fused_sources/`
  (originaux `04`, `10`, `11*`, + **`08_ppml_legacy.R`** = event study mondial
  pré-refonte), `iv_legacy/` (`build_iv_panel.R`), `validity_skeleton/`
  (ex-`07_validity`, dissous), `backups/` (`.bak`). Détail : `_archive/README.md`.
- **`_archive/output_legacy/`** (sorties legacy) : `08_ppml/` (figures/tables de
  l'event study mondial pré-refonte), `Figures/Estimation_IV(_alternative)`,
  `Tables/Estimation_IV(_alternative)`, `Tables/Robustness_legacy`,
  `Tables/Robustness_ipd`, `Figures/Interaction_degraisse`.
- **`Reports/_archive/`** : `2026-06-23_audit_pipeline.md` (remplacé par ce
  fichier), `2026-06-16_recap_analyse.md`, `2026-06-22_verifications_variables_robustesse.md`
  (distillés dans `annexe_ipd_iv.md`).
- **`Data/Clean/_archive/`** (hors dépôt git, volumineux) : caches `_baci_*`,
  `iv_panel_backup_*`.

## Rapports

- **Central `Reports/`** (synthèse + transversaux) : `README_pipeline.md`,
  `VARIABLES.md`, `report_sanctions_synthese.md`, `annexe_ipd_iv.md`, `_archive/`.
- **Par partie** : `06_descriptives/06_report.md`,
  `06_descriptives_did/06_descriptives_did_report.md`, `07_ppml/07_report.md`,
  `08_dcdh/08_report.md` (ex-`report_intensity_dcdh_phase3.md`),
  `09_decomposition/09_report.md`, `10_robustness/10_report.md`.
