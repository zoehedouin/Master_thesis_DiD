# `Codes/_archive/` — scripts retirés du pipeline actif

Rien n'est supprimé : tout ce qui sort du pipeline est **déplacé** ici (règle de
sécurité de la réorganisation `reorg-did`). Ces fichiers restent exécutables tels
quels, mais ils ne font plus partie de la feuille de route DiD (sanctions vs votes
ONU, centrée Russie). Voir `../../MIGRATION_LOG.md` pour la trace complète des
déplacements et `../../Output/Reports/README_pipeline.md` pour la carte du
pipeline actif.

## `iv/` — exploration IV abandonnée comme identification principale
Tentative d'instrumenter la distance d'idéal-point (IPD) sur le commerce. Aucun
instrument n'est simultanément fort, excluable et stable (synthèse :
`../../Output/Reports/annexe_ipd_iv.md`). Abandonnée au profit du design DiD.

| Fichier | Rôle d'origine |
|---|---|
| `05_gravity_iv.R` | Première passe IV (gravité 2SLS), instruments alliances/MID. |
| `05c_gravity_iv_exploration.R` | Exploration systématique des stratégies d'instruments. |
| `07_estimation_iv_alternative.R` | Estimation IV sur mesures géopolitiques alternatives. |
| `07b_first_stage_diagnostics.R` | Diagnostics de première étape (F-stat, signes). |
| `07c_estimation_iv_clean.R` | Version « propre » de l'estimation IV. |

## `ipd_robustness/` — robustesse de mesure IPD (legacy)
Robustesse de la mesure d'alignement IPD et de l'échantillon, branchée sur les
consommateurs IV désormais archivés. La motivation réutilisable (hétérogénéité
temporelle de l'IPD : positif ≤2014, négatif post-2014) est reprise dans
`annexe_ipd_iv.md`.

| Fichier | Rôle d'origine |
|---|---|
| `08a_sample_attrition.R` | Attrition de l'échantillon commun. |
| `08b_identifiability_checks.R` | Vérifications d'identifiabilité. |
| `08c_robustness_measure.R` | Robustesse au choix de mesure IPD. |
| `8d_common_sample_diagnosis.R` | Diagnostic de l'échantillon commun (nommage `8d` incohérent avec `08*`, conservé tel quel). |
| `09_estimations_robustesse_completes.R` | Robustesses complètes (anciennes specs IPD). |

## `legacy_descriptives/` — originaux fusionnés dans le nouveau `06_descriptives.R`
Les trois scripts descriptifs d'origine. Leur contenu réutilisable (socle général
+ blocs IPD/interaction) est fusionné et recentré Russie dans `../06_descriptives.R`.
Conservés ici comme référence du calcul d'origine.

| Fichier | Rôle d'origine |
|---|---|
| `03a_desc_trade.R` | Descriptives commerce (structure générale, séries, OTAN). |
| `03b_desc_geopolitics.R` | Descriptives IPD (distribution, heatmap, cartes). |
| `03c_desc_interaction.R` | Descriptives interaction commerce × IPD (14 figures). |

## `fused_sources/` — sources des fusions d'estimation
Originaux fusionnés dans les nouveaux scripts d'analyse. Conservés pour tracer le
pipeline numérique d'origine (les fusions reprennent leur logique à l'identique).

| Fichier | Fusionné dans |
|---|---|
| `04_gravity_estimation.R` | `../08_ppml.R` (échine PPML statique + type + 2×2). |
| `10_event_study_sanctions.R` | `../08_ppml.R` (event study Sun & Abraham). |
| `11_intensity_dcdh.R` | `../09_dcdh.R` (AVSQ intensité en paliers). |
| `11b_dcdh_outputs.R` | `../09_dcdh.R` (sorties/figures dCDH). |
| `11c_dcdh_by_tier.R` | `../09_dcdh.R` (effets par palier). |
| `11d_robustness.R` | `../09_dcdh.R` (robustesses dCDH). |

## `backups/` — sauvegardes de fichiers
| Fichier | Note |
|---|---|
| `06_build_geopol_measures.R.bak_20260624` | Sauvegarde `.bak` de l'ancien `06` (renommé `03_build_treatments.R`). |

> Backups **de données** (caches `_baci_*`, `iv_panel_backup_*.parquet`) :
> volumineux et hors dépôt git, ils sont archivés à part dans
> `Data/Clean/_archive/` pour ne pas gonfler le dépôt de code. Re-lancer `01`/`02`
> régénère les caches `_baci_*` dans `Data/Clean/`.
