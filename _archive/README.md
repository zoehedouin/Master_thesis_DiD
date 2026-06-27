# `_archive/` (code) — scripts retirés du pipeline actif

Rien n'est supprimé : tout ce qui sort du pipeline est **déplacé** ici (règle de
sécurité des réorganisations `reorg-did` puis `reorg-by-part`). Ces fichiers
restent exécutables tels quels, mais ils ne font plus partie de la feuille de
route DiD (sanctions vs votes ONU, centrée Russie). Voir `../MIGRATION_LOG.md`
pour la trace complète des déplacements et `../Reports/README_pipeline.md` pour
la carte du pipeline actif. (Sorties legacy : `output_legacy/`.)

## `iv/` — exploration IV abandonnée comme identification principale
Tentative d'instrumenter la distance d'idéal-point (IPD) sur le commerce. Aucun
instrument n'est simultanément fort, excluable et stable (synthèse :
`../Reports/annexe_ipd_iv.md`). Abandonnée au profit du design DiD.

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
+ blocs IPD/interaction) est fusionné et recentré Russie dans `../06_descriptives/06_descriptives.R`.
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
| `04_gravity_estimation.R` | ex-`08_ppml` (event study **mondial** pré-refonte → voir `08_ppml_legacy.R`). |
| `10_event_study_sanctions.R` | ex-`08_ppml` (idem). |
| `08_ppml_legacy.R` | **event study mondial pré-refonte** (fusion ex-`04`+ex-`10`, 14 cohortes, ~26 000 paires ; ATT mondial historique = **−0,265**). **Remplacé** par le PPML **Russie-centré** réécrit à neuf en `../07_ppml/07_ppml.R`. |
| `11_intensity_dcdh.R` | `../08_dcdh/08_dcdh.R` (AVSQ intensité en paliers). |
| `11b_dcdh_outputs.R` | `../08_dcdh/08_dcdh.R` (sorties/figures dCDH). |
| `11c_dcdh_by_tier.R` | `../08_dcdh/08_dcdh.R` (effets par palier). |
| `11d_robustness.R` | `../08_dcdh/08_dcdh.R` (robustesses dCDH). |

## `validity_skeleton/` — partie validité dissoute (ex-`07_validity`)
Squelette jamais implémenté. Dissous lors de la renumérotation : les **pré-tendances**
et **HonestDiD** sont intégrés au PPML (`../07_ppml/07_ppml.R`) ; la **balance/SMD**
sera construite à neuf dans `../06_descriptives_did/`. Conservé pour mémoire.

| Fichier | Note |
|---|---|
| `07_validity.R` | Squelette §2 (balance, pré-tendances, HonestDiD), jamais implémenté. |
| `07_report.md` | Stub de rapport associé. |

> Sorties legacy de l'event study mondial : `../output_legacy/08_ppml/{figures,tables}/`
> (`es_fig01_sunab_2014.png`, `tab_eventstudy_sunab.csv`, `tab_static_did.csv`, etc.).

## `iv_legacy/` — constructeur du panel IV (hors pipeline actif)
Le code des familles d'instruments IV (distances + relations), extrait du
constructeur de traitement lors de sa scission en `03_sanctions/03_build_sanctions.R`
(actif, sanctions pur) + ce builder archivé.

| Fichier | Rôle |
|---|---|
| `build_iv_panel.R` | Lit `Data/Clean/sanctions_panel.parquet`, dérive `polyarchy_dist`/`joint_dem_vdem` des niveaux `exp/imp_polyarchy` déjà présents, reconstruit `ideol_dist` (DPI), `polity_dist` (Polity5), `allied_atop`+`shared_ally_atop` (ATOP), `mid_direct`+`shared_rival_mid` (MID). Écrit `Data/Clean/_archive/iv_panel.parquet`. **Lu par aucun script actif** ; sert à reproduire l'historique IV (consommateurs dans `iv/`, `ipd_robustness/`). |

## `backups/` — sauvegardes de fichiers
| Fichier | Note |
|---|---|
| `06_build_geopol_measures.R.bak_20260624` | Sauvegarde `.bak` de l'ancien `06` (renommé `03_build_treatments.R` puis scindé en `03_build_sanctions.R`). |

> Backups **de données** (caches `_baci_*`, `iv_panel_backup_*.parquet`) :
> volumineux et hors dépôt git, ils sont archivés à part dans
> `Data/Clean/_archive/` pour ne pas gonfler le dépôt de code. Re-lancer `01`/`02`
> régénère les caches `_baci_*` dans `Data/Clean/`.
