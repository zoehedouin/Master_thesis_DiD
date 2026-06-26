# Rapport — Phase 1 : event study Sun & Abraham (sanctions non-commerciales)

> *Réorg `reorg-did` : `10_event_study_sanctions.R` est désormais fusionné dans
> `Codes/08_ppml.R`. Carte du pipeline à jour : `README_pipeline.md`.*

*Scripts : `10_event_study_sanctions.R`. Données : `iv_panel.parquet`.
Traitement = `sanction_nontrade` (GSDB-R4), pour éviter la tautologie de
l'embargo. Spec PPML identique à `04` : FE `exp_iso3^year + imp_iso3^year +
pair`, `cluster = ~pair`, zéros gardés. Chiffres exacts, sans interprétation.*

C'est la pièce « propre » pour **2014 et le canal** (matériel vs fragmentation),
complémentaire du dCDH (escalade / 2022, cf. `report_intensity_dcdh_phase3.md`).

---

## 1. Validation du traitement

- **Paires (non ordonnées) traitées** : 7 260 ; **jamais-traitées** : 19 305.
- **Left-censored** : 2 707 paires ont leur onset en 1995 (déjà sous sanction au
  début du panel, aucune pré-période).
- **Tailles de cohortes par année d'onset** (extrait) : 2005 = 442, 2012 = 369,
  **2014 = 390**, 2009 = 174, 2017 = 153, **2022 = 210**, 2023 = 78 ; nombreuses
  petites cohortes (2018 = 6, 2021 = 10, 2019 = 14).
- **Sanity check Russie** : **37 nouveaux partenaires sanctionnés en 2014**
  (Crimée) ; seulement **3 nouveaux en 2022**. → la plupart des sanctionneurs de
  2022 avaient déjà sanctionné la Russie depuis 2014, donc leur onset est daté de
  2014. 2022 n'est pas un nouvel onset mais une intensification (traité en Phase 3).
- **Caveat** : `sanction_nontrade` est NA pour toute l'année 2024 ; `treated_post`
  dérivé de l'onset (scalaire) reste valide. Construction symétrique (undirected),
  onset défini au niveau de la paire non ordonnée.

## 2. DiD statique + contraste par type

PPML 3-way FE, `cluster = ~pair` (toutes paires, zéros gardés) :

| modèle | terme | estimate | SE | p |
|---|---|---|---|---|
| ancre | `treated_post` (non-commercial) | **−0.0529** | 0.0285 | 0.064 |
| contraste | `sanction_any` | −0.0916 | 0.0163 | <0.001 |
| contraste | `sanction_trade` | **−0.1271** | 0.0238 | <0.001 |
| contraste | `sanction_nontrade` | **−0.0426** | 0.0180 | 0.018 |

- Repères GSDB-R4 (Yalcin et al. 2025) : any ≈ −0.07 ; trade ≫ non-trade en
  magnitude. **Retrouvé** : `any` −0.092 ; `trade` −0.127 ≫ `nontrade` −0.043
  (rapport ≈ 3×). Le codage des types est validé.
- Lecture : la sanction commerciale capte en partie le canal **mécanique**
  (embargo coupe le commerce par construction) → gardée comme contraste/plafond.
  L'event study reste sur le **non-commercial** (canal de fragmentation).
- L'ancre `treated_post` −0.053 est proche du ≈ −0.08 de robustesse `04`.

## 3. Event study Sun & Abraham (non-commercial)

`sunab(cohort, year)`, fenêtre **2008-2023**, onset ≥ 2009, **14 cohortes**
traitées + jamais-traités comme contrôles ; bornes binnées à ±5 ; N = 554 946.

| temps relatif | estimate | SE | IC 95 % |
|---|---|---|---|
| −5 | −0.0261 | 0.035 | [−0.096, +0.043] |
| −4 | +0.0373 | 0.043 | [−0.048, +0.122] |
| −3 | −0.0063 | 0.042 | [−0.089, +0.077] |
| −2 | −0.0329 | 0.031 | [−0.093, +0.027] |
| 0 (transition) | −0.1217 | 0.034 | [−0.188, −0.055] |
| +1 | −0.2870 | 0.062 | [−0.408, −0.166] |
| +2 | −0.2630 | 0.067 | [−0.394, −0.133] |
| +3 | −0.2743 | 0.077 | [−0.425, −0.124] |
| +4 | −0.1775 | 0.052 | [−0.279, −0.076] |
| +5 | −0.2977 | 0.075 | [−0.444, −0.151] |

**ATT agrégé = −0.2655** (SE 0.0591, p < 0.001).

- **Pré-tendances (rel < 0) : PLATES.** Les 4 coefficients pré-onset sont tous
  non significatifs (IC contiennent 0), sans tendance. → validité OK.
- **k = 0 = transition** (−0.122), non utilisé pour la lecture.
- **Effet lu à partir de k = +1** : chute ≈ −0.27 à −0.29, stable de +1 à +5.
- L'ATT (−0.27) dépasse l'ancre statique (−0.053) : le statique mélange les paires
  left-censored (toujours-traitées, atténuées) ; le sunab isole des cohortes à
  onset net (2009-2022) et lit l'effet post-transition.

Figure : `Output/Figures/EventStudy/es_fig01_sunab_2014.png`.
Tables : `tab_treatment_validation.csv` (+ `_meta`), `tab_static_did.csv`,
`tab_eventstudy_sunab.csv`.

## 4. Problèmes rencontrés

1. **Bug `nobs` dans `extract_coefs`** : la colonne `model` masquait l'objet
   `model` → `nobs(model)` appelé sur une chaîne. Corrigé (capter `nobs()` avant
   d'ajouter la colonne).
2. **NSE `bin.rel`** : `sunab(..., bin.rel = bin_spec)` ne résout pas la variable
   externe (fixest évalue dans l'env. de la formule). Corrigé en inlinant les
   bornes numériques via `sprintf`/`as.formula`.
3. **Mémoire (8 Go)** : le sunab sur le panel complet 1995-2023 (~27 cohortes,
   ~400 termes) thrashait (RSS > 2.8 Go, CPU 13 %, pageouts), sans converger en
   ~20 min CPU. **Repli** : fenêtre 2008-2023 + exclusion des cohortes pré-fenêtre
   → 14 cohortes, N = 554 946, convergence en ~8 min. (I/O chemin accentué :
   wrappers tempfile-ASCII, cf. Phase 3.)
