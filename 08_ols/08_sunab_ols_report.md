# Rapport — Sun-Abraham OLS, panel pkey (`08_sunab_ols.R`)

> *Partie **08_ols** = monde OLS/log de l'intensité des sanctions (par opposition au
> PPML **dirigé** du 07). Estimateur B = event study Sun & Abraham (2021) en **OLS sur
> `log(trade+1)`**, panel **paire NON ORDONNÉE (pkey)**, FE `pkey + year`, cluster
> paire. feols gère les FE creux → panel pkey **COMPLET** (aucun sous-échantillonnage ;
> c'est une contrainte du dCDH, pas de feols). Vraie estimation, chiffres réels.
> Sorties : [`tables/tab_sunab_ols.csv`](tables/tab_sunab_ols.csv) (B),
> [`tables/tab_sunab_pkey_ppml.csv`](tables/tab_sunab_pkey_ppml.csv) (A′),
> [`tables/tab_reporting_gap_triage.csv`](tables/tab_reporting_gap_triage.csv) (tri),
> [`figures/es_fig_sunab_ppml_vs_ols.png`](figures/es_fig_sunab_ppml_vs_ols.png) (A/A′/B).*

## Panel (verbatim §4.1)
`sanctions_panel.parquet` ; pkey non ordonnée ; 2008-2023 ; `trade_tot =
sum(trade_value)` ; `n_active_core = max(...)` ; `log_trade = log(trade_tot+1)`.
→ 425 040 obs / 26 565 paires.

## Exclusion « trou de reporting » (prudence, sans imputation)
Extinctions sous traitement (ever-traitées, positif pré 2018-2021 → **zéro réel** post
2022-2023) : **375 paires (12 Russie)** — liste Russie reproduite du diagnostic zéros
(`08_zeros_report.md`) : BLR_RUS, RUS_SDN, RUS_YEM, LBY_RUS, IRQ_RUS, AFG_RUS, PRK_RUS,
HTI_RUS, RUS_SOM, ERI_RUS, RUS_SSD, RUS_SLE.

Vecteur **auditable** `pairs_reporting_gap <- c("BLR_RUS")` (en tête de script), ancré
sur la **Biélorussie** : sa bascule ~33,5 Md USD → 0 est invraisemblable comme vrai
effondrement (commerce BLR-RUS en hausse post-2022) → lacune de déclaration BACI (les
deux côtés éteints, le miroir ne reconstruit rien). Règle : toute paire pkey contenant
« BLR » **et** « RUS » ; **extensible**. **Exclusion au niveau PAIRE, jamais pays** : la
Russie reste (c'est le traitement) ; les paires Russie-Occident restent (le partenaire
occidental déclare → flux réel). Retrait : **−1 paire, −16 obs**.

Après exclusion + retrait des **left-censored** (onset 2008, pas de pré-période :
−2 977 paires, comme la variante full-window du 07) : panel d'estimation **377 392 obs,
23 587 paires**, **15 cohortes traitées**, **21 056 never-treated**.

## Résultat B — Sun-Abraham OLS
`feols(log_trade ~ sunab(cohort, year, bin ±5) | pkey + year, cluster = ~pkey)`.
- **ATT (agg. fixest, post k≥0) = −0.096** (se 0.041, **p = 0.020**) ; moyenne post
  **k≥+1 = −0.109**.
- Profil (réf. pré-onset −1) : leads plats sauf le **−5 binné** (−0.107, blip de bord) ;
  k0 −0.163, k1 −0.132, k2 −0.194, k3 −0.095, k4 −0.055, **k5 −0.068**.

En clair : sur le panel monde pkey, l'allumage des sanctions (onset, ~2014 dominant)
réduit le commerce bilatéral d'environ **−10 %**, effet modéré et **plat dans le temps**
(pas de creusement vers le haut k).

## Décomposition MESURÉE A → A′ → B (addendum A′)
A′ = **PPML-sunab sur le MÊME panel pkey que B** (même cohortes, mêmes bornes ±5, même
exclusion BLR_RUS, mêmes left-censored exclus) : **seule l'échelle change** vs B (fepois /
zéros natifs au lieu de feols / `log(trade+1)`). Sortie : `tab_sunab_pkey_ppml.csv`.

| | A : PPML **dirigé** (07, full) | A′ : PPML **pkey** | B : OLS **pkey** `log+1` |
|---|---:|---:|---:|
| **ATT** | **−0.601** (se 0.055) | **−0.306** (se 0.052) | **−0.096** (se 0.041) |
| k=0 | −0.105 | −0.120 | −0.163 |
| k=5 (binné) | −0.889 | **−0.459** | −0.068 |

**Décomposition de l'écart (ATT) :**
- **A → A′ = +0.295** (de −0.601 à −0.306) = effet du **bundle géométrie + traitement**
  (dirigé + MRT → pkey « monde toutes-sanctions »).
- **A′ → B = +0.210** (de −0.306 à −0.096) = effet de l'**ÉCHELLE PURE** (PPML → OLS
  `log+1`, **même panel**).

**Conclusion (les chiffres tranchent — l'affirmation précédente est FALSIFIÉE).** La
dilution **n'est PAS « surtout » la géométrie/traitement** : les deux axes pèsent
**à peu près moitié-moitié** (≈ 0.30 vs ≈ 0.21). **Le passage PPML → OLS `log+1` est
conséquent** : il réduit l'ATT de moitié sur le même panel, et **bien davantage dans la
queue d'escalade** (k=5 : A′ −0.459 vs B −0.068 — PPML pondère les gros flux que
`log+1` écrase). → **Caveat sérieux pour B ET C** (tous deux en échelle OLS-log) : ils
**sous-estiment** l'effet multiplicatif, surtout aux fortes intensités. La comparaison
propre de l'échelle n'est donc **plus abandonnée** : elle est ici mesurée (A′).

Figure `es_fig_sunab_ppml_vs_ols.png` : les **trois** profils A / A′ / B en overlay.

**Pré-tendances.** A′ a des leads **plats** (−5 = −0.016, −4 = −0.003, −3/−2 ≈ +0.017,
n.s.). → Le lead **−5 de B** (−0.107, IC [−0.212 ; −0.002], marginalement négatif alors
que −4/−3/−2 sont plats) est un **bruit de lead lointain au bin-bord ±5**, PAS une
pré-tendance (A′, propre, le confirme).

**B est structurellement aveugle à 2022.** Sun-Abraham est **binaire absorbant** à l'onset
(≈2014 dominant) ; 2022 = **post-onset** (rel ≈ +8, **absorbé dans le bin +5**). B ne
distingue pas l'**intensification** 2022 de l'onset 2014 — c'est ce que **C**
(`dist_lag_het`, contemporain vs retardés) capture.

## Tri des extinctions Russie (alimente l'exclusion de C)
`tab_reporting_gap_triage.csv` : sur les **12** extinctions Russie (positif pré → zéro
post), critère reproductible = le partenaire non-russe garde-t-il un **commerce mondial
BACI** post-2022 (présent = miroir reconstruit) ? **11 partenaires gardent 36–157 % de
leur commerce mondial → collapses RÉELS → GARDÉS** ; seul **BLR_RUS FLAGUÉ** (Russie +
Biélorussie toutes deux COMTRADE-dark pour ce flux ; 33,5 Md → 0 économiquement
impossible). De plus, les 11 sont **tier_post ≤ 2** (hors headline 6+) → impact
second-ordre sur C. **Exclusion de C = `c("BLR_RUS")`** (niveau paire, jamais pays).

## Réserves / suite
- Transformation : **`log(trade+1)`** conservée (cf. `08_zeros_report.md` : IHS ≈ identique,
  cor 0.9994 ; positifs-seuls biaiserait par sélection) — mais voir le caveat d'échelle
  ci-dessus : l'OLS-log sous-estime vs PPML.
- **C = `dist_lag_het`** (`08_distlag.R`, package réel **DistLagHet**) : sépare l'effet
  contemporain (choc 2022) des retardés, même panel pkey + même exclusion. Voir
  `08_distlag_report.md`.
