# Rapport — Sun-Abraham OLS, panel pkey (`08_sunab_ols.R`)

> *Partie **08_ols** = monde OLS/log de l'intensité des sanctions (par opposition au
> PPML **dirigé** du 07). Estimateur B = event study Sun & Abraham (2021) en **OLS sur
> `log(trade+1)`**, panel **paire NON ORDONNÉE (pkey)**, FE `pkey + year`, cluster
> paire. feols gère les FE creux → panel pkey **COMPLET** (aucun sous-échantillonnage ;
> c'est une contrainte du dCDH, pas de feols). Vraie estimation, chiffres réels.
> Sorties : [`tables/tab_sunab_ols.csv`](tables/tab_sunab_ols.csv),
> [`figures/es_fig_sunab_ppml_vs_ols.png`](figures/es_fig_sunab_ppml_vs_ols.png).*

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

## Comparaison A (07) vs B — pont QUALITATIF (caveat explicite)
| | A : PPML **dirigé** (07, full 2008-2023) | B : OLS **pkey** `log(trade+1)` |
|---|---:|---:|
| ATT | **−0.600** (se 0.055) | **−0.096** (se 0.041) |
| k=0 | −0.105 | −0.163 |
| k=5 (binné) | **−0.889** | −0.068 |

Figure `es_fig_sunab_ppml_vs_ols.png` : les deux profils en overlay.

**⚠️ Caveat de comparabilité (central).** Passer de A à B fait varier **SIMULTANÉMENT
trois axes** :
1. **Échelle** : PPML (multiplicatif, Poisson) → **OLS sur `log(trade+1)`**.
2. **Géométrie** : dyade **dirigée** + FE exportateur-temps / importateur-temps
   (résistance multilatérale, MRT) → **paire non ordonnée (pkey)** + FE `pkey + year`
   (**sans MRT**).
3. **Définition du traitement** : sanction **non-commerciale dirigée partenaire→Russie**
   → **toute sanction** (`n_active_core>0`) sur la paire non ordonnée (deux directions,
   tous types, **toutes les dyades sanctionnant-sanctionné du monde**, pas seulement la
   Russie).

→ A→B est donc un **pont qualitatif de bout en bout**, **PAS** une mesure propre de la
seule **linéarité** (échelle). L'écart A (−0.60) vs B (−0.10) **mélange** les trois
effets — l'essentiel de la dilution vient de la géométrie pkey-sans-MRT et du traitement
« monde toutes-sanctions » (qui noie l'effet Russie dans 5 509 paires ever-traitées
mondiales). La comparaison **propre** de l'échelle (A′ : même géométrie/traitement, seul
PPML→OLS) est **abandonnée** ; l'**isolation de la STRUCTURE** viendra du saut **B→C**
(`dist_lag_het`, script suivant).

**B est structurellement aveugle à 2022.** Le traitement Sun-Abraham est **binaire
absorbant** à l'onset (≈2014 pour la cohorte dominante) ; 2022 = **post-onset** (rel ≈ +8
pour la cohorte 2014, **absorbé dans le bin +5**). B ne peut donc pas distinguer
l'**intensification** 2022 de l'onset 2014 — c'est précisément ce que **C** (dist_lag_het,
effet contemporain vs retardés) capturera.

## Réserves / suite
- Le choix de transformation reste **`log(trade+1)`** (cf. `08_zeros_report.md` :
  IHS ≈ identique, cor 0.9994 ; positifs-seuls biaiserait par sélection).
- Prochain script : **C = dist_lag_het** (§4.2, skeleton conservé dans `08_ols.R`) pour
  séparer l'effet contemporain (choc 2022) des effets accumulés depuis 2014, sur le même
  panel pkey + la même exclusion `pairs_reporting_gap`.
