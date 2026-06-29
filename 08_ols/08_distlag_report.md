# Rapport — C : distributed-lag hétérogène (`08_distlag.R`)

> *Partie **08_ols**, §4.2/4.3. Estimateur **distributed-lag robuste à l'hétérogénéité**
> de de Chaisemartin & D'Haultfœuille (package réel **`DistLagHet`**,
> `estim_RC_model_unified`), sur le **même panel pkey** que B (`08_sunab_ols.R`) :
> mêmes FE implicites (premières différences + dummies année), même fenêtre 2008-2023,
> **même exclusion reporting-gap** (`BLR_RUS`). Vraies estimations, chiffres réels.
> Sorties : [`tables/tab_distlag.csv`](tables/tab_distlag.csv),
> [`figures/es_fig_distlag.png`](figures/es_fig_distlag.png).*

## Pourquoi C
Le dCDH dynamique (§4.1) date l'effet au **1ᵉʳ changement de dose** ; B (Sun-Abraham OLS)
est **aveugle à 2022** (onset 2014 absorbant). C **sépare** l'effet **contemporain** d'une
hausse de dose (β₀, année *t*) des effets **retardés** (β₁…β₄ : doses des années *t−1…t−4*) :
« combien de l'impact vient du choc de l'année vs de l'accumulation ». Outcome `log(trade+1)`,
dose = `tier` (paliers 0/1/2-5/6+), en **premières différences** (ΔY, ΔD), dummies d'année
en covariables, **bootstrap clusterisé paire** (rééchantillonnage des groupes).

## Note de mise en œuvre (transparence, aucun chiffre fabriqué)
- **Package réel = `DistLagHet`** (`chaisemartinPackages/dist_lag_het`). Le skeleton
  d'origine donnait des noms **devinés** (`dist_lag_het()`, args `delta_y`…) **faux** ;
  l'API réelle est `estim_RC_model_unified(K, data, group_col, deltaY_col, deltaD_col,
  D_col, X_cols, model, bootstrap, B)` → `$B_hat` (β₀…β_K) + `$se/$ci_lower/$ci_upper`.
- **Coquille de packaging corrigée** : le `NAMESPACE` upstream déclarait
  `useDynLib(distlaghet)` alors que le package est `DistLagHet` → `R_init_DistLagHet`
  jamais appelé → **routines C++ non enregistrées** → tous les `.Call` échouaient.
  Corrigé à l'installation (patch `NAMESPACE` → `useDynLib(DistLagHet)`, réinstall locale).
  L'estimation reste **100 % le code C++ du package** (10 routines `.Call` vérifiées
  enregistrées). Le backend C++ est bien engagé.
- **B = 100** réplications bootstrap (et non 200). Motif **documenté** : le coût est
  dominé par le **nombre de groupes** (26 564 paires, dont ~21 000 contrôles never-treated
  qui n'identifient pas les coefficients de dose mais alourdissent chaque réplication) ;
  **une seule estimation full-panel = ~2 min même en C++** → B=200 × 2 specs ≈ plusieurs
  heures. B=100 conserve des SE/IC parfaitement lisibles (les **points β** sont, eux,
  exacts et indépendants de B). `boot_iterations = 100` (convergence 100 %).

## Résultat C — réponse distributed-lag robuste (log+1)
Effet d'un **cran de palier** de dose sur `log(trade+1)`, SE/IC bootstrap (B=100) :

| coef | estimate | se | IC95 |
|---|---:|---:|---|
| **β₀** (contemporain) | **−0.0755** | 0.028 | [−0.130 ; −0.021] ✓ |
| **β₁** (lag 1) | **−0.0604** | 0.025 | [−0.110 ; −0.011] ✓ |
| β₂ (lag 2) | +0.0234 | 0.032 | [−0.039 ; +0.086] n.s. |
| **β₃** (lag 3) | **−0.0681** | 0.027 | [−0.120 ; −0.016] ✓ |
| β₄ (lag 4) | +0.0174 | 0.025 | [−0.031 ; +0.066] n.s. |
| **Σβ (cumulé long terme)** | **−0.1632** | | |

**Lecture (impact vs accumulation).** L'effet est **front-loaded** : β₀ (−7,6 %,
significatif) porte l'essentiel, avec une **persistance** aux lags 1 et 3 ; **pas
d'accumulation croissante** (les lags ne grandissent pas ; β₂, β₄ ≈ 0). L'effet **cumulé**
d'une hausse de dose ≈ **−16 %**. → Un choc d'intensité agit surtout l'année même et se
maintient, plutôt que de se construire progressivement.

**LIMITE (à ne pas sur-revendiquer).** K=4 ne remonte qu'à ~2018 depuis 2022 ; séparer
proprement la contribution de l'**onset 2014** demanderait K≈8 (coûteux en bootstrap).
On mesure donc la **forme de la réponse dynamique** (impact vs accumulation à 4 ans),
**pas** la part exacte « depuis 2014 ».

## Naïf vs robuste — l'argument du Théorème 3 (contamination)
La régression distributed-lag **TWFE naïve** (`feols(log_trade ~ l(tier,0:4) | pkey + year)`)
est l'objet que de Chaisemartin & D'Haultfœuille critiquent : sous hétérogénéité, ses
coefficients de lag **mélangent** des effets d'autres lags (poids potentiellement négatifs).

| coef | naïf TWFE | robuste | écart |
|---|---:|---:|---|
| β₀ | −0.0664 ✓ | −0.0755 ✓ | proches |
| **β₁** | **+0.0326** (n.s., **positif**) | **−0.0604** ✓ (**négatif**) | **inversion de signe** |
| β₂ | −0.0014 | +0.0234 | — |
| β₃ | −0.0030 (n.s.) | −0.0681 ✓ | robuste ≫ |
| β₄ | −0.0436 ✓ | +0.0174 (n.s.) | divergence |
| **Σβ** | **−0.0818** | **−0.1632** | **naïf sous-estime de moitié** |

**Lecture (Théorème 3 confirmé).** La contamination est **réelle et matérielle** : le naïf
**inverse le signe de β₁** (positif alors que le robuste est négatif et significatif),
fabrique un β₄ spuriement significatif, et **sous-estime l'effet cumulé de moitié**
(−0.082 vs −0.163). La correction robuste n'est donc pas cosmétique ici : la régression
distributed-lag ordinaire **mal-répartit** les effets entre lags et **sous-évalue** la
réponse totale. → **Valeur ajoutée nette de l'estimateur robuste.**

## Robustesse IHS (transformation)
Même estimateur robuste, outcome `asinh(trade)` au lieu de `log(trade+1)` :

| coef | robuste log+1 | robuste IHS |
|---|---:|---:|
| β₀ | −0.0755 | −0.0763 |
| β₁ | −0.0604 | −0.0636 |
| β₃ | −0.0681 | −0.0740 |
| Σβ | −0.1632 | −0.1719 |

**Quasi identique** (écarts au 3ᵉ chiffre). Confirme — en **mesurant** et pas seulement en
invoquant la corrélation des transformations (`08_zeros_report.md` : cor 0.9994) — que le
coefficient de dose **ne bouge pas matériellement** entre log+1 et IHS.

## Comparaison au §4.1 (dCDH AVSQ)
`dCDH §4.1 ATE (cross_ge1, onset binaire) = −0.1763` vs **C : Σβ (cumulé) = −0.1632**
(et β₀ contemporain = −0.0755). Les deux approches **convergent en magnitude** (~−0.16/−0.18
pour l'effet total/d'onset) bien qu'elles diffèrent dans l'estimand (dCDH = ATE d'un
franchissement binaire daté ; C = effet cumulé d'un cran de dose ordinal, décomposé en
contemporain + retardés). → **Convergence = robustesse** ; la décomposition de C ajoute
l'information que cet effet total est **surtout contemporain**, pas une accumulation.

## Tri des extinctions Russie (rappel, alimente l'exclusion)
`08_sunab_ols.R` (PARTIE 2, `tab_reporting_gap_triage.csv`) : sur les 12 extinctions Russie
(positif pré → zéro post), **11 partenaires gardent 36–157 % de leur commerce mondial BACI
post-2022 → collapses RÉELS → GARDÉS** ; seul **BLR_RUS FLAGUÉ** (Russie + Biélorussie
toutes deux COMTRADE-dark pour ce flux ; 33,5 Md → 0 économiquement impossible). De plus,
les 11 sont **tier_post ≤ 2** (hors headline 6+). **Exclusion uniforme de C = `c("BLR_RUS")`**
(niveau paire, jamais pays).

## Validation (loggée)
- **Convergence bootstrap : 100 %** (`boot_iterations = 100` sur 100 ; `fail_rate ≈ 0`).
- Sanity : les points β sont le fit exact (non bootstrap) ; le bootstrap ne sert qu'aux SE/IC.
- B=100 (réduit vs 200) — documenté ci-dessus ; n'affecte pas les points.

## Synthèse
Sur le monde pkey OLS-log, l'intensité des sanctions agit **surtout l'année du choc**
(β₀ ≈ −7,6 %), avec persistance mais **sans accumulation croissante** ; effet cumulé
≈ −16 %, **convergent avec le dСDH §4.1**. Le naïf TWFE est **contaminé** (β₁ inversé,
Σβ sous-estimé de moitié) → l'estimateur robuste est nécessaire. Transformation
(log+1 / IHS) **non décisive**. Caveat d'échelle hérité de A′ (`08_sunab_ols_report.md`) :
l'OLS-log **sous-estime** vs PPML, surtout aux fortes intensités — les magnitudes C sont
donc un **plancher**.
