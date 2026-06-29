# PPML — bootstrap pigeonhole multinomial (dépendance dyadique)

> *Méthode : **pigeonhole multinomial**, Davezies, D'Haultfœuille & Guyonvarch
> (2021, *Annals of Statistics*, §2.3). Population pays unique (exp & imp tirés du
> même ensemble) ; comptes multinomiaux W ; poids dyade = W[exp]×W[imp] ; refit
> `fepois` pondéré ; **B = 200** réplications. p-value = mean(|θ*−θ̂| > |θ̂|).*

**⚠️ Limite intrinsèque (pays focal unique).** Le traitement est concentré sur **la Russie**. Le tirage multinomial sur la population de pays met **W[RUS]=0 avec proba ≈ e⁻¹ ≈ 37 %** ; dans ces réplications, toutes les dyades-Russie ont un poids nul → plus aucune variation de traitement → le coefficient n'est pas identifié (draw **dégénéré**, compté en échec). C'est pourquoi `fail_rate ≈ 0,37` (≈ 68/200 draws W[RUS]=0) — **ce n'est pas un bug** mais une propriété du bootstrap pays avec un pays focal. Les SE/IC/p ci-dessous sont donc calculés sur les ~63 % de réplications **non dégénérées**, et s'interprètent comme une inférence robuste à la dépendance dyadique **conditionnelle à la présence de la Russie dans le rééchantillon**.

Comparaison SE **clusterisée-paire** (existante) vs SE/IC/**p** **bootstrap dyadique** :

| modèle | coef | θ̂ | SE paire | p paire | SE boot | IC95 boot | p boot | conv. |
|---|---|---:|---:|---:|---:|---|---:|---:|
| static_treated_post | treated_post | -0.5807 | 0.0741 | 0.0000 | 0.0835 | [-0.7195 ; -0.3943] | 0.0000 | 66% |
| type_contrast_dir | rus_tr | -0.0664 | 0.1672 | 0.6915 | 0.2581 | [-0.6053 ; 0.3421] | 0.8106 | 66% |
| type_contrast_dir | rus_nt | -0.4813 | 0.1401 | 0.0006 | 0.1777 | [-0.8254 ; -0.2153] | 0.0000 | 55% |
| did_2x2_cell_x_post2022 | cell_2022::a_both:post2022 | -1.2814 | 0.1831 | 0.0000 | 0.3608 | [-1.6769 ; -0.3593] | 0.0076 | 66% |
| did_2x2_cell_x_post2022 | cell_2022::b_condemn_only:post2022 | -0.4398 | 0.1998 | 0.0277 | 0.3997 | [-0.9717 ; 0.5507] | 0.2045 | 66% |

## Significativités qui changent (seuil 5 %)
- **did_2x2_cell_x_post2022 / cell_2022::b_condemn_only:post2022** : p paire = 0.0277 (sig) → p boot = 0.2045 (n.s.) — significativité PERDUE.

> ⚠️ **Avertissement** : taux d'échec de convergence jusqu'à 45% sur certains coefs (>10%) — résultats conservés mais à lire avec prudence.

*Convergence des réplications : 55%–66%. Sanity : moyenne bootstrap ≈ θ̂ (écart max 0.1202).*
