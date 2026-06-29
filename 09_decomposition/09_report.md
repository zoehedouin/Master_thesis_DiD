# Rapport — Partie 09 : décomposition par bucket × direction (§5)

> *Produit par `09_decomposition.R`. Rejoue les meilleures specs du total (07 PPML
> Sun-Abraham, 08 dCDH paliers, 07 2×2 statique) sur les **3 buckets MECE** de `02`
> (`embargo` / `strategic_nonembargo` / `nonstrategic_nonembargo`, somme = `trade_value`),
> **séparément par direction** (sous-panel `RUS exportateur` = le monde importe de
> Russie ; `RUS importateur` = le monde exporte vers la Russie). Buckets **non
> recalculés** (lus dans `master_panel_with_strategic.parquet`). Exclusion
> reporting-gap `BLR_RUS` (ancre 08). Sorties : `tables/tab_t1..t3*.csv`,
> `figures/fig_t1_eventstudy.png`, `figures/fig_t3_static_2x2.png`. Chiffres réels,
> renvoi aux CSV ; aucun chiffre fabriqué.*

**La direction EST le design** (pas une robustesse) : l'audit de `02` a montré une
asymétrie forte (énergie côté export russe, non-stratégique massif côté import russe).
Tout est estimé **par sous-panel directionnel** ; rien n'est collapsé.

**Spec FE identifiée (testée, documentée).** Sur un sous-panel mono-direction, `exp=RUS`
(ou `imp=RUS`) est constant : `exp^year`/`imp^year` dégénèrent, et le terme
partenaire×temps **absorbe le traitement** (vérifié : `partner^year` → `treated_post`
colinéaire/absorbé dans les **deux** directions). Spec retenue partout : **`pkey + year`**
(TWFE étagé, Sun-Abraham), cluster paire, zéros gardés (PPML). Panel : 7 328 obs,
229 partenaires, 44 traités (cohortes 2008/2014/2022/2023, 2014 dominante).

---

## Tier 1 — Event study PPML par bucket × direction (`tab_t1_*`, `fig_t1_eventstudy.png`)

**ATT agrégé (post k≥0)** :

| direction | bucket | ATT | p |
|---|---|---:|---:|
| RUS exportateur | total | −0.696 | <0.001 |
| RUS exportateur | **embargo** | **−0.758** | <0.001 |
| RUS exportateur | strategic_ne | −0.104 | 0.63 (n.s.) |
| RUS exportateur | **nonstrat_ne** | **−0.467** | <0.001 |
| RUS importateur | total | −0.566 | <0.001 |
| RUS importateur | **embargo** | **−0.789** | <0.001 |
| RUS importateur | strategic_ne | −0.265 | 0.12 (n.s.) |
| RUS importateur | **nonstrat_ne** | **−0.541** | <0.001 |

**Lecture (mécanique vs fragmentation).** L'effet ne se limite **pas** au commerce
banni : le bucket **`nonstrat_ne` (commerce AUTORISÉ, ni embargo ni stratégique)
recule fortement et significativement dans les deux directions** (−0.47 export, **−0.54
import**). C'est la **vraie fragmentation** (de-risking / sur-conformité sur du commerce
licite), pas un simple effet mécanique du ban. Le bras embargo (−0.76 / −0.79) est lui
attendu (mécanique + redirection). Le bucket `strategic_nonembargo` est **mince et
non significatif** (IC larges, cf. caveat 2).

**GATE pré-tendances (NON masqué).** Les leads sont mitigés (`tab_t1_eventstudy`,
colonne `pretrend_flag`) :
- **Propres (ok)** : `nonstrat_ne | RUS_exportateur`, `embargo | RUS_importateur`.
- **DRIFT signalé** : `total` (2 sens), `embargo|RUS_exp`, `strategic_ne` (2 sens),
  et **`nonstrat_ne | RUS_importateur`**. Pour ce dernier (le résultat phare), le
  drapeau est tiré par le **lead −5 binné (+0.30, p<0.001)** — un artefact de bord ;
  les leads proches **−4/−3/−2 sont plats** (+0.06/+0.04/+0.03, n.s.). Le résultat
  reste donc lisible mais **à reporter avec ce caveat de pré-tendance**.

---

## Tier 2 — dCDH AVSQ ciblé (paliers d'intensité) (`tab_t2_*`)

Estimateur `did_multiplegt_dyn` (paliers `tier` 0/1/2-5/6+, effects=4, placebo=2,
cluster paire), **uniquement sur les buckets gras** par direction. **ATE** :

| modèle | transf. | ATE | IC95 |
|---|---|---:|---|
| **embargo \| RUS_exp** | log+1 | **−0.321** | [−0.516 ; −0.125] |
| embargo \| RUS_exp | IHS | −0.334 | [−0.541 ; −0.128] |
| **nonstrat_ne \| RUS_imp** | log+1 | **−0.160** | [−0.279 ; −0.041] |
| nonstrat_ne \| RUS_imp | IHS | −0.161 | [−0.291 ; −0.031] |
| strategic_ne \| RUS_imp *(exploratoire)* | log+1 | −0.146 | [−0.349 ; +0.058] n.s. |

**Lecture.** À la marge d'**intensité**, un cran de dose réduit le commerce **autorisé**
(`nonstrat_ne|RUS_imp`) de **−0.16** (significatif) : la fragmentation se renforce avec
l'escalade, pas seulement à l'allumage. **Robustesse transformation OK** : log+1 et IHS
**coïncident** (écart au 3ᵉ chiffre) même sur ces buckets à forte part de zéros → pas de
divergence en queue gauche, robustesse gratuite. `strategic_ne` reste **exploratoire**
(IC traverse 0, ne pas mettre au même rang).

---

## Tier 3 — PPML statique 2×2 (vote ONU) par cellule × bucket × direction (`tab_t3_*`, `fig_t3_static_2x2.png`)

Vote = date unique 2022 → **statique** (pas d'event study). `i(cell_2022, post2022,
ref = Neither)`, FE `pkey + year`. Effets clés :

| direction | bucket | `a_both` (condamne+sanctionne) | `b_condemn_only` (condamne seul) |
|---|---|---:|---:|
| RUS_exp | embargo | −1.67*** | −0.68* |
| RUS_exp | **nonstrat_ne** | **−0.97*** | **+0.08 (n.s.)** |
| RUS_imp | embargo | −2.25*** | −0.60 (p=0.07) |
| RUS_imp | **nonstrat_ne** | **−1.39*** | **−0.50 (p=0.064)** |

**Lecture.**
- **`a_both` (bras matériel complet)** : négatif **partout**, y compris sur le commerce
  **autorisé** (`nonstrat_ne` −0.97 export / **−1.39 import**) → les sanctionneurs-
  condamneurs coupent à la fois l'embargo (mécanique) ET l'autorisé (fragmentation).
- **`b_condemn_only` (condamne seul, sans sanction)** : recul sur l'**embargo côté
  export** (−0.68, énergie → **absorption/redirection** plutôt qu'effet propre), **nul
  sur l'autorisé côté export** (+0.08 n.s.), et **marginalement négatif sur l'autorisé
  côté import** (−0.50, p=0.064). → **résidu expressif faible** (de-risking sans
  sanction) sur les exports occidentaux vers la Russie, à ne pas sur-vendre (marginal,
  cohérent avec la fragilité du canal expressif vue en 07).

---

## Caveats (câblés)

1. **Redirection (RUS exportateur).** Le DiD capte la chute **sanctionneur-spécifique**
   du commerce, **PAS la recette russe agrégée** : l'Occident cesse d'acheter, mais le
   brut/charbon part vers la Chine/Inde. Un embargo −0.76 côté export russe ≠ −76 % de
   revenus russes. Interprétation = réorientation des flux, pas effondrement des recettes.
2. **`strategic_nonembargo` mince → IC larges.** Robustesse optionnelle prévue (fusion
   avec `nonstrategic_nonembargo` = « tout hors-embargo ») : non encore tournée comme
   spec ; à ajouter si besoin, **hors spec principale**.
3. **« Embargo ≈ surtout énergie » = en partie un artefact de valeur.** HS27 pèse lourd
   **par construction** (l'énergie est massive en valeur). Formuler « les flux embargés
   sont **dominés en valeur** par l'énergie », pas « les sanctions portent surtout sur
   l'énergie ».
4. **Plancher de magnitude (hérité 08).** L'OLS-log (Tier 2) **sous-estime** vs PPML aux
   fortes intensités ; les magnitudes dCDH sont un **plancher**. Le Tier 1 (PPML) est la
   référence d'amplitude.

## À retenir
La décomposition tranche : **l'effet déborde du commerce banni vers le commerce
autorisé** (`nonstrat_ne` significatif dans les deux sens, surtout import −0.54 ; dose
−0.16) = **fragmentation réelle**, portée par le bras matériel complet (`a_both`) et
non par la condamnation seule (résidu expressif marginal). Caveat de pré-tendance sur
le phare import (drapeau tiré par le bord −5 ; leads proches plats).
