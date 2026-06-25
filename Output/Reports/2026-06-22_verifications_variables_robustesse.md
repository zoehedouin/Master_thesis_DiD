# Vérifications sur les variables de robustesse — scripts `08a` → `8d`

*Date : 2026-06-22*

Ce document récapitule **tous les contrôles effectués sur les mesures
alternatives de distance géopolitique** avant et pendant leur utilisation
en robustesse de mesure. Il complète :

- [`VARIABLES.md`](VARIABLES.md) — définitions, sources, formules des 11
  variables construites dans `06_build_geopol_measures.R` ;
- [`2026-06-16_recap_analyse.md`](2026-06-16_recap_analyse.md) — chemin
  général du projet (baseline PPML, exploration IV, pivot robustesse).

Les variables concernées sont les 7 substituts de l'IPD retenus :
`polyarchy_dist`, `ideol_dist`, `polity_dist`, `allied_atop`,
`shared_rival_mid`, `sanction_nontrade`, `n_common_sanctioners`.

Toutes les valeurs citées renvoient à un CSV source précis. Les tables
principales sont à la racine de
[`Output/Tables/Robustness/`](../Tables/Robustness/) ; les diagnostics
auxiliaires sont dans
[`Output/Tables/Robustness/_archive/`](../Tables/Robustness/_archive/).

---

## 0. Vue d'ensemble : qu'est-ce qui a été vérifié

| Script | Question posée | Sorties |
|---|---|---|
| `08a_sample_attrition.R` | **Pourquoi** chaque mesure couvre-t-elle si peu d'obs ? (origine de l'attrition) | `_archive/tab_sample_attrition.csv` |
| `08b_identifiability_checks.R` | Les mesures sont-elles **identifiables** ? Perd-on de la valeur ? Y a-t-il **sélection** ? | `_archive/tab_identifiability.csv`, `_archive/tab_ideol_selection.csv` |
| `08c_robustness_measure.R` | **Substitution** de l'IPD : les signes/magnitudes sont-ils cohérents ? | `tab_own_sample.csv`, `tab_palier_{A,B,C}.csv`, `tab_robustness_synthesis.csv`, `report_robustness.md` |
| `8d_common_sample_diagnosis.R` | Pourquoi le coef IPD **change de signe** sur le sample commun ? | `_archive/tab_8d_{coverage,covariate_balance,anchor}.csv` |

La logique est séquentielle : `08a`/`08b` qualifient les variables
(couverture, identifiabilité, sélection) **avant** de les utiliser ; `08c`
fait la substitution proprement dite ; `8d` diagnostique l'anomalie révélée
par `08c` (le retournement de signe sur sample commun).

---

## 1. `08a` — Origine de l'attrition d'échantillon

**Idée.** Chaque mesure couvre beaucoup moins que le panel principal
(1 593 900 obs). La perte vient de deux sources qu'on veut séparer :

1. **troncature temporelle** (ex. MID s'arrête en 2014, Polity en 2018) ;
2. **couverture pays**, qui pèse de façon **quadratique** sur les dyades :
   si `c%` des pays sont couverts, seules `~c²%` des paires le sont (les
   *deux* pays d'une paire doivent être présents).

Base = obs avec `trade_value` non-NA. Source :
[`_archive/tab_sample_attrition.csv`](../Tables/Robustness/_archive/tab_sample_attrition.csv).

| Mesure | % panel | Fenêtre | Couv. pays | Couv. dyad théo. (c²) | Perte temporelle | Perte intra-fenêtre |
|---|---|---|---|---|---|---|
| polyarchy_dist | 56.2% | 1995–2024 | 75.3% | 56.7% | 0% | **100%** |
| ideol_dist | 16.1% | 1995–2023 | 56.7% | 32.2% | 4% | 96% |
| polity_dist | 36.6% | 1995–2018 | 70.1% | 49.2% | 31.5% | 68.5% |
| allied_atop | 80.0% | 1995–2018 | 100% | 100% | **100%** | 0% |
| shared_rival_mid | 66.7% | 1995–2014 | 100% | 100% | **100%** | 0% |
| sanction_nontrade | 96.7% | 1995–2023 | 100% | 100% | 100% | 0% |
| n_common_sanctioners | 96.7% | 1995–2023 | 100% | 100% | 100% | 0% |

**Lecture — deux régimes d'attrition nets :**

- **Familles *strategic* et *sanctions*** (allied, shared_rival, sanctions) :
  couverture pays = 100%, perte **entièrement temporelle**. Ces mesures sont
  exhaustives sur les pays mais tronquées en fin de période (ATOP→2018,
  MID→2014, GSDB→2023). Leur restriction d'échantillon est une question de
  *fenêtre*, pas de *sélection de pays*.

- **Famille *institutional*** (polyarchy, polity, ideol) : la perte est
  dominée par l'**effet quadratique de la couverture pays**. Pour
  `polyarchy_dist`, la couverture dyadique théorique (56.7%) coïncide
  presque exactement avec le % réel du panel (56.2%) → la perte est *purement*
  l'effet quadratique de la couverture pays V-Dem (75%), sans NA internes.
  Pour `ideol_dist`, le réel (16.1%) tombe *sous* le théorique (32.2%) → il y
  a en plus des trous intra-fenêtre (gaps DPI), point repris en `08b(C)`.

→ Conséquence directe : l'**échantillon commun** aux mesures (utilisé en
`08c`/`8d`) est borné par la **troncature MID (≤2014)** côté temps et par la
**couverture pays V-Dem/DPI** côté composition. Ce n'est pas un échantillon
aléatoire du panel.

---

## 2. `08b` — Identifiabilité, valeur retenue, sélection

Trois contrôles courts **avant** d'utiliser les mesures comme régresseurs.
Sources :
[`_archive/tab_identifiability.csv`](../Tables/Robustness/_archive/tab_identifiability.csv),
[`_archive/tab_ideol_selection.csv`](../Tables/Robustness/_archive/tab_ideol_selection.csv).

### (A) Part de **valeur** commerciale retenue vs part de dyades

`ecart_val_dyad = %valeur − %dyades`. Mesure si l'échantillon d'une variable
sur-représente (positif) ou sous-représente (négatif) les **gros flux**.

| Mesure | % dyades | % valeur | Écart valeur−dyades |
|---|---|---|---|
| polyarchy_dist | 56.2% | 95.5% | **+39.3** |
| ideol_dist | 16.1% | 52.7% | +36.6 |
| polity_dist | 36.6% | 61.6% | +25.0 |
| allied_atop | 80.0% | 69.1% | −10.9 |
| shared_rival_mid | 66.7% | 52.5% | −14.1 |
| sanction_nontrade | 96.7% | 94.5% | −2.2 |
| n_common_sanctioners | 96.7% | 94.5% | −2.2 |

**Lecture :**
- `polyarchy_dist` retient 95.5% de la **valeur** mondiale avec seulement
  56% des dyades → V-Dem couvre presque tous les **gros traders** ; les
  paires droppées sont des micro-flux. La comparaison de son coef à celui de
  l'IPD est donc relativement « propre » côté valeur.
- `allied_atop` / `shared_rival_mid` ont un écart **négatif** : ils perdent
  *plus* de valeur que de dyades (troncature post-2014/2018 = on coupe les
  années récentes, qui pèsent lourd en valeur). Comparaison baseline à
  relativiser.
- Les sanctions (écart ≈ −2) sont quasi-représentatives du panel en valeur.

### (B) Ratio de variance **within** après FE three-way

Analogue du **0.083** de l'IPD (cf. recap §2) : part de la variance de la
mesure qui survit à la projection sur `exp×year + imp×year + pair`. C'est la
variance qui *identifie réellement* le coefficient.

| Mesure | within_ratio | Lecture |
|---|---|---|
| allied_atop | **0.086** | ≈ IPD : à peine identifiée, surtout absorbée par les pair FE |
| polity_dist | 0.149 | modérée |
| polyarchy_dist | 0.157 | modérée |
| sanction_nontrade | 0.162 | modérée |
| n_common_sanctioners | 0.266 | bonne |
| ideol_dist | 0.656 | forte (mais petit échantillon, cf. (C)) |
| shared_rival_mid | 0.688 | forte |

**Lecture :** un within_ratio proche de 0 (allied_atop, et l'IPD elle-même à
0.083) signifie que la mesure est presque entièrement **structurelle**
(between-pair) et absorbée par les pair FE. Un coef non-significatif sur ces
variables doit se lire **« non identifiée »** et **non « effet nul »**. À
l'inverse, `ideol_dist` et `shared_rival_mid` varient beaucoup *within-pair*
— mais sur des sous-échantillons restreints (16% et 67%), donc précision et
représentativité limitées.

### (C) Sélection de `ideol_dist` sur le niveau de démocratie

On teste si la **manquance** de `ideol_dist` (DPI) dépend du régime, en
croisant avec `joint_dem_vdem` (plancher de démocratie de la paire). Source :
[`_archive/tab_ideol_selection.csv`](../Tables/Robustness/_archive/tab_ideol_selection.csv).

| Tranche de démocratie jointe | n | % `ideol_dist` manquant |
|---|---|---|
| [0–0.2] (autocraties) | 259 858 | **82.6%** |
| (0.2–0.4] | 302 350 | 86.5% |
| (0.4–0.6] | 188 882 | 64.5% |
| (0.6–0.8] | 107 568 | 53.0% |
| (0.8–1] (démocraties) | 37 834 | **27.2%** |

**Lecture :** la manquance décroît nettement des paires autocratiques vers
les paires démocratiques → **sélection confirmée** : `ideol_dist` (codage
`execrlc` gauche/centre/droite de DPI) manque surtout pour les paires
autocratiques (partis non codés, gouvernements militaires). Son échantillon
est biaisé vers les démocraties. Combiné aux hypothèses fortes du construit
documentées dans [`VARIABLES.md`](VARIABLES.md) (cardinalité imposée sur une
variable ordinale, idéologie *économique* ≠ alignement *de politique
étrangère*), **`ideol_dist` est la mesure la plus fragile** et est traitée
comme telle dans `08c` (annexe, hors paliers principaux).

---

## 3. `08c` — Robustesse de mesure : substitution de l'IPD

**Spec.** Exactement la Spec 4 « workhorse » de `04_gravity_estimation.R` :

```
fepois(trade_value ~ X + rta | exp_year + imp_year + pair, vcov = ~pair)
```

en remplaçant `X` par l'IPD puis par chaque mesure alternative. `mid_direct`
est ajouté comme **contrôle** quand `X = shared_rival_mid` (conflit direct vs
rivalités indirectes, cf. [`VARIABLES.md`](VARIABLES.md)).

### 3.1 Chaque variable sur son propre échantillon maximal

Source :
[`tab_own_sample.csv`](../Tables/Robustness/tab_own_sample.csv).

| Variable | Fenêtre | N estim. | coef | SE | p |
|---|---|---|---|---|---|
| **ipd** (baseline) | 1995–2024 | 965 872 | **−0.0663** | 0.0318 | 0.037 |
| polyarchy_dist | 1995–2024 | 805 994 | −0.0408 | 0.0617 | 0.509 |
| polity_dist | 1995–2018 | 534 884 | +0.0052 | 0.0020 | 0.011 |
| allied_atop | 1995–2018 | 752 739 | +0.0228 | 0.0248 | 0.359 |
| shared_rival_mid | 1995–2014 | 607 569 | +0.0222 | 0.0083 | 0.007 |
| sanction_nontrade | 1995–2023 | 932 281 | −0.0805 | 0.0198 | <0.001 |
| n_common_sanctioners | 1995–2023 | 932 281 | +0.0733 | 0.0174 | <0.001 |

**Attention aux signes attendus.** Les mesures n'ont pas toutes la même
orientation : `polyarchy_dist`, `polity_dist`, `ideol_dist`,
`shared_rival_mid`, `sanction_nontrade` sont des **distances/hostilités**
(signe négatif attendu si l'effet géopolitique freine le commerce) ;
`allied_atop` et `n_common_sanctioners`… ici `allied_atop` est une *proximité*
(signe positif attendu) tandis que `n_common_sanctioners` est ambigu (statut
« pariah » conjoint). Sur leurs échantillons propres, **seules les sanctions
non-trade reproduisent le signe négatif de l'IPD** ; les autres sont soit
non-significatives, soit de signe opposé.

→ **Mais** ces échantillons ont des fenêtres temporelles différentes
(2014/2018/2023/2024), ce qui interdit la comparaison directe — d'où les
paliers à fenêtre contrôlée ci-dessous.

### 3.2 Paliers sur échantillon commun (fenêtre + composition contrôlées)

Trois paliers emboîtés, chacun sur l'**intersection des non-NA** des mesures
qu'il contient (donc même échantillon pour toutes les lignes d'un palier).
Sources :
[`tab_palier_A.csv`](../Tables/Robustness/tab_palier_A.csv),
[`tab_palier_B.csv`](../Tables/Robustness/tab_palier_B.csv),
[`tab_palier_C.csv`](../Tables/Robustness/tab_palier_C.csv).

**Palier A** — 6 mesures, contraint par MID (1995–2014), N = 476 778 :

| Variable | coef | p |
|---|---|---|
| **ipd** | **+0.0422** | 0.022 |
| polyarchy_dist | +0.0207 | 0.784 |
| polity_dist | +0.0022 | 0.371 |
| allied_atop | +0.0250 | 0.326 |
| shared_rival_mid | +0.0205 | 0.016 |
| sanction_nontrade | +0.0229 | 0.189 |
| n_common_sanctioners | +0.0190 | 0.076 |

**Palier B** — 5 mesures sans shared_rival, contraint par ATOP (1995–2018),
N = 563 942 : IPD = **+0.0410** (p = 0.009) ; les substituts restent
majoritairement non-significatifs (n_common_sanctioners +0.030, p = 0.033).

**Palier C** — 3 mesures core, contraint par DPI/GSDB (1995–2023),
N = 810 230 : IPD = **−0.0231** (p = 0.29, NS) ; sanction_nontrade
**−0.0804** (p < 0.001) ; n_common_sanctioners +0.0736 (p < 0.001) ;
polyarchy_dist −0.0200 (NS).

**Lecture cruciale :** sur les paliers A et B (bornés ≤2014 / ≤2018), l'IPD
est **positif et significatif** ; sur le palier C (≤2023), il bascule
négatif (NS) ; sur le full sample (→2024), il est **−0.066 significatif**.
Le signe du coefficient IPD **dépend de la fenêtre temporelle**.

### 3.3 Synthèse temporelle : temps vs composition

C'est le test décisif. Source :
[`tab_robustness_synthesis.csv`](../Tables/Robustness/tab_robustness_synthesis.csv).

**Partie 2 — composition du palier C *fixée*, fenêtre temporelle qui varie :**

| Coupure | N estim. | IPD coef | p |
|---|---|---|---|
| ≤2014 | 512 570 | +0.0416 | 0.021 |
| ≤2018 | 631 374 | +0.0419 | 0.006 |
| ≤2023 | 778 448 | −0.0231 | 0.29 |

**Partie 3 — full sample, fenêtres expansives + sous-périodes :**

| Fenêtre | N estim. | IPD coef | p |
|---|---|---|---|
| ≤2014 | 607 569 | +0.0426 | 0.018 |
| ≤2018 | 752 739 | +0.0428 | 0.005 |
| ≤2023 | 932 281 | −0.0225 | 0.30 |
| full (→2024) | 965 872 | −0.0663 | 0.037 |
| 1995–2014 | 607 569 | +0.0426 | 0.018 |
| **2015–2024** | 322 238 | **−0.1983** | <0.001 |

**Lecture :** en gardant la **composition** du palier C constante et en
faisant uniquement varier la **fenêtre** (Partie 2), l'IPD passe de positif
(≤2014, ≤2018) à négatif (≤2023). Le basculement vient donc du **temps**, pas
de la sélection de paires. La sous-période isolée **2015–2024** donne un effet
IPD **fortement négatif (−0.198, p < 10⁻⁵)** : tout l'effet négatif baseline
est porté par la décennie récente (guerres commerciales, Russie-Ukraine,
découplage US-Chine). Avant 2015, la relation IPD↔commerce est *positive*.

→ **Résultat de robustesse central :** l'effet géopolitique négatif sur le
commerce est un **phénomène récent (post-2014)**, pas une régularité de toute
la période 1995–2024. Ce n'est pas une fragilité de mesure mais une
**hétérogénéité temporelle réelle** du paramètre.

---

## 4. `8d` — Diagnostic du retournement de signe sur sample commun

**Problème soulevé par `08c`.** Sur le sample commun (intersection des 6
mesures), l'IPD est **positif** (+0.042) alors qu'il est **négatif** (−0.066)
sur le full sample. Hypothèse testée : le sample commun serait biaisé vers de
grandes économies développées, alignées, où la complémentarité économique
domine la friction géopolitique.

### 4.1 Ancrage : full vs commun

Source :
[`_archive/tab_8d_anchor.csv`](../Tables/Robustness/_archive/tab_8d_anchor.csv).
Spec 4 stricte, aucun changement.

| Échantillon | N estim. | IPD coef | p |
|---|---|---|---|
| full | 965 872 | −0.0663 | 0.037 |
| commun | 446 171 | **+0.0422** | 0.022 |

Le retournement est reproduit (et le commun coïncide exactement avec le
palier A de `08c` : +0.0422, p = 0.022 → cohérence inter-scripts vérifiée).

### 4.2 Couverture du sample commun

Source :
[`_archive/tab_8d_coverage.csv`](../Tables/Robustness/_archive/tab_8d_coverage.csv).

| Groupe | N | % panel | % valeur | % zéros |
|---|---|---|---|---|
| commun | 476 778 | 46.2% | 51.2% | 26.2% |
| reste | 554 714 | 53.8% | 48.8% | 34.8% |

Le sample commun pèse 46% des dyades mais 51% de la valeur et contient
**moins de zéros** (26% vs 35%) → légèrement orienté vers des paires
commercialement actives, mais l'écart est modéré.

### 4.3 Équilibre des covariables (SMD commun vs reste)

Source :
[`_archive/tab_8d_covariate_balance.csv`](../Tables/Robustness/_archive/tab_8d_covariate_balance.csv).
SMD = différence standardisée des moyennes.

| Variable | moy. commun | moy. reste | SMD |
|---|---|---|---|
| log_dist | 8.68 | 8.81 | **−0.169** |
| comlang_off | 0.127 | 0.173 | −0.131 |
| exp/imp_gdppc | 11 335 | 13 218 | −0.109 |
| contig | 0.022 | 0.012 | +0.073 |
| **ipd** | 0.921 | 0.874 | +0.065 |
| rta | 0.336 | 0.313 | +0.051 |
| trade_value | 405 602 | 331 691 | +0.017 |

**Lecture :** tous les SMD sont **petits** (|SMD| < 0.17). Le sample commun
est marginalement plus proche géographiquement (log_dist plus faible) et un
peu moins riche en PIB/hab — c'est-à-dire **l'inverse** de l'hypothèse de
départ (« grandes économies développées alignées »). L'IPD elle-même est très
peu déséquilibrée (SMD +0.065). **La composition n'explique donc pas le
retournement.**

### 4.4 Conclusion `8d`

Le retournement de signe sur le sample commun **n'est pas un effet de
composition** (covariables quasi-équilibrées, SMD tous < 0.17) mais un
**artefact de fenêtre temporelle** : le sample commun est borné à **≤2014**
par la troncature MID (cf. `08a`), donc il capture mécaniquement la période où
l'effet IPD est positif. Ce diagnostic **converge avec la Partie 2 de `08c`**
(composition fixée, fenêtre variable → c'est le temps qui retourne le signe).

Les deux scripts, par deux chemins indépendants (l'un fixe la composition et
bouge le temps, l'autre compare les compositions à fenêtre subie), aboutissent
à la **même conclusion** : *time, not composition*.

---

## 5. Synthèse des vérifications

1. **Attrition (`08a`)** — comprise et décomposée : *strategic*/*sanctions*
   perdent par troncature temporelle (pays 100% couverts) ;
   *institutional* perd par effet quadratique de la couverture pays. Le
   sample commun est borné ≤2014 (MID).

2. **Identifiabilité (`08b`)** — `allied_atop` est à peine identifiée
   (within 0.086, ≈ IPD) → ses NS se lisent « non identifiée ». `polyarchy`
   retient l'essentiel de la valeur mondiale. **`ideol_dist` est sélectionnée
   sur le régime** (manque surtout chez les autocraties) → la plus fragile,
   reléguée hors paliers principaux.

3. **Substitution (`08c`)** — aucune mesure alternative ne reproduit
   *proprement* l'IPD sur toute la période ; seules les sanctions non-trade
   partagent son signe négatif (mais canal partiellement tautologique, cf.
   `VARIABLES.md`). Le fait saillant n'est pas une discordance de mesure mais
   une **hétérogénéité temporelle** : l'effet géopolitique négatif est
   **post-2014** (sous-période 2015–2024 : IPD = −0.198, p < 10⁻⁵ ;
   avant 2015 : positif).

4. **Diagnostic du sample commun (`8d`)** — le retournement de signe est un
   **artefact de fenêtre (≤2014)**, pas de composition (SMD covariables tous
   < 0.17). Confirme la lecture temporelle de `08c`.

**Implication pour le mémoire.** Le baseline PPML 3-way FE (IPD = −0.066) tient
sur la période complète, mais sa robustesse révèle qu'il **agrège deux régimes**
: une ère pré-2015 où alignement ONU et commerce ne sont pas (ou positivement)
liés, et une ère post-2014 de découplage géopolitique marqué. C'est le
résultat de robustesse à mettre en avant — plus informatif qu'une simple
table « le signe tient / ne tient pas ».

---

## 5bis. Extension à toute l'échelle de specs (`09`)

> **Ajout 2026-06-22 (soir)** : `08c` n'estimait la substitution que sur la
> **Spec 4** (workhorse). Le script
> [`09_estimations_robustesse_completes.R`](../../Codes/09_estimations_robustesse_completes.R)
> rejoue **l'intégralité de l'échelle de specs de `04`** (Spec 1–10 + Rob1–4,
> via un registre paramétré) sur les 5 mesures retenues, dans les paliers
> communs A/B/C, en période propre (palier D), et sur une grille temporelle.
> Sorties :
> [`report_estimations.md`](../Tables/Robustness/report_estimations.md),
> [`tab_grille_mesures.csv`](../Tables/Robustness/tab_grille_mesures.csv),
> [`tab_grille_temporelle.csv`](../Tables/Robustness/tab_grille_temporelle.csv).
> Tous les coefficients Spec 4 reproduisent exactement `tab_own_sample.csv`.

Deux résultats consolident la lecture « temps, pas mesure » :

**(a) Lecture appariée (Spec 4, palier D).** Comparée à l'IPD estimé sur *son
propre* échantillon (même fenêtre), **chaque mesure de distance/hostilité
partage le signe de l'IPD apparié** :

| Mesure | Période | coef mesure | IPD apparié | même signe |
|---|---|---|---|---|
| polyarchy_dist | 1995–2024 | −0.041 | −0.067 | ✓ |
| polity_dist | 1995–2018 | +0.005 | +0.041 | ✓ |
| shared_rival_mid | 1995–2014 | +0.022 | +0.043 | ✓ |
| sanction_nontrade | 1995–2023 | −0.081 | −0.023 | ✓ |
| n_common_sanctioners | 1995–2023 | +0.073 | −0.023 | ✗ (pariah, ambigu) |

→ Les signes « positifs » de `polity_dist` et `shared_rival_mid` **ne
contredisent pas l'IPD** : leur couverture s'arrête avant 2015, dans l'ère où
l'IPD apparié est lui-même positif. Seul `n_common_sanctioners` diverge — par
construction (statut pariah, signe non imposé).

**(b) La bascule post-2014 est structurelle, pas un artefact de Spec 4.** Le
passage positif (≤2014/2018) → négatif (2015–2024) **se reproduit sur toute la
famille FE three-way** : Spec 4 (−0.198), Spec 5 (−0.198), Spec 6 strategic
(−0.175), Spec 7 non-strategic (−0.204), Rob2 no-micro (−0.200), Rob3 post-2002
(−0.198) — tous p < 0.01 sur 2015–2024. La Spec 10 `i(period, ipd)` donne une
**intensification monotone** : +0.002 (1995-2007) → −0.041 → −0.094 → −0.144 →
**−0.212 (2022-2024)**. Les specs *sans* `pair` FE (Spec 2/3) restent positives
(effet de niveau *between-pair*) ; Spec 1 (OLS) et Rob1 (quadratique) sont
d'une autre nature — la dispersion verticale du signe vient donc du contraste
*within* (pair FE) vs *between*, pas d'une fragilité du résultat temporel.

---

## 6. Inventaire des sorties

**Racine [`Output/Tables/Robustness/`](../Tables/Robustness/) (livrables `08c`) :**
- `tab_own_sample.csv` — 7 variables sur échantillon propre
- `tab_palier_A.csv`, `tab_palier_B.csv`, `tab_palier_C.csv` — paliers communs
- `tab_robustness_synthesis.csv` — synthèse temporelle (Parties 2 et 3)
- `report_robustness.md` — narratif `08c`

**Racine (livrables `09`, toute l'échelle de specs) :**
- `tab_grille_mesures.csv` — long : palier(A/B/C/D)/mesure/spec/geovar/term/coef/se/p/n
- `tab_grille_temporelle.csv` — long : base/fenêtre/spec/geovar/term/coef/se/p/n
- `report_estimations.md` — narratif `09` (lecture appariée + bascule × specs)

**[`Output/Tables/Robustness/_archive/`](../Tables/Robustness/_archive/) (diagnostics) :**
- `tab_sample_attrition.csv` — `08a`
- `tab_identifiability.csv`, `tab_ideol_selection.csv` — `08b`
- `tab_8d_coverage.csv`, `tab_8d_covariate_balance.csv`, `tab_8d_anchor.csv` — `8d`
- versions antérieures de `08c` (own_sample, common_sample, tiers, time_isolation…)
