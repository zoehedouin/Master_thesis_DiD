# Récapitulatif d'analyse — Mémoire M2

*Date : 2026-06-16*

Ce document retrace le chemin parcouru sur ce projet, de la construction de
la base à l'exploration IV. Toutes les valeurs citées renvoient à un
fichier source précis (script ou table CSV) ; en cas de doute, se référer
à la source.

---

## 1. Question & données

**Question** : quel est l'effet de la **distance géopolitique** sur le
**commerce bilatéral** ?

**Variable d'intérêt** : l'IPD (*Ideal Point Distance*) de Bailey,
Strezhnev et Voeten (2017), calculée comme la différence absolue entre les
ideal points estimés par un modèle Bayésien IRT sur les votes UNGA. Plus
deux pays votent différemment à l'ONU, plus leur IPD est grand.

**Variable dépendante** : le commerce bilatéral (exports en milliers USD)
agrégé depuis BACI-CEPII HS92 V202601 (1995–2024).

**Sources principales** (toutes harmonisées en ISO3 dans
`build_master_panel.R`) :
- **BACI-CEPII HS92 V202601** : flux bilatéraux par produit HS6
- **CEPII Gravity V202211** : variables time-invariant (distance,
  contiguïté, langue commune, lien colonial)
- **IPD Bailey-Strezhnev-Voeten 1946-2025** : distance dyadique
- **DESTA** : RTA dyadiques
- **NATO** : adhésions hardcodées
- **World Bank WDI** : PIB, population, inflation

**Problème d'endogénéité** : la causalité IPD ↔ commerce est *a priori*
bidirectionnelle. Deux pays qui commercent beaucoup peuvent diverger
moins à l'ONU (la richesse mutuelle favorise l'alignement) ; ou
inversement, deux pays alignés peuvent commercer plus parce que la
proximité diplomatique facilite les flux. La spécification PPML 3-way FE
réduit cette endogénéité mais ne l'élimine pas.

---

## 2. Spécification principale (script `04_gravity_estimation.R`)

**Modèle** : PPML avec trois familles de fixed effects, clusterisé par
paire de pays.

```
trade_value ~ ipd + rta | exp_iso3×year + imp_iso3×year + pair
```

Cette structure (Spec 4 « workhorse » dans
`tab_main_progression.csv`) absorbe :
- toute hétérogénéité dyadique structurelle (paire) : distance, langue,
  colonie, etc.
- les *multilateral resistance terms* d'Anderson-van Wincoop (exp×year,
  imp×year)

→ Le coefficient sur l'IPD est identifié uniquement par la variation
*within-pair* dans le temps, après contrôle des chocs pays-année.

**Résultat principal** (Spec 4, source :
[`tab_main_progression.csv`](../Tables/Estimation/tab_main_progression.csv),
[`tab_diagnostics.csv`](../Tables/Estimation/tab_diagnostics.csv)) :

| Variable | Coef | SE | p-value | N |
|---|---|---|---|---|
| **IPD** | **−0.0663** | 0.0318 | **0.037** | 965 872 |
| RTA | +0.114 | 0.023 | <0.001 | 965 872 |

**Lecture** : une augmentation **d'une unité d'IPD** (échelle Bailey-
Strezhnev-Voeten : valeur absolue de la différence des ideal points UNGA,
distribuée approximativement entre 0 et 5–6 dans nos données, médiane
≈ 1.3, cf. `tab_07c_point_estimates.csv` colonne IPD baseline) est
associée à une réduction du commerce de l'ordre de **6.6%**
(semi-élasticité PPML). À l'échelle empirique, le passage de la médiane
mondiale (~1.3) à un niveau de polarisation type US-Russie (~3) — soit
environ +1.7 unités — implique une compression du commerce de l'ordre
de **−11%** *toutes choses égales par ailleurs*. Statistiquement
significatif à 5% sous clustering par paire, marginal sous *multiway
clustering* (Spec 5 : p = 0.098, mêmes coefficients).

**Caveat d'identification** : le ratio *within/total* de la variance de
l'IPD après FE three-way est de **0.083** (cf. Section 1 du script 04).
La majeure partie de la variance d'IPD est *entre* paires (structurelle,
absorbée par pair FE) ; seule une faible part est *within-pair* sur le
temps. L'identification s'appuie sur peu de variation utile, ce qui
limite la précision.

**Hétérogénéité sectorielle** (source :
[`tab_strategic_hetero.csv`](../Tables/Estimation/tab_strategic_hetero.csv),
Specs 6 et 7) : l'effet négatif est concentré sur le commerce
**non-stratégique** (coef = −0.0728, SE = 0.0323, p = 0.024) et
**non significatif** pour le commerce stratégique (coef = −0.0126,
SE = 0.0440, p = 0.78). Une interprétation possible : les chaînes
d'approvisionnement stratégiques (semi-conducteurs, pharma, défense)
sont plus rigides à court terme que le commerce de biens substituables.

---

## 3. Exploration IV

### 3.1 Première approche : alignement aux pôles (script `05_gravity_iv.R`)

**Idée** : construire un instrument à partir de la distance dans l'espace
d'alignement aux trois pôles géopolitiques (USA, CHN, RUS), laggée de
2 ans, exclusion fondée sur la temporalité (l'alignement aux pôles à
t−2 ne peut pas avoir été causé par le commerce à t).

**Méthode** : control function (résidu inclusion 2SRI) :
1. **1er stage** OLS : `ipd ~ instrument_l2 + rta | exp×year + imp×year + pair`
2. **2nd stage** PPML : `trade_value ~ ipd + v_hat + rta | mêmes FE`

Le coefficient sur `v_hat` est un test d'endogénéité de Hausman implicite.

**Résultat** ([`tab_iv_main.csv`](../Tables/Estimation_IV/tab_iv_main.csv)) :
- PPML sur même échantillon : IPD = +0.009 (NS, p=0.62)
- CF-IV : **IPD = +0.124** (p=0.001), **v_hat sig (p<0.001)** ⇒
  endogénéité détectée
- First-stage F : ~28 400 (très fort)

**⚠️ Limite fondamentale** : l'instrument est lui-même dérivé de l'IPD
(juste à un lag différent). Corrélation `instrument_l2 ↔ ipd` ≈ 0.90.
L'exclusion repose entièrement sur l'argument temporel — fragile.

### 3.2 Instruments alternatifs externes (script `05c_gravity_iv_exploration.R`)

9 instruments testés couvrant des sources de variation différentes :
spatial lag, leave-one-out, géo×events, GDP per capita distance, etc.

**Verdict** ([`iv_synthesis_report.md`](../Tables/Estimation_IV/iv_synthesis_report.md)) :
- Sur 9 instruments, 3 sig (tous positifs) — mais ce sont les instruments
  les plus liés à l'IPD elle-même (LOO mean, spatial, lag2).
- 6 NS — ce sont les instruments les plus *vraiment externes*
  (géo×events, GDP pc dist), à intervalles de confiance trop larges
  pour conclure.
- **Pattern** : plus l'instrument est externe à l'IPD, plus il est
  imprécis. Plus il est dérivé de l'IPD, plus il est sig mais l'exclusion
  est suspecte.

### 3.3 Instruments institutionnels et stratégiques (scripts `06`, `07c`)

Construction d'un panel de **mesures dyadiques alternatives** à partir
de sources indépendantes de l'IPD :
- **Institutional** : V-Dem v16, DPI 2023, Polity5
- **Strategic relations** : ATOP v5.1, dyadic_mid v4.03
- **Sanctions** : GSDB v4 (1995–2023, ~159k dyad-years)

Définitions exactes des 11 variables construites dans
[`VARIABLES.md`](VARIABLES.md).

Specs estimées dans `07c_estimation_iv_clean.R` (table source :
[`tab_07c_point_estimates.csv`](../Tables/Estimation_IV_alternative/tab_07c_point_estimates.csv))
selon le même protocole CF-PPML 2SRI :
- S7 just-identifié : `polyarchy_dist` seul
- S7b–S7e : autres single-instruments en juste-identifié
- S8a : famille institutional (polyarchy_dist + ideol_dist)
- S8b : famille strategic (allied_atop + shared_rival_mid)
- S8c : combined (4 instruments)
- S8bis : Polity (annex robustesse mesure)

Variantes additionnelles : ces spec rejouées sur **échantillon commun**
(intersection des couvertures = 114 346 obs) pour comparabilité.

---

## 4. Diagnostics & résultats

Sources :
[`tab_07c_point_estimates.csv`](../Tables/Estimation_IV_alternative/tab_07c_point_estimates.csv),
[`tab_07c_first_stage_signs.csv`](../Tables/Estimation_IV_alternative/tab_07c_first_stage_signs.csv),
[`first_stage_per_instrument.csv`](../Tables/Estimation_IV_alternative/diagnostics/first_stage_per_instrument.csv),
[`first_stage_joint_family.csv`](../Tables/Estimation_IV_alternative/diagnostics/first_stage_joint_family.csv).

### Pertinence du premier stage

Sous cluster ~pair et FE three-way (`pair_full`) :
- Chaque instrument seul a un F effectif > 100 (de 115 pour `ideol_dist`
  à 28 406 pour `instrument_l2`).
- Familles jointes : F = 60 (institutional) à 274 (strategic).

Sans pair FE (`no_pair`), les F joints montent à 1334 (institutional) et
2726 (strategic) — **×22 à ×10**, ce qui confirme que les pair FE
absorbent l'essentiel du signal de l'IPD.

**Artefact corrigé** : un calcul antérieur (script `07.R`, ligne 50,
LHS = `log(trade+1)` au lieu de `trade_value`) produisait un Effective F
de 4.6 et un Hansen J p = 0.002 trompeurs. La version reconciliée
(script `07b`, LHS = `trade_value`) donne **KP rk = 60.4, Sargan p = 0.34,
Hansen J cluster-robust p = 0.55** pour la famille institutional ;
diagnostics propres.

### Test d'endogénéité (significativité de v_hat)

Pour la plupart des instruments crédibles conditionnellement aux FE
(S7 polyarchy, S7b ideol, S7c allied, S8a institutional, S8b strategic,
S8c combined, S8bis Polity), **v_hat n'est PAS significatif** (p entre
0.36 et 0.98).

Trois instruments font exception en détectant un v_hat significatif,
pour des raisons distinctes :

- **S8ter `instrument_l2`** (p < 0.001) : instrument *dérivé de l'IPD
  elle-même* (distance dans l'espace des positions ONU laggée). Sa
  corrélation avec l'IPD à t (≈ 0.90) est en partie de
  l'auto-corrélation, pas de l'identification d'un effet causal — la
  significativité de v_hat ici est *par construction* un artefact de
  contamination, pas un signal informatif d'endogénéité.

- **S7d `shared_rival_mid`** (p = 0.002) : *non dérivé* de l'IPD, vient
  des Militarized Interstate Disputes de Correlates of War. La
  significativité de v_hat peut venir (a) d'une corrélation
  substantielle avec la *part endogène* de l'IPD (les pays partageant
  des rivaux militaires votent souvent de manière similaire à l'ONU et
  ont des dynamiques commerciales corrélées), ou (b) d'une violation
  d'exclusion (les rivaux militaires communs affectent le commerce
  bilatéral par d'autres canaux que via l'IPD : contraintes
  géostratégiques, sanctions secondaires, partenariats régionaux). Les
  deux peuvent coexister.

- **S7e `polity_dist`** (p = 0.014) : *non dérivé* de l'IPD, vient de
  Polity5. Lecture similaire : la distance de régime politique covarie
  avec l'IPD via des canaux multiples — alignement diplomatique formel,
  préférences commerciales liées au régime (l'OMC, l'UE, le démocratie-
  développement). Difficile de distinguer mécaniquement la corrélation
  avec la part endogène de l'IPD d'une violation d'exclusion.

→ **Lecture conservatrice** : les instruments à exclusion *a priori*
défendable (institutional, strategic via allied, Polity en mesure
alternative) **ne détectent pas d'endogénéité** significative
conditionnellement aux FE three-way. Le résultat PPML baseline n'est
pas notablement biaisé par les sources de variation qu'ils captent. Les
exceptions (lag2, shared_rival, polity_dist) sont chacune attribuables
à un mélange de contamination par construction (lag2) ou de
co-déterminations contestées (shared_rival, polity_dist) — pas à un
signal univoque d'endogénéité.

### Sur-identification (Hansen J cluster-robust)

Tableau (source CSV ci-dessus) :

| Famille | Hansen J p | Lecture |
|---|---|---|
| S8a institutional | **0.55** ✓ | over-id non rejetée |
| S8b strategic | 0.005 ❌ | over-id rejetée |
| S8c combined | <0.001 ❌ | over-id rejetée |
| S8bis Polity | 0.37 ✓ | over-id non rejetée |

Seules deux familles passent Hansen J ; elles donnent **des signes
opposés** sur le coefficient IPD (institutional : −0.258 NS ; Polity :
+0.234 NS).

### Instabilité des estimations

Comparaison des coefficients IPD sur leurs propres échantillons et sur
l'échantillon commun (114 346 obs) :

| Instrument | Own sample | Commun |
|---|---|---|
| polyarchy_dist | −0.31 (NS) | **−2.03 (sig)** |
| ideol_dist | −0.15 (NS) | −0.11 (NS) |
| allied_atop | +0.32 (NS) | +0.96 (NS) |
| shared_rival_mid | **−0.55 (sig)** | **+0.72 (sig)** ⚠ signe inversé |
| polity_dist | **+1.45 (sig)** | **+5.42 (sig)** |
| instrument_l2 | **+0.12 (sig)** | +0.08 (NS) |

→ Les estimations CF varient de **−2 à +5** selon l'instrument et
l'échantillon. Le baseline PPML lui-même change de signe selon
l'échantillon (full : −0.066 sig ; commun : +0.011 NS).

**Trois causes coexistent pour expliquer cette instabilité**, sans qu'on
puisse les démêler proprement avec les données :

1. **LATEs / compliers hétérogènes** : chaque instrument identifie
   l'effet causal sur une sous-population différente de paires
   (« compliers ») dont la variation d'IPD est expliquée par cet
   instrument. `polyarchy_dist` identifie des paires où la divergence
   démocratique change ; `shared_rival_mid` identifie des paires où le
   réseau de conflits évolue ; `polity_dist` cible un sous-espace
   distinct encore. Les *Local Average Treatment Effects* peuvent
   légitimement différer entre ces compliers, même sous instruments
   tous valides.

2. **Violations d'exclusion** : pour plusieurs instruments, l'hypothèse
   d'exclusion est défendable mais pas inattaquable. Les sanctions
   financières affectent le commerce via la coupure du financement ;
   les alliances via les externalités de sécurité (Gowa & Mansfield
   1993) ; la démocratie via les institutions et la qualité contractuelle.
   Une partie de la variance des coefficients vient probablement de ces
   canaux indirects, pas de l'effet géopolitique pur.

3. **Composition d'échantillon** : les instruments ne couvrent pas les
   mêmes pays-années (cf. couverture dans `VARIABLES.md`). DPI couvre
   16% du panel, V-Dem 56%, MID s'arrête en 2014. L'échantillon commun
   à toutes les mesures (114 346 obs) est une sous-population restreinte
   où la relation IPD-commerce n'est pas la même que sur le panel
   complet — le baseline PPML lui-même change de signe (−0.066 → +0.011).
   Comparer les coefs sur des échantillons différents revient à
   comparer des paramètres différents.

Ces trois causes se combinent. **On ne peut pas, avec les données
disponibles, attribuer la divergence à une seule d'entre elles.** Cette
indistinction est elle-même un résultat — elle disqualifie l'IV comme
méthode d'identification principale.

---

## 5. Conclusion IV

Aucun instrument bilatéral testé n'est simultanément :
1. **fort** au sens KP/Sargan/Hansen sur cluster ~pair,
2. **excluable** sans hypothèse contestable (les sanctions et les
   alliances ont des canaux directs sur le commerce ; la démocratie
   et l'idéologie aussi),
3. **stable** entre instruments et entre échantillons.

L'instabilité observée (Section 4) résulte d'un **mélange** de trois
causes — LATEs/compliers différents, violations partielles d'exclusion,
et composition d'échantillon — que les données ne permettent pas de
séparer. Cohérent avec les difficultés documentées dans la littérature
(par exemple Cevik, FMI 2024).

**Décision méthodologique** : l'IV ne sert plus d'identification
principale dans le mémoire. Elle est rétrogradée en **sonde de
transparence en annexe**, servant à montrer (a) qu'on a essayé, (b)
pourquoi ça ne marche pas, et (c) que le baseline PPML reste le
résultat le plus défendable.

---

## 6. Pivot robustesse — ✅ fait (scripts `08a`–`8d`)

> **Mise à jour 2026-06-22** : ce pivot est **réalisé**. Les scripts
> `08a`–`8d` ont été écrits et exécutés ; les tables sont dans
> [`Output/Tables/Robustness/`](../Tables/Robustness/). Le rapport détaillé
> des vérifications est
> [`2026-06-22_verifications_variables_robustesse.md`](2026-06-22_verifications_variables_robustesse.md).
>
> **Résultat clé** : aucune mesure alternative ne reproduit *proprement*
> l'IPD sur toute la période, mais le fait saillant n'est pas une discordance
> de mesure — c'est une **hétérogénéité temporelle**. L'effet géopolitique
> négatif sur le commerce est **post-2014** (sous-période 2015–2024 :
> IPD = −0.198, p < 10⁻⁵) ; avant 2015 il est *positif* (≤2014 : +0.043,
> p = 0.018). Le retournement de signe sur l'« échantillon commun » aux
> mesures est un **artefact de fenêtre (≤2014, troncature MID)**, pas de
> composition (SMD des covariables tous < 0.17, cf. `8d`). Le baseline PPML
> (−0.066) agrège donc deux régimes ; c'est ce résultat qu'il faut mettre en
> avant.

Les mesures construites dans `06_build_geopol_measures.R` sont
utilisées **comme alternatives à l'IPD elle-même** (et non comme
instruments), pour tester la robustesse de mesure de l'effet
géopolitique.

Spec exécutée :

```
trade_value ~ <mesure_alt> + rta | exp×year + imp×year + pair
```

en remplaçant `<mesure_alt>` par successivement :
- `polyarchy_dist` (distance démocratique V-Dem)
- `polity_dist` (distance démocratique Polity, annex)
- `ideol_dist` (distance idéologique DPI)
- `allied_atop` (alliance ATOP, indicateur de *proximité*)
- `shared_rival_mid` (rivaux communs)
- `sanction_nontrade` (sanctions non-trade)
- `n_common_sanctioners` (sanctionneurs communs)

**À noter** : `mid_direct` (conflit direct entre i et j) **n'est PAS
une mesure de distance géopolitique** — c'est un *contrôle dyadique*
qui capte l'effet mécanique d'un conflit ouvert sur le commerce
(découplage forcé). À ce titre, il sert d'**inclusion conjointe** dans
les régressions strategic_relations (cf. `06_build_geopol_measures.R`
et `07c`), pas de substitut à l'IPD.

Question : si on remplace l'IPD par chacune de ces mesures, dans la
même Spec 4, observe-t-on des signes/magnitudes cohérents ? Si oui →
robustesse de mesure. Si non → la « distance géopolitique » est un
construit pluriel non agrégeable simplement.

**Vérifications en amont des substitutions** (scripts `08a`/`08b`, détail
dans le rapport dédié) : `08a` décompose l'attrition de chaque mesure
(troncature temporelle vs effet quadratique de la couverture pays) ; `08b`
contrôle l'identifiabilité (ratio within après FE three-way, part de valeur
retenue) et la sélection (`ideol_dist` manque surtout pour les paires
autocratiques → mesure la plus fragile).

**Statut** : ✅ *fait*. Tables dans `Output/Tables/Robustness/`, rapport
dans `2026-06-22_verifications_variables_robustesse.md`.

---

## 7. Inventaire des scripts et livrables

### Scripts (dossier `Codes/`)

| Fichier | Rôle | Statut |
|---|---|---|
| `build_master_panel.R` | Panel directionnel BACI × Gravity × IPD × WDI × DESTA × NATO | ✅ |
| `02_build_strategic_panel.R` | + variables Aiyar 2024 (sectors stratégiques) | ✅ |
| `03a_desc_trade.R` | Descriptives commerce (9 figures + 3 tables) | ✅ |
| `03b_desc_geopolitics.R` | Descriptives IPD + NATO (7 fig + 3 cartes + 3 tables) | ✅ |
| `03c_desc_interaction.R` | Descriptives interaction (14 figures + 4 tables) | ✅ |
| `04_gravity_estimation.R` | **Estimation principale PPML 3-way FE** | ✅ |
| `05_gravity_iv.R` | CF-IV alignement L2 (1er essai IV) | ✅ |
| `05c_gravity_iv_exploration.R` | 9 instruments alternatifs explorés | ✅ |
| `06_build_geopol_measures.R` | Construction mesures alternatives (institutional + strategic + sanctions) | ✅ |
| `07_estimation_iv_alternative.R` | Premier essai IV institutional/strategic (contient artefact LHS log) | ⚠ archivé |
| `07b_first_stage_diagnostics.R` | Diagnostics 1er stage propres | ✅ |
| `07c_estimation_iv_clean.R` | **CF-PPML propre (specs S7–S8bis), partie A uniquement (pas de bootstrap)** | ✅ partie A |
| *(Partie B bootstrap)* | Pas écrite | ❌ non écrit |
| `08a_sample_attrition.R` | Diagnostic attrition par mesure (temps vs couverture pays) | ✅ |
| `08b_identifiability_checks.R` | Within-ratio FE, valeur retenue, sélection `ideol_dist` | ✅ |
| `08c_robustness_measure.R` | **Robustesse de mesure : substitution IPD par chaque mesure (paliers + synthèse temporelle)** | ✅ |
| `8d_common_sample_diagnosis.R` | Diagnostic du retournement de signe sur sample commun (temps vs composition) | ✅ |
| `09_estimations_robustesse_completes.R` | **Toute l'échelle de specs de `04` (Spec 1–10 + Rob1–4) × mesures, paliers A/B/C/D + grille temporelle** | ✅ |

### Outputs

| Dossier | Contenu |
|---|---|
| `Data/Clean/master_panel_with_strategic.parquet` | Panel final pour PPML (1 593 900 obs, 30 cols) |
| `Data/Clean/iv_panel.parquet` | Panel mesures alternatives (1 593 900 obs, ~23 cols) |
| `Output/Figures/{Trade,Geopolitics,Interaction,Estimation,Estimation_IV,Estimation_IV_alternative}/` | ~40 figures PNG |
| `Output/Tables/...` | ~25 tables (.tex + .csv) |
| `Output/Tables/Robustness/` | Tables robustesse de mesure (`08a`–`8d` + `09`) + `_archive/` diagnostics |
| `Output/Maps/{Trade,Geopolitics}/` | 4 cartes choroplèthes |
| `Output/Reports/VARIABLES.md` | Documentation des mesures alternatives |
| `Output/Reports/2026-06-16_recap_analyse.md` | Ce document |
| `Output/Reports/2026-06-22_verifications_variables_robustesse.md` | Vérifications variables robustesse (`08a`–`8d`) |

---

## 8. Décisions méthodologiques clés

1. **PPML retenu sur OLS log-linéaire** : OLS sur log surestime
   l'élasticité distance et exclut les zéros (Santos Silva & Tenreyro
   2006). Réf : Spec 1 vs Spec 4 dans `tab_main_progression.csv`.
2. **FE three-way (exp×year + imp×year + pair)** : absorbe MRT
   d'Anderson-van Wincoop et toute hétérogénéité dyadique structurelle.
   Coût : seuls les régresseurs time-varying dyadiques sont identifiés ;
   les variables gravity classiques (distance, langue, etc.) sont
   absorbées par les pair FE.
3. **Clustering par paire** : standard dans la gravity literature.
   Robustesse en multiway (pair + exp×year + imp×year) testée Spec 5 du
   même tableau : SE plus larges, p-value sur IPD passe de 0.037 à
   0.098.
4. **Densification cartésienne pour les zéros** (Spec 2bis de
   `build_master_panel.R`) : sans zéros, le PPML est biaisé. Le panel
   final contient ~45% de paires-années à `trade_value = 0`.
5. **Sanctions multilatérales : keying par signature de coalition**
   (Section 6bis de `06_build_geopol_measures.R`). Sans cela,
   `n_common_sanctioners` est soit explosé (192 ONU members comptés
   séparément) soit fragmenté (cases différents pour mêmes coalitions).
   Documenté dans `VARIABLES.md`.
6. **Control function PPML (2SRI), pas 2SLS** : pour les modèles
   non-linéaires, le 2SPS (substitution de la valeur prédite) est
   inconsistant. On inclut le résidu du 1er stage comme régresseur
   additionnel (Wooldridge 2014).
7. **IV abandonné comme identification principale** après convergence des
   diagnostics : instruments crédibles non significatifs sur v_hat,
   instruments significatifs non excluables, estimations instables. Cf.
   `iv_synthesis_report.md` et `tab_07c_point_estimates.csv`.
