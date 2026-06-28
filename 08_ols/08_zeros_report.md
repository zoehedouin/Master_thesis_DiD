# Rapport — Diagnostic des zéros (`08_zeros_diagnostic.R`)

> *Diagnostic de **données** sur le panel pkey EXACT du §4.1 (08_ols), avant
> d'implémenter le dist_lag_het (§4.2) et de figer la transformation de l'outcome.
> Aucune estimation, aucun chiffre fabriqué : on lit et on décrit. Sortie chiffrée :
> [`tables/tab_zeros_diagnostic.csv`](tables/tab_zeros_diagnostic.csv) (97 lignes longues
> étiquetées). Échantillon COMPLET (pas de sous-échantillonnage des contrôles).*

## Panel (verbatim §4.1)
`sanctions_panel.parquet` ; pkey non ordonnée ; fenêtre **2008-2023** ; `trade_tot =
sum(trade_value)` (deux directions) ; `n_active_core = max(...)` ; paliers 0/1/2-5/6+.
→ **425 040** obs pkey-année, **26 565** paires (**5 509** ever-traitées, **21 056**
jamais-traitées).

## 1+2. Unités — situer le « +1 »
`trade_value` est en **milliers USD** (BACI, à confirmer). Flux strictement positifs :
min **0.001**, p1 **0.055**, p5 **0.783**, **médiane 3 194.4**. Donc le « +1 » de
`log(trade+1)` vaut **0.03 % de la médiane** (négligeable pour le flux typique) mais
**128 % du p5** : il **double** les plus petits flux (sous le millier d'USD). Le « +1 »
ne distord que la **sous-queue** des micro-flux, pas la masse.

## 3. Part de zéros
- **Global : 31.56 %** (134 138 / 425 040). **Stable par année (~30-33 %)** : aucun
  saut global en 2014 ni 2022 → le commerce mondial avec zéros est structurel, pas un
  effet de la vague de sanctions.
- **Par palier** : tier 0 = 33.1 %, tier 1 = **17.0 %**, tier 2 = 25.2 %, tier 3 (6+) =
  **1.5 %**. Les paires traitées commercent davantage (moins de zéros).
- **Ever vs never traitées** : **19.1 %** vs **34.8 %**.
- **Sous-panel RUSSIE (230 paires) : 9.73 % global**, mais **saut net** : ~5-10 %
  jusqu'en 2021 (2019 = 3.9 %), puis **2022 = 30.0 %** et **2023 = 33.0 %**. C'est le
  **signal de marge extensive** : sous l'intensification 2022, une part substantielle
  des partenaires bascule à zéro déclaré avec la Russie.

## 4. Structurel vs intermittent (au sein des paires)
- **always_positive 13 200 (49.7 %)**, **always_zero 3 439 (12.9 %)**, **intermittent
  9 926 (37.4 %)**.
- Ever-traitées : 3 416 always_pos / **292 always_zero** / 1 801 intermittentes.
  Contrôles : 9 784 / 3 147 / 8 125.
- **Russie** : 150 always_pos / **3 always_zero** / 77 intermittentes. Les zéros
  russes sont donc **quasi tous intermittents** (entrées/sorties), pas structurels —
  cohérent avec une bascule liée au choc plutôt qu'avec une absence permanente de lien.

## 5. Zéros induits par le traitement (le point clé)
Sur **4 947** paires ever-traitées évaluables (pré>0, post observé), **375 (7.58 %)**
passent d'un commerce **positif en pré (moyenne 2018-2021) à un zéro réel en post
(2022-2023)**. **12** sont des paires Russie :

| pkey | moy. pré 2018-21 (k USD) | palier post |
|---|---:|---:|
| BLR_RUS | 33 506 383 | 1 |
| RUS_SDN | 744 686 | 2 |
| RUS_YEM | 332 562 | 2 |
| LBY_RUS | 200 669 | 1 |
| IRQ_RUS | 178 202 | 1 |
| AFG_RUS | 153 055 | 1 |
| PRK_RUS | 30 930 | 2 |
| HTI_RUS | 12 093 | 1 |
| RUS_SOM | 7 056 | 2 |
| ERI_RUS | 5 504 | 0 |
| RUS_SSD | 3 574 | 2 |
| RUS_SLE | 1 915 | 0 |

**⚠️ Caveat d'attrition de déclaration (surprise signalée).** La plus grosse « induite »,
**BLR_RUS (~33,5 Md USD → 0)**, est **invraisemblable comme vrai effondrement** : le
commerce Biélorussie-Russie a **augmenté** après 2022. C'est presque sûrement une
**lacune de déclaration BACI** (la Biélorussie, elle-même sanctionnée, cesse de
déclarer), pas un effet causal. Le reste de la liste est dominé par des **États en
conflit / fragiles** (SDN, YEM, LBY, AFG, PRK, SOM, SSD, SLE, ERI), où zéro déclaré
mêle effondrement réel et trou de reporting. **Conclusion : les « zéros induits »
existent et sont concentrés sur la Russie post-2022, mais ils ne se séparent pas
proprement de l'attrition de déclaration** — à traiter en robustesse, pas à prendre
au pied de la lettre.

## 6. Sensibilité de la transformation (sans estimer)
- `log(trade+1)` et **IHS = asinh(trade)** sont **quasi identiques** : `cor = 0.99943`.
  IHS conserve les **134 138 zéros** que `log(positifs)` **perd**.
- `cor(log(trade+1), log(positifs))` sur les positifs = `0.99444`.
- **Queue gauche** (positifs ≤ p10 = 4.584, n = 29 092) : moyennes `log1p = 0.677`,
  `IHS = 0.866`, `log(pos) = −0.592` ; écart moyen `|log1p − IHS| = 0.189`. Sur les
  gros flux, `IHS − log1p ≈ 0.693 = log 2` (décalage quasi constant). Les trois ne
  divergent vraiment que **dans la sous-queue** et, surtout, **sur le traitement des
  zéros** (log1p et IHS = 0 ; log(pos) = indéfini → supprimé).

## Recommandation
**Garder `log(trade+1)`** (statu quo §4.1), pour trois raisons tirées des coupes 1, 4, 5 :

1. **Coupe 5 (induits) + 3 (Russie 5 %→30 %)** : 31.6 % de zéros au global et une
   **bascule extensive Russie post-2022** qui est *précisément le signal d'intérêt*.
   **Positifs-seuls supprimerait 134 138 obs (un tiers du panel) et, par construction,
   les 375 paires (dont 12 Russie) qui s'éteignent sous traitement** → **biais de
   sélection** effaçant la marge extensive. À écarter (ou alors avec une note de
   sélection explicite ; ce n'est pas le choix recommandé ici).
2. **Coupe 6 (transformation)** : `log(trade+1)` et IHS sont corrélés à **0.9994** →
   basculer en IHS **ne changerait rien de matériel** aux effets dCDH (décalage ≈ log 2
   dans la masse, divergence seulement dans la sous-queue). Pas de raison de quitter
   `log(trade+1)`, qui est la spec du §4.1, interprétable, et alignée sur la littérature
   (GSDB-R4).
3. **Coupe 1/2 (unités)** : le « +1 » est **négligeable vs la médiane (0.03 %)** ; il ne
   pèse que sur la sous-queue (p5), marginale pour des effets pilotés par la masse des
   paires. Pas un motif de changer de transformation.

**Réserve à porter dans le §4.2 (pas un changement de transformation)** : les zéros
induits Russie mêlent **effondrement réel et attrition de déclaration** (BLR_RUS
emblématique). → Prévoir une **robustesse** : (i) signaler/écarter les paires à lacune
de reporting connue (Biélorussie, États sanctionnés ayant cessé de déclarer), et/ou
(ii) lire la marge extensive **avec ce caveat**. Ne **rien fabriquer** : aucune
imputation de flux manquant.
