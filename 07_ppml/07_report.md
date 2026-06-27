# Rapport — Partie 07 : PPML (colonne vertébrale gravité, DiD sanctions/votes ONU)

> *Rapport de la **partie 07_ppml** (produit par `07_ppml.R`). Sorties co-localisées
> dans [`figures/`](figures/), [`tables/`](tables/). Synthèse transversale :
> [`../Reports/report_sanctions_synthese.md`](../Reports/report_sanctions_synthese.md) ;
> carte du pipeline : [`../Reports/README_pipeline.md`](../Reports/README_pipeline.md).*
>
> *Design (litt. : Crozet & Hinz 2020 ; GSDB-R4 2025 ; Larch et al. 2024) : gravité
> structurelle sur **panel multi-pays large** (FE exp-temps + imp-temps + paire
> dirigée, cluster paire non-ordonnée, zéros gardés). « Centré Russie » vit dans le
> **traitement** (dirigé, non-commercial : le partenaire sanctionne la Russie),
> PAS dans une coupe d'échantillon (qui saturerait les FE pays-temps — testé).*

## Construction du traitement (validée)
- Panel large : 850 080 obs, 26 565 paires (dont 230 paires-Russie), 2008-2023.
- Traitement = flag **dirigé partenaire→RUS, non-commercial** (GSDB v4, exclusion
  KAZ case 1519 / Option B ; garde-fou : **0 cas Russie-émettrice**).
- **44 partenaires traités / 186 jamais-traités** ; cohortes dominées par **2014
  (39)**, 2008 (1), 2022 (3), 2023 (1) → **2022 = intensification, pas onset**.

## Verdict de validité (jugé AVANT le résultat)
1. **Balance / sorting** : densités brutes + carte 2×2 dans `06_descriptives_did` ;
   SMD formel à construire là. Fait de sorting : les sanctionneurs sont concentrés
   en dépendance énergétique **moyenne/haute** (tercile **bas sans aucun traité** —
   Sud global non-sanctionneur).
2. **Pré-tendances de l'event study** — fenêtre **PROPRE 2010-2021**
   (`sanctions_event_study.png`, `tab_eventstudy_sunab.csv`, window=`2010_2021`) :
   hors crise 2008-09 et hors guerre 2022-23 → **effet 2014 net**. Leads pré-onset
   plats. Version **secondaire 2008-2023** (`sanctions_event_study_full_window.png`)
   conservée pour transparence : le bin +5 y **absorbe 2022-2023** (intensité →
   `08_dcdh`), d'où un ATT plus négatif. **Conditionnelles à l'énergie**
   (`sanctions_by_energy_dependence.png`, `tab_pretrends_conditional.csv`) : par
   tercile (medium, high ; low **sans traité**) sur la fenêtre propre → pré-tendances
   à dépendance comparable.
3. **HonestDiD (Rambachan & Roth 2023)** — `tab_honestdid_bounds.csv`, **reciblé sur
   l'ATT** (moyenne post k≥+1, **pas k=0**) de la fenêtre propre, **grille fine**
   `Mbarvec = seq(0, 1.2, 0.05)`. Contrôle de cohérence **réussi** : IC à M̄=0
   [−0.510 ; −0.345] ≡ IC de l'ATT cible [−0.510 ; −0.345]. **M̄ de rupture exact =
   0.96** (interpolé) → robustesse **bonne** (l'IC robuste ne croise zéro qu'à
   M̄≈1, c.-à-d. tolère des violations presque aussi grandes que les pré-tendances
   observées). *Le 0.5 antérieur était un artefact de grille grossière (pas de 0.5).*
4. **Pré-tendances du résultat phare (2×2 Ukraine 2022)** —
   `condemnation_2x2_pretrends_ukraine.png`, `tab_2x2_pretrends_2022.csv` (cellule ×
   année, réf. 2021 × Neither) : pour `b_condemn_only` (Condemns only), les
   coefficients **2016-2020 sont plats et n.s.** (−0.05 à +0.08, tous p>0.27) →
   **pas de divergence pré-2022** ; l'effet apparaît en **2022 (−0.31) et 2023
   (−0.42)**. Le canal expressif a donc une pré-tendance propre.
   **2×2 Crimée 2014 (test du confondeur)** — `condemnation_2x2_pretrends_crimea.png`,
   `tab_2x2_pretrends_2014.csv` (réf. 2013 × Neither, fenêtre propre 2010-2021,
   `Condemns only` ≈ 56 partenaires) : on regarde si `Condemns only` décroche **déjà
   après 2014**, *avant* que le groupe Neither (Chine/Inde) n'absorbe l'offre russe
   redirigée post-2022. [chiffres ci-dessous, section résultat].
5. **HonestDiD du résultat phare (2×2)** — `honestdid_condemnation_2x2.png`,
   `tab_honestdid_2x2.csv` (effet post moyen 2022-2023, réf. 2021, **2 lags
   seulement**) :
   - `a_both` : post moyen = −1.06 (IC [−1.44 ; −0.68]), **M̄ de rupture = 1.20**
     (plafond de grille) → **robuste** ;
   - **`b_condemn_only` (canal expressif, le phare) : post moyen = −0.36 (IC
     [−0.75 ; +0.03]), M̄ de rupture = 0.00 → FRAGILE.** Dès M̄=0 l'IC robuste
     inclut zéro : sous la loupe event-study (référence 2021 seule, 2 lags), l'effet
     expressif n'est pas individuellement significatif année par année. Le −0.44
     (p=0.028) du 2×2 poolé tient par le **poolage** des deux années post, pas
     au-delà. **À reporter comme suggestif mais fragile**, pas comme établi.

## Résultat (lu APRÈS la validité)
- **DiD statique** (`tab_static_did.csv`) : sanction non-commerciale dirigée →
  **−0.58** (p<0.0001 ; −0.55 avec contrôle `under_any_sanction`).
- **Contraste par type** : non-commercial **−0.48** (p<0.001) vs commercial −0.07
  (n.s.). Les deux ensembles de sanctionneurs **se recouvrent presque entièrement**
  (`tab_type_counts.csv` : 44 partenaires non-commerciaux vs 43 commerciaux, 802 vs
  778 dyades-années) : le contraste s'identifie donc sur la **variation de calendrier
  intra-partenaire** (années sous mesure commerciale vs non-commerciale), pas sur des
  partenaires distincts. Le « commercial n.s. » est à présenter **avec prudence
  (quasi-colinéarité des deux traitements)** — ce n'est ni un manque de puissance par
  petit n, ni une preuve forte que le commercial ne mord pas ; au mieux une indication
  que, à coalition quasi identique, c'est la composante non-commerciale (financière)
  qui porte l'effet, les mesures commerciales étant largement vidées par les
  exemptions énergétiques.
- **Event study (fenêtre propre 2010-2021)** : **ATT 2014 net = −0.40** (p<0.0001).
  Version full 2008-2023 = −0.60 (caveat : conflate l'onset 2014 et
  l'intensification 2022 ; cette dernière relève de `08_dcdh`).
- **2×2 condamne × sanctionne** (`tab_2x2_did.csv`, réf. Neither-Russie × post-2022) :
  - Condamne + sanctionne : **−1.28** (p<10⁻¹¹) — attention : les « both »
    sanctionnent **depuis 2014**, donc ce coefficient mesure l'**escalade 2022
    par-dessus les sanctions déjà en place**, *pas* un onset propre (à ne pas
    sur-vendre) ; son effet post 2022-23 est robuste (M̄=1.20, cf. ci-dessus) ;
  - **Condamne seulement : −0.44** (p=0.028) → signal compatible avec un **canal
    expressif** (le commerce des condamneurs-seuls baisse après 2022 *sans* sanction
    matérielle). **Caveat d'interprétation majeur** : ce −0.44 reflète surtout que
    les condamneurs-seuls **n'ont PAS participé à la réorientation commerciale vers
    la Russie** qu'ont opérée les pays *Neither* (Chine, Inde) après 2022 — visible
    dans la vue descriptive à trois groupes
    [`../06_descriptives_did/figures/did_fig01_trade_index_by_status.png`](../06_descriptives_did/figures/did_fig01_trade_index_by_status.png)
    (panel b : *Neither* explose, *Condemns only* reste ~plat ; les IC, eux, sont
    dans les event studies de `07`). C'est **compatible avec le canal expressif mais
    aussi avec une capacité/volonté d'absorption différentielle** — à trancher par
    le 2×2 Crimée (ci-dessous), la balance (`06`) et la décomposition (`09`). **Ne
    pas sur-vendre** ; rappel : ce −0.44 est **fragile** sous HonestDiD (M̄=0.00) ;
  - Ni l'un ni l'autre (hors-Russie) ≈ 0 ; écart (a)−(b) ≈ **−0.84** = part matérielle ;
  - Cellule « sanctionne sans condamner » **vide** (post-KAZ Option B).
- **Robustesse `align`** (`tab_2x2_did_align.csv`, réf. = **abstention ≈ Neither**) :
  signes désormais **comparables** au 2×2 binaire — condamneurs (`yes`) × post2022 =
  **−1.21** (p<10⁻¹²) = baisse, **même histoire** que le 2×2. (Le `+1.47` du run
  précédent était une référence inversée, pas une contradiction : effectifs
  yes=139, abstain=35, no/absent=16.) Les abstentionnistes (Chine/Inde) ressortent
  comme bénéficiaires relatifs (réorientation du commerce russe — caveat tiers).
- **2×2 Crimée 2014 (test du confondeur)** (`condemnation_2x2_pretrends_crimea.png`,
  `tab_2x2_pretrends_2014.csv`, réf. 2013 × Neither) : c'est l'épisode **propre** car
  en 2014 le groupe *Neither* n'avait **pas encore** son boom d'absorption post-2022.
  Lecture : `Condemns only` a des leads **plats avant 2013** (2011 −0.00, 2012 −0.04 ;
  2010 +0.14 n.s.), puis dérive **légèrement négative après 2014** (2015 −0.04, 2016
  −0.19 [p=0.08], 2017 −0.13, 2018-2021 ≈ −0.07 à −0.09), **mais individuellement
  non significative**. → Un **soupçon de canal expressif dès 2014, indépendant de la
  réorientation post-2022** (ce qui *renforce* l'interprétation expressive du −0.44),
  **mais faible et fragile**. **Caveat** : les sanctions de 2014 (financières,
  sectorielles) sont **sans commune mesure** avec celles de 2022 — l'amplitude n'est
  pas comparable.

## À rédiger
Interprétation économique du contraste expressif/matériel (cœur du mémoire), de
l'effet 2014 net (−0.40, **robuste** : M̄=0.96) vs l'escalade 2022 (renvoyée à
`08_dcdh`), du canal financier (commercial vidé par l'énergie, quasi-colinéaire), et
de la **fragilité** du canal expressif pur (`condemn_only`, M̄=0.00) — à présenter
comme suggestif, le matériel (`a_both`, M̄=1.20) étant le résultat solide.
