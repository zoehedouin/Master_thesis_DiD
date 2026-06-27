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
2. **Pré-tendances de l'event study** — fenêtre **PROPRE 2010-2021** (`es_sunab_russia.png`,
   `tab_eventstudy_sunab.csv`, window=`2010_2021`) : hors crise 2008-09 et hors
   guerre 2022-23 → **effet 2014 net**. Leads pré-onset plats. Version **secondaire
   2008-2023** (`es_sunab_russia_fullwindow.png`) conservée pour transparence : le
   bin +5 y **absorbe 2022-2023** (intensité → `08_dcdh`), d'où un ATT plus négatif.
   **Conditionnelles à l'énergie** (`es_sunab_by_energy.png`, `tab_pretrends_conditional.csv`) :
   par tercile (T2_mid, T3_high ; T1_low **sans traité**) sur la fenêtre propre →
   pré-tendances à dépendance comparable.
3. **HonestDiD (Rambachan & Roth 2023)** — `tab_honestdid_bounds.csv`, **reciblé sur
   l'ATT** (moyenne post k≥+1, **pas k=0**) de la fenêtre propre. Contrôle de
   cohérence **réussi** : IC à M̄=0 [−0.510 ; −0.345] ≡ IC de l'ATT cible
   [−0.510 ; −0.345]. **M̄ de rupture = 0.5** → robustesse **modérée** (l'effet
   survit à des violations jusqu'à 0.5× les pré-tendances, IC inclut 0 dès M̄=1).
4. **Pré-tendances du résultat phare (2×2)** — `es_2x2_pretrends.png`,
   `tab_2x2_pretrends.csv` (cellule × année, réf. 2021 × Neither) : pour
   `b_condemn_only`, les coefficients **2016-2020 sont plats et n.s.** (−0.05 à
   +0.08, tous p>0.27) → **pas de divergence pré-2022** ; l'effet apparaît en
   **2022 (−0.31) et 2023 (−0.42)**. Le canal expressif a donc une pré-tendance propre.

## Résultat (lu APRÈS la validité)
- **DiD statique** (`tab_static_did.csv`) : sanction non-commerciale dirigée →
  **−0.58** (p<0.0001 ; −0.55 avec contrôle `under_any_sanction`).
- **Contraste par type** : non-commercial **−0.48** (p<0.001) vs commercial −0.07
  (n.s.). **Effectifs quasi identiques** (`tab_type_counts.csv` : 44 vs 43
  partenaires, 802 vs 778 dyades-années) → le « commercial n.s. » **n'est PAS un
  manque de puissance** mais reflète le **contenu** des sanctions sur la Russie
  (mesures commerciales largement vidées par les exemptions énergétiques ;
  l'hostilité mord par le canal financier/non-commercial).
- **Event study (fenêtre propre 2010-2021)** : **ATT 2014 net = −0.40** (p<0.0001).
  Version full 2008-2023 = −0.60 (caveat : conflate l'onset 2014 et
  l'intensification 2022 ; cette dernière relève de `08_dcdh`).
- **2×2 condamne × sanctionne** (`tab_2x2_did.csv`, réf. Neither-Russie × post-2022) :
  - Condamne + sanctionne : **−1.28** (p<10⁻¹¹) ;
  - **Condamne seulement : −0.44** (p=0.028) → **canal expressif réel** (le commerce
    des condamneurs-seuls baisse aussi après 2022 *sans* sanction matérielle) ;
  - Ni l'un ni l'autre (hors-Russie) ≈ 0 ; écart (a)−(b) ≈ **−0.84** = part matérielle ;
  - Cellule « sanctionne sans condamner » **vide** (post-KAZ Option B).
- **Robustesse `align`** (`tab_2x2_did_align.csv`, réf. = **abstention ≈ Neither**) :
  signes désormais **comparables** au 2×2 binaire — condamneurs (`yes`) × post2022 =
  **−1.21** (p<10⁻¹²) = baisse, **même histoire** que le 2×2. (Le `+1.47` du run
  précédent était une référence inversée, pas une contradiction : effectifs
  yes=139, abstain=35, no/absent=16.) Les abstentionnistes (Chine/Inde) ressortent
  comme bénéficiaires relatifs (réorientation du commerce russe — caveat tiers).

## À rédiger
Interprétation économique du contraste expressif/matériel (cœur du mémoire), de
l'effet 2014 net (−0.40) vs l'escalade 2022 (renvoyée à `08_dcdh`), de la robustesse
modérée (M̄=0.5), et du canal financier (commercial vidé par l'énergie).
