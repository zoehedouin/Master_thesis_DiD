# Rapport — Partie 06 (DiD) : descriptives Russie-centrées

> *Rapport de la **partie 06_descriptives_did** (produit par `06_descriptives_did.R`).
> Bloc Russie-centré du §1, séparé du socle général `../06_descriptives/`. Sorties
> co-localisées dans [`figures/`](figures/), [`tables/`](tables/), [`maps/`](maps/),
> toutes préfixées `did_`. Synthèse transversale :
> [`../Reports/report_sanctions_synthese.md`](../Reports/report_sanctions_synthese.md) ;
> carte du pipeline : [`../Reports/README_pipeline.md`](../Reports/README_pipeline.md).*
>
> *Frontière : on **montre** (moyennes/densités/indices BRUTS) et on construit
> **ici** la **balance standardisée formelle** (SMD, seuil |SMD|>0.1, cf. §Balance
> ci-dessous) ; pré-tendances + HonestDiD vivent en `07_ppml`. Russie = cible, on
> raisonne en `partner_iso3`.*

## Sorties descriptives (§1)

Sorties produites (toutes descriptives, aucune estimation) :

**Figures**
- `did_fig01_trade_index_by_status` — commerce avec la Russie en indice base 100
  (2013), par statut de sanction (a) et par cellule 2×2 (b) ; préfigure le DiD.
- `did_fig02_treatment_calendar` — onsets de sanction (pic 2014) + intensité
  (`sanc_n_active_core`) : 2022 = intensification, pas onset.
- `did_fig03_covariate_distributions_by_group` — densités brutes (énergie,
  exposition pré-2014, polyarchy, log PIB/tête) par sanctionneur/condamneur.
- `did_fig04_sorting_energy_exposure` — nuage exposition × énergie, coloré par cellule.
- `did_fig05_strategic_share_by_cell` — part stratégique par cellule (prépare §5).

**Carte**
- `did_map01_cell_2022_world` — géographie de l'alignement (cellules 2×2).

**Tables** (`.csv` + `.tex`)
- `did_tab01_panel_coverage` — cadrage du panel Russie-centré.
- `did_tab02_crosstab_2x2` — 2×2 : nb de partenaires + poids commercial par cellule (2022 & 2014).
- `did_tab03_emblematic_by_cell` — partenaires emblématiques (top 5 par cellule).
- `did_tab04_un_vote_counts` — décompte des votes (garde-fou totaux officiels).
- `did_tab05_vote_transition_2014_2022` — matrice de transition des votes.
- `did_tab06_trade_by_status_periods` — commerce moyen par statut × sous-période.

## Balance / SMD calibrée sur la capacité d'absorption

> *Construite ici (un partenaire/ligne, baseline pré-guerre 2018-2021). Sorties :
> `did_balance_smd.csv`/`.tex`, `did_balance_love_plot.png`,
> `did_balance_smd_sanctioner.csv`. SMD = (différence de moyennes)/écart-type poolé ;
> |SMD|>0.1 préoccupant, >0.25 fort déséquilibre.*

**Pourquoi cette balance.** Le test Crimée 2014 et `did_fig01` ont montré que
« Condemns only × post-2022 » (−0.44, cf. `07_ppml`) reflète surtout que les
condamneurs-seuls **n'ont PAS rejoint la réorientation commerciale vers la Russie**
opérée par le groupe *Neither* (Chine/Inde) — pas forcément de l'alignement expressif.
On teste donc frontalement si **`b_condemn_only` diffère de `d_neither`** sur les
déterminants de la **capacité d'absorption** (Bloc A : taille, proximité, demande
pour les biens russes réorientés), indépendamment du vote ONU.

**Verdict (contraste phare `Condemns only` vs `Neither`).** **Déséquilibre massif
sur le Bloc A : 11 des 12 covariables dépassent |SMD|>0.25.** Les *Neither* (Chine,
Inde) sont bien plus **peuplés** (SMD population **−0.90**), plus gros en PIB (log GDP
−0.48) mais plus pauvres par tête (GDP/tête +0.76), **plus proches** de la Russie
(log distance +0.49), **plus dépendants** de l'énergie russe (−0.45) et **plus
exposés** avant 2014 (−0.49) ; la composition régionale diffère fortement (Afrique
−0.45, Amériques +0.50, Asie −0.26). Seule « Region: Europe » reste sous 0.25 (≈0.13).
→ **Les deux cellules ne sont PAS comparables sur la capacité d'absorption.** Le −0.44
est donc **confondu par l'absorption** : l'interprétation « artefact d'absorption »
(les condamneurs-seuls, petits/lointains/peu dépendants, ne pouvaient pas absorber
l'offre russe redirigée comme la Chine/Inde) est **verrouillée par les données** ;
le canal expressif **pur** ne peut PAS être lu directement sur ce coefficient (cohérent
avec sa fragilité HonestDiD, M̄=0.00 en `07_ppml`). Le contraste matériel `a_both`
vs `Neither` est lui aussi déséquilibré (sanctionneurs = UE/OTAN/riches), ce qui
relève du sorting classique, pas du confondeur d'absorption.

**Bloc B (UE, OTAN, idéal-point, polyarchy) — DESCRIPTIF seulement.** Très déséquilibré
aussi (a_both = 65% UE / 67% OTAN / polyarchy 0.78), mais ces variables sont
**quasi-colinéaires au traitement** (« bad controls ») : on les **montre pour
documenter le sorting**, JAMAIS comme variables de conditionnement causal.

**Sorting sanctionneur vs non** (`did_balance_smd_sanctioner.csv`) : sorting classique
confirmé — les sanctionneurs sont plus gros (SMD log GDP +1.22), plus riches (+1.25),
**plus proches** (−1.65), plus dépendants de l'énergie russe (+0.58), UE (+1.85),
OTAN (+1.91), démocratiques (+1.90). C'est ce sorting qui **justifie le panel large
+ FE de résistance multilatérale** (`07_ppml`) plutôt qu'une coupe Russie-restreinte.

**Limite (honnêteté).** Le cœur théorique de la capacité d'absorption —
l'**infrastructure de paiement** (CIPS, SPFS, lignes de swap en devises locales) —
**n'est PAS mesurable** dans le panel. Taille + proximité + demande énergétique en
sont les **proxies mesurables** ; aucune variable de paiement n'est fabriquée.

*Reste à trancher en aval : la décomposition stratégique/non-stratégique (`09`) et
l'intensité (`08`) pour démêler absorption vs expressif sur le total.*
