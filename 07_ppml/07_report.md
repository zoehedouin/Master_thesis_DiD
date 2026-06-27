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
   SMD formel à construire là (hors `07`). Fait saillant de sorting : les
   sanctionneurs sont concentrés en dépendance énergétique **moyenne/haute** (le
   tercile **bas n'a aucun traité** — Sud global non-sanctionneur).
2. **Pré-tendances (inconditionnelles)** — `tab_eventstudy_sunab.csv` : leads
   k=−2,−3,−4 **plats et non significatifs** ; seul le **−5 binné** est positif
   (+0.16, p<0.05) = léger blip de bord. Globalement rassurant.
   **Conditionnelles à l'énergie** — `es_sunab_by_energy.png` / `tab_pretrends_conditional.csv` :
   event study par tercile de dépendance énergétique pré-guerre (T2_mid, T3_high ;
   T1_low sans traité) → vérifie que les leads tiennent *à dépendance comparable*.
3. **HonestDiD (Rambachan & Roth 2023)** — `tab_honestdid_bounds.csv` :
   **M̄ de rupture = 0.5** (relative magnitudes). L'effet survit à des violations
   jusqu'à 0.5× les pré-tendances observées, mais l'IC inclut 0 dès M̄=1 →
   **robustesse modérée** (à mentionner sans surinterpréter). Méthode : sensibilité
   sur la représentation event-study TWFE (≈ Sun-Abraham vu la cohorte 2014 dominante).

## Résultat (lu APRÈS la validité)
- **DiD statique** (`tab_static_did.csv`) : sanction non-commerciale dirigée →
  **−0.58** sur le commerce avec la Russie (p<0.0001 ; −0.55 avec contrôle
  `under_any_sanction`).
- **Contraste par type** : non-commercial **−0.48** (p<0.001) ≫ commercial −0.07
  (n.s.). NB : *inverse* du pattern mondial GSDB-R4 (commercial ≫ non-commercial),
  car les sanctions sur la Russie sont **financières/non-commerciales** avec
  exemptions énergétiques — résultat, pas artefact.
- **Event study Sun & Abraham** : ATT agrégé **−0.60** (p<0.0001) ; effet croissant
  de k=+1 (−0.26) à k=+5 (−0.89).
- **2×2 condamne × sanctionne** (`tab_2x2_did.csv`, réf. = Neither-Russie,
  × post-2022) : **canal expressif réel** —
  - Condamne + sanctionne : **−1.28** (p<10⁻¹¹) ;
  - **Condamne seulement : −0.44** (p=0.028) → le commerce des condamneurs-seuls
    baisse aussi après 2022 *sans* sanction matérielle = l'alignement a un effet
    propre (l'IPD capte du réel) ;
  - Ni l'un ni l'autre (hors-Russie) ≈ 0 (contrôle plat).
  - Écart (a)−(b) ≈ **−0.84** = approximation de la part purement matérielle.
  - Cellule « sanctionne sans condamner » **vide** (post-KAZ Option B).

## À rédiger
Interprétation économique du contraste expressif/matériel (cœur du mémoire),
discussion de la robustesse modérée (M̄=0.5) et du caveat de pré-tendance au bord,
et articulation avec l'intensité (`08_dcdh`) et la décomposition (`09_decomposition`).
