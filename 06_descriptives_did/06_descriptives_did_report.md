# Rapport — Partie 06 (DiD) : descriptives Russie-centrées

> *Rapport de la **partie 06_descriptives_did** (produit par `06_descriptives_did.R`).
> Bloc Russie-centré du §1, séparé du socle général `../06_descriptives/`. Sorties
> co-localisées dans [`figures/`](figures/), [`tables/`](tables/), [`maps/`](maps/),
> toutes préfixées `did_`. Synthèse transversale :
> [`../Reports/report_sanctions_synthese.md`](../Reports/report_sanctions_synthese.md) ;
> carte du pipeline : [`../Reports/README_pipeline.md`](../Reports/README_pipeline.md).*
>
> *Frontière : ici on **montre** (moyennes/densités/indices BRUTS), on ne **juge**
> pas — l'écart standardisé (SMD), le seuil |SMD|>0.1 et le verdict de balance
> sont en `07_validity`. Russie = cible, on raisonne en `partner_iso3`.*

## À rédiger (feuille de route §1)

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

*Lecture à rédiger : la cellule « condamne sans sanctionner » (b) est-elle
exploitable (poids commercial) ? le sorting est-il sévère (fig03/fig04) ?*
