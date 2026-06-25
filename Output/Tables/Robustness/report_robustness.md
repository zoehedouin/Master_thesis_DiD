# Rapport de robustesse de mesure
Date : 2026-06-17 11:56:50

Substitution de l'IPD par chaque mesure alternative, spec EXACTE de
`04_gravity_estimation.R` Spec 4 (workhorse) :
`fepois(trade_value ~ <var> + rta | exp_year + imp_year + pair, vcov = ~pair)`.

## 1. Sample propre par variable (`tab_own_sample.csv`)

Chaque variable estimee sur son propre sample maximal :

| Variable | Fenetre | N_sample | coef | SE | p |
|---|---|---|---|---|---|
| ipd | 1995-2024 | 1,031,492 | -0.0663 | 0.0318 | 0.0372 |
| polyarchy_dist | 1995-2024 | 838,286 | -0.0408 | 0.0617 | 0.509 |
| polity_dist | 1995-2018 | 563,942 | +0.0052 | 0.0020 | 0.0108 |
| allied_atop | 1995-2018 | 817,916 | +0.0228 | 0.0248 | 0.359 |
| shared_rival_mid | 1995-2014 | 674,276 | +0.0222 | 0.0083 | 0.0075 |
| sanction_nontrade | 1995-2023 | 996,710 | -0.0805 | 0.0198 | 4.89e-05 |
| n_common_sanctioners | 1995-2023 | 996,710 | +0.0733 | 0.0174 | 2.42e-05 |

## 2. Paliers sur sample commun

Pour chaque palier : IPD + mesures du palier estimees sur le sample commun (
intersection des non-NA sur les mesures du palier).

### Palier A — 6 measures (binding MID)
- Fenetre realisee : 1995-2014
- N_sample : 476,778
- Fichier : `tab_palier_A.csv`

### Palier B — 5 measures sans shared_rival (binding ATOP)
- Fenetre realisee : 1995-2018
- N_sample : 563,942
- Fichier : `tab_palier_B.csv`

### Palier C — 3 measures core (binding DPI/GSDB)
- Fenetre realisee : 1995-2023
- N_sample : 810,230
- Fichier : `tab_palier_C.csv`

## 3. Synthese temporelle (`tab_robustness_synthesis.csv`)

### Partie 2 — Composition palier C fixe, fenetre varie

| Coupure | Fenetre obs | N_estim | IPD coef | SE | p |
|---|---|---|---|---|---|
| <=2014 | 1995-2014 | 512570 | +0.0416 | 0.0180 | 0.0212 |
| <=2018 | 1995-2018 | 631374 | +0.0419 | 0.0153 | 0.00613 |
| <=2023 | 1995-2023 | 778448 | -0.0231 | 0.0218 | 0.29 |

### Partie 3 — Full sample, fenetres

| Fenetre | Fenetre obs | N_estim | IPD coef | SE | p |
|---|---|---|---|---|---|
| <=2014 | 1995-2014 | 607569 | +0.0426 | 0.0180 | 0.0178 |
| <=2018 | 1995-2018 | 752739 | +0.0428 | 0.0152 | 0.00496 |
| <=2023 | 1995-2023 | 932281 | -0.0225 | 0.0218 | 0.3 |
| full | 1995-2024 | 965872 | -0.0663 | 0.0318 | 0.0372 |
| 1995-2014 | 1995-2014 | 607569 | +0.0426 | 0.0180 | 0.0178 |
| 2015-2024 | 2015-2024 | 322238 | -0.1983 | 0.0425 | 3.1e-06 |

## Lecture

L'effet IPD negatif sur le commerce est attribuable a la sous-periode
**post-2014**. Sur les paliers A et B (bornes a ≤2014 et ≤2018),
l'IPD est *positif* et significatif. Sur le palier C (≤2023), il devient
non-significatif. Sur le full sample, il est negatif (-0.066, p<0.05). La
**Partie 2** (composition C fixe, fenetre temporelle variable) confirme
que le basculement vient du temps et non de la composition de l'echantillon.

## Fichiers conserves a la racine

- `report_robustness.md` (ce fichier)
- `tab_own_sample.csv`
- `tab_palier_A.csv`, `tab_palier_B.csv`, `tab_palier_C.csv`
- `tab_robustness_synthesis.csv`

Les diagnostics auxiliaires (08a sample_attrition, 08b identifiability /
ideol_selection, 8d coverage / covariate_balance / anchor, et les versions
anterieures de 08c) sont archives dans `_archive/`.
