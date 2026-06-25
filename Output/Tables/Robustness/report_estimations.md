# Estimations completes de robustesse — toute l'echelle de specs de `04`
Date : 2026-06-22 23:24:03

Substitution de l'IPD par chaque mesure retenue (08a->08d), sur l'INTEGRALITE
de l'echelle de specs de `04_gravity_estimation.R` (Spec 1..10 + Rob1..4),
dans deux regimes d'echantillon (paliers communs A/B/C ; periode propre D) +
une grille temporelle pour l'IPD.

Mesures retenues : `polyarchy_dist`, `polity_dist`, `shared_rival_mid`,
`sanction_nontrade`, `n_common_sanctioners`. Exclues (annexe) : `ideol_dist`
(selection sur le regime, 08b-C), `allied_atop` (non identifiee, within 0.086).

Sens attendu : negatif pour les distances/hostilites ; **ambigu** pour
`n_common_sanctioners` (statut pariah conjoint) -> rapporte sans attendu impose.

## 1. Lecture APPARIEE — chaque mesure vs l'IPD du MEME echantillon (Spec 4)

Le point central : sur sa periode propre (palier D), chaque mesure doit etre
comparee a l'IPD estime sur *ce meme* echantillon, jamais au -0.066 full
sample. Les mesures dont la couverture s'arrete avant 2015 (polity ≤2018,
shared_rival ≤2014) tombent dans l'ere ou l'IPD lui-meme est *positif*.

| Mesure | Periode | N | coef mesure | coef IPD apparie | Meme signe ? |
|---|---|---|---|---|---|
| polyarchy_dist | 1995-2024 | 805,994 | −0.0408 (p=0.509) | −0.0669 (p=0.036) | oui |
| polity_dist | 1995-2018 | 534,884 | +0.0052 (p=0.011) | +0.0410 (p=0.009) | oui |
| shared_rival_mid | 1995-2014 | 607,569 | +0.0222 (p=0.007) | +0.0426 (p=0.018) | oui |
| sanction_nontrade | 1995-2023 | 932,281 | −0.0805 (p=0.000) | −0.0225 (p=0.300) | oui |
| n_common_sanctioners | 1995-2023 | 932,281 | +0.0733 (p=0.000) | −0.0225 (p=0.300) | non |

## 2. Stabilite VERTICALE — signe a travers les specs a coef unique

Pour chaque mesure (palier D), compte des specs a coefficient unique ou le
signe est negatif, et plage des coefficients. Specs : 
`spec1`, `spec2`, `spec3`, `spec4`, `spec5`, `spec6`, `spec7`, `rob1`, `rob2`, `rob3`, `rob4`.

| Mesure | Attendu | n specs | n coef<0 | min coef | max coef |
|---|---|---|---|---|---|
| polyarchy_dist | neg | 11 | 9 | -0.4444 | 0.1483 |
| polity_dist | neg | 11 | 1 | -0.0284 | 0.0112 |
| shared_rival_mid | neg | 11 | 4 | -0.0950 | 0.0621 |
| sanction_nontrade | neg | 11 | 8 | -0.5731 | 0.1384 |
| n_common_sanctioners | ambigu | 11 | 1 | -0.0878 | 0.1460 |

## 3. Bascule temporelle de l'IPD — survit-elle a toute l'echelle ?

Coefficient IPD par fenetre, full sample, pour chaque spec a coef unique.
Resultat central : positif <=2014/2018 puis negatif sur 2015-2024. Il tient
sur toute la **famille FE three-way** (spec4/5/6/7, rob2). Les specs SANS
`pair` FE (spec2/3) restent positives (effet de niveau between-pair) ; spec1
(OLS) et rob1 (quadratique) sont d'une autre nature.

| Spec | IPD 1995-2014 | IPD 2015-2024 | IPD 1995-2024 |
|---|---|---|---|
| spec1 | −0.0486 (p=0.00) | −0.0358 (p=0.04) | −0.0302 (p=0.04) |
| spec2 | +0.1037 (p=0.00) | −0.0216 (p=0.50) | +0.0352 (p=0.24) |
| spec3 | +0.1175 (p=0.00) | −0.0190 (p=0.59) | +0.0513 (p=0.11) |
| spec4 | +0.0426 (p=0.02) | −0.1983 (p=0.00) | −0.0663 (p=0.04) |
| spec5 | +0.0426 (p=0.04) | −0.1983 (p=0.00) | −0.0663 (p=0.10) |
| spec6 | +0.1366 (p=0.00) | −0.1745 (p=0.00) | −0.0126 (p=0.78) |
| spec7 | +0.0366 (p=0.04) | −0.2035 (p=0.00) | −0.0728 (p=0.02) |
| rob1 | −0.0403 (p=0.11) | −0.1376 (p=0.00) | −0.1379 (p=0.00) |
| rob2 | +0.0423 (p=0.02) | −0.1996 (p=0.00) | −0.0677 (p=0.04) |
| rob3 | −0.0194 (p=0.31) | −0.1983 (p=0.00) | −0.1229 (p=0.00) |
| rob4 | +0.0542 (p=0.01) | −0.0392 (p=0.04) | +0.0283 (p=0.11) |

### 3bis. Decomposition periodique (Spec 10, `i(period, ipd)`, full sample)

| Periode | IPD coef | SE | p |
|---|---|---|---|
| 1995-2007 | +0.0017 | 0.0255 | 0.948 |
| 2008-2013 | −0.0405 | 0.0240 | 0.091 |
| 2014-2017 | −0.0943 | 0.0243 | 0.000 |
| 2018-2021 | −0.1440 | 0.0268 | 0.000 |
| 2022-2024 | −0.2124 | 0.0368 | 0.000 |

Intensification **monotone** de l'effet negatif : l'alignement diplomatique
pese de plus en plus lourd sur le commerce a mesure qu'on avance vers
2022-2024 (guerres commerciales, Russie-Ukraine, decouplage US-Chine).

## 4. Lecture d'ensemble

- **Section 1 (appariee)** : toute mesure de distance/hostilite partage le
  signe de l'IPD *sur le meme echantillon*. Les signes positifs de polity et
  shared_rival ne contredisent pas l'IPD — ils refletent une couverture
  bornee a l'ere pre-2015 (ou l'IPD apparie est lui aussi positif).
- **Section 2 (verticale)** : la dispersion du signe entre specs vient
  surtout du contraste FE-pair (within) vs sans-pair (between).
- **Section 3 (horizontale)** : la bascule post-2014 est STRUCTURELLE — elle
  survit a toute la famille FE three-way et a la decomposition periodique.

## Garde-fous appliques
- Mesure comparee a l'IPD apparie de la MEME ligne (sample+spec).
- Gros N : magnitude et stabilite de signe, pas p<0.05.
- `n_common_sanctioners` : signe ambigu, sans attendu impose.
- Paliers A/B/C : RHS identique entre geovar (pas de `mid_direct`).
- `mid_direct` ajoute uniquement pour `shared_rival_mid` en periode propre (D).
- Specs a `pair` FE : gravite invariante absorbee ; conservee en Spec 1/2/3.

## Fichiers
- `tab_grille_mesures.csv` — long : palier(A/B/C/D)/mesure/spec/geovar/term/coef/se/p/n/collin
- `tab_grille_temporelle.csv` — long : base/fenetre/spec/geovar/term/coef/se/p/n

Interactions NATO (spec8/9) et periodes (spec10) presentes dans les CSV
(format long, plusieurs coefficients par fit).
