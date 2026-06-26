# Rapport — Dose d'intensité des sanctions + Phase 3 (dCDH)

> *Réorg `reorg-did` : `06_build_geopol_measures.R` → `Codes/03_build_treatments.R` ;
> `11_intensity_dcdh.R` + `11b_dcdh_outputs.R` → `Codes/09_dcdh.R`. Carte :
> `README_pipeline.md`.*

*Date : 2026-06-24. Scripts : `06_build_geopol_measures.R` (enrichi),
`11_intensity_dcdh.R`, `11b_dcdh_outputs.R`. Données : `iv_panel.parquet`
(régénéré), GSDB v4 brut. Chiffres exacts, sans interprétation.*

---

## Étape 0 — « nombre de sanctions actives » constructible ? → **Cas (a), OUI**

Le fichier dyadique `GSDB_V4_dyadic.dta` contient `case_id`, une **chaîne
comma-séparée de cas atomiques** (ex. `"595,471,574"`). En l'éclatant et en
comptant les cas distincts par dyade-année, on obtient un **compte de sanctions
actives** exploitable (1144 cas atomiques distincts sur 1995-2023). C'est déjà
le mécanisme utilisé par `06` pour `n_common_sanctioners`.

Preuve que le compte capte l'escalade (Russie comme cible, tous senders) :

| année | n_cases actifs | n_senders | binaire/type |
|---|---|---|---|
| 2014-2016 | 8 | 40 | tous types = 1 (saturé) |
| 2019-2021 | 10 → 13 | 40 | plat |
| **2022** | **38** | 46 | plat |
| **2023** | **46** | 48 | plat |

→ le compte ×5 de 2014 à 2022 là où binaire/type/onset saturent. Cas (a) confirmé.
Le fichier **case-level** `GSDB_V4.csv` (1547 cas, types par cas) permet en plus
la version « core » (cas commercialement pertinents).

---

## Étape 1 — Doses et types ajoutés dans `06` (additif)

### Colonnes ajoutées (10 ; `iv_panel` passe de 23 à 33 colonnes)
- **Par type** (indicatrices dyade-année, convention symétrique/undirected
  identique à l'existant, fenêtre NA-après-2023) : `sanc_arms`, `sanc_military`,
  `sanc_financial`, `sanc_travel`, `sanc_other`, `sanc_trade_complete`
  (`descr_trade` contient `compl`), `sanc_trade_partial` (contient `part`).
- **`sanc_n_types`** = somme des 7 indicatrices (0-7), pour lire le canal.
- **Doses** : `sanc_n_active_all` (cas distincts par dyade-année) et
  `sanc_n_active_core` (cas avec ≥1 type ∈ {trade, financial, arms, military} ;
  exclut les cas purement travel/other = mesures individuelles ciblées).

### Validation
**Reproduction des 4 colonnes historiques : 0 différence** sur les 1 593 900
lignes (`sanction_any`, `sanction_trade`, `sanction_nontrade`,
`n_common_sanctioners`). Stats inchangées (sanction_nontrade 12.85 %,
sanction_any 13.01 %, n_common max = 14).

Comptes par type (year ≤ 2023, % du panel) : arms 10.22 %, military 7.15 %,
financial 8.10 %, travel 7.82 %, other 3.23 %, trade_complete 0.65 %,
trade_partial 3.23 %.

**Test « la dose travaille » — dyade Russie-USA :**

| année | sanc_n_types | sanc_n_active_all | sanc_n_active_core |
|---|---|---|---|
| 2014-2016 | 3 | 1 | 1 |
| 2017-2021 | 3 | 2 → 4 | 2 → 4 |
| **2022** | **4** | **11** | **11** |
| **2023** | **4** | **13** | **13** |

→ `sanc_n_types` quasi plat (3→4) ; la dose saute (4→11→13) en 2022-2023.
Agrégat Russie (exp = RUS) : moyenne cas actifs 1.7-2.0 (2014-2021) → 3.41 (2022)
→ 4.02 (2023) ; max 5 → 11 → 13. La dose distingue 2022 ; le type non.

---

## Étape 2 — Phase 3 : dCDH avec la dose en **paliers**

### Spécification
- Estimateur `did_multiplegt_dyn` (DIDmultiplegtDYN 2.1.2), en **logs**
  `log(trade_value + 1)` ; groupe = paire **non ordonnée** (`pkey`, commerce =
  somme des deux directions) ; temps = `year` ; cluster = paire ;
  `effects = 4`, `placebo = 2`.
- Traitement = `sanc_n_active_core` **discrétisé en paliers** (calé sur la distrib :
  médiane 1, p90 = 3, p95 = 4, p99 = 5) : **0 / 1 / 2-5 / 6+** (palier 0/1/2/3).
  Le palier 6+ est l'escalade lourde (Russie 2022-2023 = core 11-13).
- Fenêtre **2008-2023** (base pré-2014 pour les placebos, onset 2014, escalade 2022).

### Résultats — lecture PAR PALIER (Bloc A)

Le dCDH non-normalisé date l'effet au **1er changement de palier** (~2014 pour la
Russie) : c'est l'ATE dynamique du traitement par paliers sur les switchers, **pas**
un effet par niveau. On corrige avec deux lectures (script `11c`) :

**(1) Normalisé** (`normalized = TRUE`, effet par cran de dose) : placebos plats
(rel −1 = +0.027, rel −2 = −0.024, ns) ; effets −0.084 / −0.039 / −0.018 / −0.027 ;
**ATE = −0.0797** (SE 0.025) [−0.128, −0.031] — cohérent avec le PPML 3-way FE
(≈ −0.08 robustesse `04`) et le DiD statique non-commercial (−0.053, Phase 1).

**(2) dCDH binaire par seuil `1{core ≥ s}`** (effet de FRANCHIR le palier s) :

| seuil | placebos (rel −1, −2) | ATE | effets +1 / +2 / +3 / +4 | paires traitées |
|---|---|---|---|---|
| **core ≥ 1** (onset ≈2014) | +0.008, −0.066 (ns) | **−0.176** [−0.236, −0.117] | −0.136 / −0.128 / −0.203 / −0.190 | 5 509 |
| core ≥ 2 | +0.025, −0.042 (ns) | −0.066 (ns) [−0.147, +0.016] | −0.081 / −0.110 / +0.060 / −0.099 | 3 214 |
| **core ≥ 6** (escalade lourde — **2022** Russie-Occ.) | +0.0004, −0.008 (**plats**) | **−0.434** [−0.691, −0.177] | −0.178 (ns) / −0.634 / −0.568 / −0.671 | 147 |

- **Pré-tendances/placebos plats dans TOUTES les lectures** (IC contiennent 0) →
  parallélisme non rejeté, y compris pour le seuil 6+.
- **Réponse au 2022** : franchir le palier **6+** — ce que les dyades Russie-Occident
  font en 2022 — réduit le commerce de **≈ 43 % en moyenne** (ATE −0.434, logs),
  montant à −0.57 / −0.67 à +2/+4 ans. C'est **≈ 2.5×** l'effet de l'onset (−0.176).
- **Caveat** : le seuil 6+ agrège TOUS les franchissements vers 6+ (Iran, Syrie,
  Corée du Nord, Russie-2022…), pas seulement la Russie ; l'effet est celui de
  l'escalade lourde en général, dont Russie-2022 est une instance (147 paires →
  IC larges). Isoler Russie-2022 seule = cohorte spécifique (Phase 2).

Figure : `Output/Figures/EventStudy/es_fig02_dcdh_tiers.png` (onset vs escalade
lourde). Table : `Output/Tables/EventStudy/tab_dcdh_by_tier.csv` (normalisé + 3 seuils).

### Robustesses (Bloc C, script `11d`, `tab_dcdh_robustness.csv`)

- **(i) Dose alternative `n_senders_target`** (nb de senders sanctionnant la cible) :
  Russie 40 (2014) → 46 (2022) → 48 (2023) — **sature dès 2014**. dCDH `1{senders ≥ 20}` :
  placebos plats, ATE −0.117, effets montant à −0.171. → capte une coalition large
  (≈2014), **pas** l'escalade 2022. De plus la mesure est **contaminée par la
  décomposition ONU** (p90 = 192 senders : un embargo ONU = ~190 États-membres).
  → confirme par contraste que **seul le compte de cases capte 2022**.
- **(ii) Dose CONTINUE `sanc_n_active_core`** (brute, non paliers) : placebos plats,
  ATE **−0.066** (plus petit/bruité que le normalisé −0.080). Le linéaire traite
  chaque cas additionnel à l'identique → **dilue** l'effet et **gomme** le palier
  lourd (−0.43). C'est exactement ce que les paliers corrigent (GSDB-R4 note 1).
- **(iii) Lecture par TYPE** (`tab_russia_cases_by_type.csv`, cible Russie) :
  l'escalade 2022 est une **hausse proportionnelle ≈ ×4-5 de TOUS les types
  simultanément** (financial 9→40→46 ; trade 8→40→47 ; travel/arms/military de même),
  avec le commerce restant **partiel** (`trade_complete` = 1 case même en 2022-2023).
  → l'escalade est une **intensification large dans des canaux déjà actifs depuis
  2014**, pas un basculement vers l'embargo complet. Aucun canal unique ne la porte.

---

## Étape 3 — Trame d'ensemble (mise à jour)

- **Binaire / type** (Phases 1-2) : captent proprement **2014** et le **canal**
  (matériel via trade vs fragmentation via non-trade). Conservés.
- **Intensité en paliers (dCDH, Phase 3)** : **seule voie pour 2022**, l'escalade
  étant un changement de dose et non un onset. Effet net négatif, pré-tendances
  plates.
- Voie « event study trade-onset pour 2022 » **abandonnée** : la donnée la tue
  (l'onset des sanctions commerciales sur la Russie est lui aussi en 2014, partiel).

---

## Étape 4 — Problèmes rencontrés

1. **I/O chemin accentué (OneDrive, NFD)** : `arrow`/`haven`/`readxl` n'ouvrent pas
   les fichiers sous le chemin du projet. `06` corrigé : `PATH_ROOT` mis à jour +
   wrappers tempfile-ASCII (`read_parquet_safe`, `write_parquet_safe`,
   `read_dta_safe`, `read_excel_safe`). `fread`/`fwrite` non concernés.
2. **Bug data.table (closure)** : `c(list(..., year), .SD)` laissait `year`
   sans nom → `by=.(year)` résolvait vers `data.table::year` (fonction). Corrigé
   (`year = year`).
3. **dCDH mémoire (8 Go RAM)** : `did_multiplegt_dyn` sur 26 565 groupes →
   *vector memory exhausted*. **Repli** : fenêtre 2008-2023 + **échantillon de
   contrôles** (toutes les 5 509 paires ever-traitées + 4 000 paires jamais-traitées
   tirées au sort, seed 1234). L'échantillonnage des contrôles laisse l'ATE non
   biaisé, baisse la précision. Temps ≈ 2 min 40 (single-thread).
4. **dCDH cluster** : exige une variable numérique → cluster sur `gid` (id numérique
   1:1 avec `pkey` = cluster par paire).
5. **NSE des arguments `effects`/`placebo`** : `did_multiplegt_dyn` capte ces
   arguments par leur expression → passer des **littéraux** (`effects = 4`), pas une
   variable (`effects = EFF` → "Positive integer required").

### Caveats / lectures à garder en tête
- La lecture **par palier** (Bloc A) est désormais faite (normalisé + seuils binaires) :
  le seuil 6+ isole l'escalade lourde (2022). Le seuil 6+ agrège tous les
  franchissements vers 6+, pas seulement la Russie (147 paires → IC larges).
- Échantillon de contrôles (8 Go) : à refaire sur la totalité des paires
  (machine plus grosse / cloud) pour la précision finale.

### Inventaire des sorties (event study, après nettoyage Bloc D)
- Figures : `es_fig01_sunab_2014.png` (Phase 1, canal/2014),
  `es_fig02_dcdh_tiers.png` (Phase 3, onset vs escalade lourde).
- Tables : `tab_treatment_validation.csv` (+ `_meta`), `tab_static_did.csv`,
  `tab_eventstudy_sunab.csv`, `tab_dcdh_by_tier.csv`, `tab_dcdh_robustness.csv`,
  `tab_russia_cases_by_type.csv`.
- Rapports : `report_eventstudy_phase1.md`, `report_intensity_dcdh_phase3.md`.

**STOP avant le 2×2 vote ONU et le reste.**
