# Synthèse — Event study des sanctions sur le commerce bilatéral

> *Synthèse transversale 08+09 (reste **centrale** dans `Reports/`). Structure par
> partie (réorg #2) : `03_treatments/03_build_treatments.R`, `08_ppml/08_ppml.R`,
> `09_dcdh/09_dcdh.R` ; sorties co-localisées dans `08_ppml/` et `09_dcdh/`. Carte
> du pipeline : [`README_pipeline.md`](README_pipeline.md).*

*Date : 2026-06-25. Objet : effet causal des sanctions (choc géopolitique) sur le
commerce bilatéral, cadre gravité PPML. Données : `iv_panel.parquet` (enrichi),
GSDB v4 brut. Scripts : `06` (enrichi), `10`, `11`, `11b`, `11c`, `11d`.
Reprend et consolide `report_eventstudy_phase1.md` et
`report_intensity_dcdh_phase3.md`. Chiffres exacts.*

---

## 0. Question et stratégie

On estime l'effet des sanctions sur le commerce dyadique, en distinguant trois
lectures complémentaires :
- **Binaire / type** → capte proprement **2014** et le **canal** (matériel via le
  commercial vs fragmentation via le non-commercial) ;
- **Onset event study (Sun & Abraham)** → la forme dynamique de l'effet, avec test
  de pré-tendances ;
- **Intensité en paliers (de Chaisemartin–D'Haultfœuille)** → **2022**, qui n'est
  **pas un nouvel onset mais une intensification**.

**Découverte structurante** : pour la Russie, tous les types de sanctions
s'allument ensemble en 2014 ; 2022 est une montée en intensité (nombre de cas
actifs 8 → 38 → 46), pas un nouvel onset. → une mesure de **dose** est nécessaire.

---

## 1. Données — enrichissement de `06` (additif)

GSDB v4 dyadique (`GSDB_V4_dyadic.dta`, 159 065 lignes) : déjà dyade-année,
**codes ISO3 natifs**, institutions (UE/ONU) **déjà dépaquetées en États membres**,
6 types binaires (`arms`, `military`, `trade`, `financial`, `travel`, `other`) +
sous-type commercial `descr_trade` (`exp/imp_compl`, `exp/imp_part`), `case_id`
(chaîne comma-séparée de cas atomiques).

### Colonnes ajoutées (10 ; `iv_panel` 23 → 33 colonnes), conventions identiques à l'existant (symétrique/undirected, max sur cas coexistants, fenêtre NA-après-2023)
- **Types** : `sanc_arms`, `sanc_military`, `sanc_financial`, `sanc_travel`,
  `sanc_other`, `sanc_trade_complete`, `sanc_trade_partial` ; `sanc_n_types` (0-7).
- **Doses** : `sanc_n_active_all` (cas distincts par dyade-année),
  `sanc_n_active_core` (cas avec ≥1 type ∈ {trade, financial, arms, military} ;
  exclut les cas purement travel/other = mesures individuelles ciblées).

### Validation
- **Reproduction des 4 colonnes historiques** (`sanction_any/trade/nontrade`,
  `n_common_sanctioners`) : **0 différence** sur 1 593 900 lignes.
- **Test « la dose travaille » (Russie-USA)** : `sanc_n_types` quasi plat (3→4) ;
  `sanc_n_active` saute 4 → 11 → 13 en 2022-2023. La dose distingue 2022, le type non.

---

## 2. Phase 1 — Sun & Abraham (sanctions non-commerciales : 2014 et le canal)

Traitement = `sanction_nontrade` (évite la tautologie de l'embargo). Spec PPML
identique à `04` : FE `exp_iso3^year + imp_iso3^year + pair`, `cluster = ~pair`,
zéros gardés. Script `10`.

### Validation du traitement
7 260 paires traitées, 19 305 jamais-traitées ; 2 707 left-censored (onset 1995).
Russie : **37 nouveaux partenaires en 2014** (Crimée), **3 seulement en 2022** (les
sanctionneurs de 2022 avaient déjà un onset en 2014). Caveat : `sanction_nontrade`
NA en 2024.

### DiD statique + contraste par type (réplique GSDB-R4, Yalcin et al. 2025)
| modèle | terme | estimate | SE | p |
|---|---|---|---|---|
| ancre | `treated_post` | −0.0529 | 0.0285 | 0.064 |
| contraste | `sanction_any` | −0.0916 | 0.0163 | <0.001 |
| contraste | `sanction_trade` | **−0.1271** | 0.0238 | <0.001 |
| contraste | `sanction_nontrade` | **−0.0426** | 0.0180 | 0.018 |

→ `trade` ≫ `nontrade` (≈ 3×), comme le benchmark. Codage des types validé. La
sanction commerciale capte en partie le canal mécanique → gardée comme contraste.

### Event study Sun & Abraham (non-commercial), fenêtre 2008-2023, 14 cohortes, N = 554 946
| temps relatif | estimate | SE | IC 95 % |
|---|---|---|---|
| −5 | −0.026 | 0.035 | [−0.096, +0.043] |
| −4 | +0.037 | 0.043 | [−0.048, +0.122] |
| −3 | −0.006 | 0.042 | [−0.089, +0.077] |
| −2 | −0.033 | 0.031 | [−0.093, +0.027] |
| 0 (transition) | −0.122 | 0.034 | [−0.188, −0.055] |
| +1 | −0.287 | 0.062 | [−0.408, −0.166] |
| +2 | −0.263 | 0.067 | [−0.394, −0.133] |
| +3 | −0.274 | 0.077 | [−0.425, −0.124] |
| +4 | −0.178 | 0.052 | [−0.279, −0.076] |
| +5 | −0.298 | 0.075 | [−0.444, −0.151] |

**ATT = −0.2655** (SE 0.059, p < 0.001).
- **Pré-tendances PLATES** (4 coefficients pré-onset tous non significatifs) → validité OK.
- Lecture à partir de **k = +1** (k = 0 = transition) : chute stable ≈ −0.27 à −0.30.
- Figure : `es_fig01_sunab_2014.png`.

---

## 3. Phase 3 — dCDH : intensité en paliers (2022)

Estimateur `did_multiplegt_dyn` (DIDmultiplegtDYN 2.1.2), traitement non-binaire
**non-absorbant** → capte l'escalade et la réversibilité. En **logs**
`log(trade+1)` ; groupe = paire non ordonnée (`pkey`, commerce des deux directions) ;
cluster = paire ; fenêtre 2008-2023 ; `effects = 4`, `placebo = 2`. Dose
`sanc_n_active_core` en **paliers 0 / 1 / 2-5 / 6+**. Scripts `11`, `11c`.

**Lecture par palier (placebos plats partout — IC contiennent 0) :**

| lecture | placebos (−1, −2) | ATE | effets +1/+2/+3/+4 | paires |
|---|---|---|---|---|
| normalisé (par cran de dose) | +0.027, −0.024 | **−0.080** [−0.128, −0.031] | −0.084/−0.039/−0.018/−0.027 | — |
| **core ≥ 1** (onset ≈2014) | +0.008, −0.066 | **−0.176** [−0.236, −0.117] | −0.136/−0.128/−0.203/−0.190 | 5 509 |
| core ≥ 2 | +0.025, −0.042 | −0.066 (ns) | −0.081/−0.110/+0.060/−0.099 | 3 214 |
| **core ≥ 6** (escalade lourde — 2022 Russie-Occ.) | +0.0004, −0.008 | **−0.434** [−0.691, −0.177] | −0.178(ns)/−0.634/−0.568/−0.671 | 147 |

- **Réponse au 2022** : franchir le palier **6+** réduit le commerce de **≈ 43 %**
  en moyenne (logs), montant à −0.57/−0.67 à +2/+4 ans — **≈ 2.5×** l'effet de
  l'onset (−0.176). Pré-tendances plates, y compris au seuil 6+.
- ATE normalisé −0.080 cohérent avec le PPML 3-way FE (≈ −0.08) et l'ancre
  statique non-commerciale (−0.053).
- Figure : `es_fig02_dcdh_tiers.png` (onset vs escalade lourde).
- **Caveat** : le seuil 6+ agrège tous les franchissements vers 6+ (Iran, Syrie,
  Corée du Nord, Russie-2022…), pas la Russie seule (147 paires → IC larges).

---

## 4. Robustesses (script `11d`)

- **(i) Dose alternative `n_senders_target`** : Russie 40 (2014) → 46 (2022) →
  48 (2023), **sature dès 2014** ; dCDH `1{senders ≥ 20}` : ATE −0.117, montant à
  −0.171, placebos plats → capte la coalition large (≈2014), **pas** l'escalade 2022.
  Mesure **contaminée par la décomposition ONU** (p90 = 192 senders). → confirme par
  contraste que **seul le compte de cases capte 2022**.
- **(ii) Dose CONTINUE** `sanc_n_active_core` brute : ATE **−0.066**, plus petit et
  bruité ; le linéaire **dilue** l'effet et **gomme** le palier lourd (−0.43). C'est
  ce que les paliers corrigent (GSDB-R4 note 1 : + de sanctions ≠ impact proportionnel).
- **(iii) Lecture par TYPE** (Russie) : l'escalade 2022 = **hausse proportionnelle
  ≈ ×4-5 de TOUS les types** (financial 9→46 ; trade 8→47 ; travel/arms/military de
  même), commerce restant **partiel** (`trade_complete` = 1) → intensification large
  dans des canaux déjà actifs depuis 2014, pas un basculement vers l'embargo complet.

---

## 5. Lecture d'ensemble

- **2014** (onset / canal) : capté proprement par le binaire/type et le sunab
  (ATT −0.27 ; `trade` ≫ `nontrade`). Effet net négatif, pré-tendances plates.
- **2022** (intensification) : capté **uniquement** par la dose en paliers ; franchir
  le palier lourd (6+) réduit le commerce ≈ 2.5× plus que l'onset. La voie « event
  study trade-onset pour 2022 » est **abandonnée** (la donnée la tue : onset trade
  aussi en 2014, partiel).
- L'escalade est une **intensification large** (tous canaux ×4-5), pas un nouveau
  canal ni un embargo complet.

---

## 6. Problèmes rencontrés & caveats

1. **I/O chemin accentué (OneDrive, NFD)** : `arrow`/`haven`/`readxl` n'ouvrent pas
   les fichiers du projet → wrappers tempfile-ASCII dans `06`/`10`/`11*`
   (`read_parquet_safe`, etc.) ; `fread`/`fwrite`/`ggsave` non concernés.
   `PATH_ROOT` de `06` mis à jour (était `/Users/zoe/Desktop/...`, obsolète).
2. **Mémoire 8 Go** : (a) le sunab complet 1995-2023 (~27 cohortes, ~400 termes)
   thrashait → fenêtre 2008-2023, 14 cohortes ; (b) dCDH sur 26 565 groupes → OOM →
   **échantillon de contrôles** (toutes paires ever-traitées + 4 000 jamais-traitées
   tirées au sort, seed 1234). ATE non biaisé, **précision réduite** → un run
   plein échantillon (machine plus grosse / cloud) reste à faire.
3. **Bugs corrigés** : `nobs(model)` masqué par une colonne `model` (capter avant) ;
   `c(list(..., year), .SD)` → `year` non nommé résolu vers `data.table::year` ;
   NSE de `sunab(bin.rel=)` et de `did_multiplegt_dyn(effects=, placebo=)` →
   passer des **littéraux** / inliner les bornes.

---

## 7. Livrables

**Scripts** : traitement = `03_treatments/03_build_treatments.R` (ex-`06`, enrichi ;
sauvegarde `.bak_20260624` dans `_archive/backups/`) ; PPML/event study =
`08_ppml/08_ppml.R` (fusion ex-`04`+ex-`10`) ; intensité dCDH = `09_dcdh/09_dcdh.R`
(fusion `11`+`11b`+`11c`+`11d`).

**Figures** : `08_ppml/figures/es_fig01_sunab_2014.png` ;
`09_dcdh/figures/es_fig02_dcdh_tiers.png`.

**Tables** — `08_ppml/tables/` : `tab_treatment_validation.csv` (+ `_meta`),
`tab_static_did.csv`, `tab_eventstudy_sunab.csv` ; `09_dcdh/tables/` :
`tab_dcdh_by_tier.csv`, `tab_dcdh_robustness.csv`, `tab_russia_cases_by_type.csv`.

**Rapports** : `08_ppml/08_report.md` (ex-`report_eventstudy_phase1.md`),
`09_dcdh/09_report.md` (ex-`report_intensity_dcdh_phase3.md`), et la présente
synthèse (`Reports/report_sanctions_synthese.md`).

**Données** : `Data/Clean/iv_panel.parquet` (régénéré, 33 colonnes ;
sauvegarde `iv_panel_backup_20260624.parquet`).

**À faire (non lancé, STOP demandé)** : 2×2 vote ONU (ES-11/1 2022) × sanction sur
les dyades Russie ; run dCDH plein échantillon ; HonestDiD.
