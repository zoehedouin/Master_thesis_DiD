# Documentation des mesures alternatives de distance géopolitique

Ce document décrit, pour chaque variable construite dans
`06_build_geopol_measures.R` et écrite dans `Data/Clean/iv_panel.parquet`,
ce qu'elle mesure, ses sources, sa formule, sa construction dyadique,
et l'harmonisation des identifiants.

**Cadrage.** Ces variables sont des **mesures alternatives** de la distance
géopolitique entre paires de pays, utilisées en *robustesse* dans le
mémoire (et non comme instruments — les diagnostics de `07b` et `07c` ont
montré que la stratégie IV ne converge pas, cf. annexe IV). Pour chaque
variable, la section « comme mesure » explique l'angle d'observation qu'elle
fournit. La discussion d'exclusion (utile si l'on tentait l'usage IV) est
isolée en encart « (pour usage IV uniquement) ».

Toutes les variables sont au niveau **paire-année directionnelle** dans le
même squelette que le panel master (231 pays × 30 ans × 2 directions hors
self-flows). Convention : `i` = exporteur (côté A), `j` = importateur
(côté B). Les mesures dont la construction est par essence undirected sont
symétriques par construction.

---

## Famille 1 — Institutional / régime politique

### `polyarchy_dist`

- **Mesure** : distance bilatérale entre niveaux de démocratie électorale.
- **Comme mesure** : capte la divergence sur le degré de compétition
  démocratique au sens V-Dem (élections libres, libertés civiles,
  contre-pouvoirs). Variable continue, fine sur la gradation des régimes.
- **Source** : V-Dem v16 (`V-Dem-CY-Full+Others-v16.csv`), variable
  `v2x_polyarchy` (Electoral Democracy Index, 0–1).
- **Couverture** : 1995–2024 (179 pays sur 231 du panel).
- **Formule** : `polyarchy_dist_ij,t = |v2x_polyarchy_i,t − v2x_polyarchy_j,t|`
- **Construction dyadique** : undirected (valeur absolue).
- **Harmonisation** : `country_text_id` V-Dem ≈ ISO3 natif. 5 codes
  historiques non mappés, non pertinents pour 1995+.
- **Dedup** : `(iso3, year)` dédoublonné par moyenne.
- *(Pour usage IV uniquement)* : l'exclusion supposerait que la
  démocratie domestique n'affecte pas le commerce après FE de paire et
  FE pays-année. Cette hypothèse est contestable (institutions ↔ qualité
  contractuelle, IDE, ouverture commerciale via la littérature
  Persson-Tabellini / OMC).

### `joint_dem_vdem`

- **Mesure** : niveau plancher de démocratie au sein de la paire.
- **Comme mesure** : capte si la paire contient au moins une autocratie
  (faible valeur) ou est jointly démocratique (haute valeur).
- **Source** : V-Dem v16, `v2x_polyarchy`.
- **Formule** : `joint_dem_vdem_ij,t = min(v2x_polyarchy_i,t, v2x_polyarchy_j,t)`
- **Construction dyadique** : undirected.
- **⚠️ Note technique** : **forte multicolinéarité** avec `polyarchy_dist`
  après FE three-way (sachant la valeur absolue de la différence et le
  niveau d'un des deux, on déduit l'autre). `fixest` la drop
  silencieusement en pratique. Conservée pour transparence ; **ne pas
  l'utiliser comme régresseur séparé**.

### `ideol_dist`

- **Mesure** : distance bilatérale d'idéologie politique de l'exécutif.
- **Comme mesure** : capte si les deux exécutifs au pouvoir partagent
  une orientation partisane (gauche, centre, droite). Variable
  *bas-axe* : très simplifiée par rapport à V-Dem.
- **Source** : Database of Political Institutions 2023 (DPI 2023),
  variable `execrlc` (executive party right-center-left).
- **Couverture** : 1995–2023 (131 pays). Maillon faible (≈16% du panel).
- **Variables brutes** : `execrlc` est de type *character* :
  - `"Right"` → recodé `1`
  - `"Center"` → recodé `2`
  - `"Left"` → recodé `3`
  - `"0"` (no party / military govt), `"-999"`, `"NA"` → recodé `NA`.
- **Formule** : `ideol_dist_ij,t = |execrlc_i,t − execrlc_j,t|` (valeurs
  possibles : 0, 1, 2).
- **⚠️ Hypothèses fortes du construit** :
  - (a) **Cardinalité imposée** : le recodage 1/2/3 puis `|diff|`
    suppose des espacements égaux entre Right–Center–Left sur ce qui
    est intrinsèquement une variable **ordinale** (`Center` n'est pas
    « à mi-chemin » de `Right` et `Left` au sens cardinal). L'écart
    `|Right − Left| = 2` vs `|Right − Center| = 1` est une convention.
  - (b) **Validité de construit** : l'axe `execrlc` est une idéologie
    *économique domestique* (gauche/droite sur la redistribution), qui
    colle imparfaitement à l'alignement de *politique étrangère*. Deux
    gouvernements de **même bord idéologique** peuvent être en conflit
    géopolitique ouvert (cas emblématique : la **rupture sino-soviétique**
    1960-1989, deux régimes communistes en rivalité ouverte malgré une
    idéologie économique partagée) ; deux gouvernements « de droite »
    peuvent diverger sur l'ordre international (ex. partenaires
    transatlantiques vs. populismes nationalistes).
- **Construction dyadique** : undirected.
- **Harmonisation** : DPI utilise `ifs` (codes IMF) ≈ ISO3 avec quelques
  divergences remappées (`ROM`→`ROU`, `WBG`→`PSE`, `ZAR`→`COD`,
  `GER`→`DEU`, `RSS`/`USR`→`RUS`). Fallback `countrycode::name_to_iso3()`
  si l'ifs ne mappe pas. 4 entités non mappées.
- **Dedup** : `(iso3, year)` par `first()`.

### `polity_dist`

- **Mesure** : distance bilatérale de régime politique sur l'échelle
  Polity (autocratie–démocratie composite).
- **Comme mesure** : alternative à `polyarchy_dist` sur échelle plus
  grossière mais historiquement utilisée. Sert à **valider la
  robustesse de mesure** entre V-Dem et Polity.
- **Source** : Polity5 v2018 (`Polity5.xls`, sheet `p5v2018`), `polity2`
  (−10 à +10).
- **Couverture** : 1995–2018 (162 pays). Coupure 2019-2024.
- **Formule** : `polity_dist_ij,t = |polity2_i,t − polity2_j,t|` (range
  possible : 0 à 20).
- **Construction dyadique** : undirected.
- **Harmonisation** : `ccode` (COW) → ISO3 via `cow_to_iso3()` avec
  custom_match pour post-1991.
- **Filtre qualité** : `flag != 0` (interregnums, occupations étrangères)
  → `NA`.

---

## Famille 2 — Strategic relations

### `allied_atop`

- **Mesure** : présence d'une alliance formelle active entre i et j.
- **Comme mesure** : capte l'existence d'un engagement défensif/militaire
  formel et codifié par traité. Variable binaire, forte mais grossière
  (n'exprime ni l'intensité ni la portée de l'alliance).
- **Source** : ATOP v5.1 (`atop5_1ddyr.csv`, dyad-year directional).
- **Couverture** : 1995–2018.
- **Variable brute** : `atopally` (0/1).
- **Formule** : `allied_atop_ij,t = atopally` après symétrisation.
- **Construction dyadique** : **undirected**. ATOP fournit deux lignes
  par paire (i→j, j→i) mais l'alliance est symétrique : on dédoublonne.
- **Harmonisation** : COW → ISO3 via `cow_to_iso3()`. 0 codes non mappés.
- *(Pour usage IV uniquement)* : argument d'exclusion fortement
  questionné par **Gowa & Mansfield (1993, *APSR* 87(2), "Power
  Politics and International Trade")** : ils montrent que les alliés
  commercent plus en raison d'externalités de sécurité (le commerce
  bilatéral augmente le bien-être des deux, ce qui renforce la coalition
  en cas de conflit). Le canal alliance → commerce n'est donc pas
  purement géopolitique → violation potentielle de l'exclusion.

### `shared_ally_atop`

- **Mesure** : nombre de pays tiers `k` alliés à la fois à `i` et à `j`
  la même année t.
- **Comme mesure** : capte l'« encastrement » de la paire dans un même
  réseau d'alliances. Plus la valeur est élevée, plus les deux pays sont
  proches du même bloc structurel.
- **Source** : ATOP v5.1, dérivé de `atopally`.
- **Formule** :
  `shared_ally_atop_ij,t = #{k ≠ i, j : allied_ik,t = 1 ET allied_jk,t = 1}`
- **Construction dyadique** : undirected ; calcul via self-join.
- **⚠️ Caveat** : très fortement corrélé avec `allied_atop` (corr ≈ 0.94)
  → quasi-colinéarité. **Retiré des spécifications principales du
  script `07c`** (`shared_ally` apporte peu d'info au-delà de `allied`).

### `shared_rival_mid`

- **Mesure** : nombre de pays tiers `k` avec lesquels i ET j ont
  simultanément un Militarized Interstate Dispute (MID) actif l'année t.
- **Comme mesure** : capte la dimension « my enemy's enemy » du réseau
  de conflits. Pas une mesure d'alignement intentionnel mais d'une
  position structurellement opposée à un même tiers.
- **Source** : Correlates of War Dyadic MID v4.03 (`dyadic_mid_4.03.csv`).
- **Couverture** : **1995–2014** (Maoz v4.03 s'arrête en 2014). La
  famille strategic est donc tronquée. Versions ultérieures (Palmer v5,
  GW-MID) étendent à 2018+ mais pas dans `Data/Raw/`.
- **Variables brutes** : `disno, statea, stateb, strtyr, endyear`.
  Chaque ligne = un MID-dyad sur sa durée totale.
- **Expansion temporelle** : chaque ligne étendue de `strtyr` à
  `min(endyear, 2024)` pour produire un MID-dyad-année.
- **Définition exacte de « rival commun »** : `rival(k, t) = 1` si k a
  un MID actif (n'importe quel hostility level) avec le pays focal à
  l'année t.
  `shared_rival_mid_ij,t = #{k ≠ i, j : MID actif (i,k,t) ET MID actif (j,k,t)}`
- **Construction dyadique** : undirected.
- **Harmonisation** : COW → ISO3.

### `mid_direct`

- **Mesure** : indicateur d'un MID direct entre i et j l'année t.
- **Comme mesure** : conflit ouvert dyadique. Variable distincte de
  `shared_rival_mid` qui capte les rivalités via tiers.
- **Source** : Correlates of War Dyadic MID v4.03.
- **Couverture** : 1995–2014.
- **Formule** : `mid_direct_ij,t = 1` si MID actif entre i et j en t.
- **Construction dyadique** : undirected.
- **Rôle** : **CONTROL, pas instrument**. Inclus comme covariable dans
  les régressions strategic_relations pour absorber l'effet mécanique
  du conflit direct sur le commerce, sans contaminer l'instrument
  `shared_rival_mid` qui mesure les rivalités *indirectes*.

---

## Famille 3 — Sanctions économiques (GSDB v4)

### `sanction_any`

- **Mesure** : sanction active entre i et j l'année t (toutes catégories
  confondues), undirected.
- **Comme mesure** : capte la **présence d'hostilité économique
  explicite et binaire** entre la paire. Variable forte mais avec
  caveats (cf. `sanction_trade` et `sanction_nontrade`).
- **Source** : Global Sanctions Data Base v4 (`gsdb_v4/GSDB_V4_dyadic.dta`,
  159 065 dyad-years).
- **Couverture** : 1995–2023, 96.7% du panel.
- **Format brut** : dyad-année **directionnel** (sender → target).
- **Formule** :
  ```
  sanction_any_ij,t = 1 si ∃ cas actif (i → j, t) OU (j → i, t)
                      0 sinon
  ```
- **Construction dyadique** : undirected (OR sur les deux directions).
- **Distribution** : 13.0% des paires-années.

### `sanction_trade`

- **Mesure** : sanction commerciale (embargo, restrictions exp/imp)
  active entre i et j l'année t.
- **Comme mesure** : capte les restrictions commerciales explicites.
- **Source** : GSDB v4, variable brute `trade` (0/1).
- **Formule** : `sanction_trade_ij,t = max(trade) sur cas actifs ij,t`
  puis undirected (max sur deux directions).
- **⚠️ Tautologique en gravity** : par construction, ces sanctions
  *coupent mécaniquement* le commerce qu'on cherche à expliquer.
  Utiliser cette variable comme régresseur dans une équation de gravité
  PPML revient à expliquer le commerce par lui-même. Variable conservée
  pour transparence, à **éviter** comme régresseur principal.
- **Distribution** : 3.9% des paires-années.

### `sanction_nontrade`

- **Mesure** : sanction non-commerciale (financière, voyage, armes,
  militaire, autre) active entre i et j l'année t.
- **Comme mesure** : capte la dimension **politique/sécuritaire** des
  sanctions (gel d'avoirs, restrictions de visa, embargos d'armes), en
  excluant le canal *trade* explicite. Variable recommandée comme proxy
  d'hostilité géopolitique.
- **Source** : GSDB v4, dérivée comme `1` si AU MOINS UN des types
  `arms`, `military`, `financial`, `travel`, `other` vaut 1.
- **Formule** :
  ```
  sanction_nontrade_ij,t = 1 si (arms=1 OU military=1 OU financial=1
                                  OU travel=1 OU other=1) sur tout cas
                                  actif (i→j ou j→i, t)
  ```
- **Construction dyadique** : undirected.
- **⚠️ Nuance importante** : `sanction_nontrade` est *moins directement
  liée au commerce* que `sanction_trade`, **mais pas exempte** :
  - les **sanctions financières** coupent le financement du commerce
    (lettres de crédit, transferts bancaires, accès au SWIFT) → réduisent
    le commerce *de facto* même sans restriction explicite ;
  - les **embargos d'armes** coupent directement le commerce d'armes (un
    sous-ensemble du commerce total, généralement codé sous HS9301-9306).
  Donc utiliser `sanction_nontrade` réduit la tautologie *par rapport à*
  `sanction_trade`, mais l'effet de commerce attendu reste contaminé par
  ces canaux financiers et sectoriels.
- **Distribution** : 12.85% des paires-années (≈ identique à
  `sanction_any`, donc la quasi-totalité des sanctions ont un volet
  non-trade).

### `n_common_sanctioners`

- **Mesure** : nombre d'**entités sender distinctes** qui sanctionnent à
  la fois `i` ET `j` la même année t.
- **Comme mesure** : capte la position « pariah » conjointe — deux pays
  sanctionnés par les mêmes acteurs (États unilatéraux ou coalitions
  ONU/UE) partagent un statut hors-norme dans l'ordre international.
- **Source** : GSDB v4, agrégation par self-join.
- **⚠️ Décision de construction (multilatéraux)** : GSDB v4 a deux
  subtilités à gérer :
  1. **Décomposition multilatérale** : un embargo ONU contre
     l'Afghanistan est codé comme 192 lignes (une par État membre).
     Compter naïvement = 192 sanctionneurs.
  2. **`case_id` est une chaîne comma-séparée** : par ex.
     `case_id = "1057,1354,1355"` signifie que ce sender participe à
     **3 cas atomiques** simultanément. Et un même cas atomique peut
     être réutilisé pour cibler différentes victimes à différentes
     années.

  Une approche naïve par `case_id` brut est **doublement biaisée** :
  - elle fragmente la coalition UE qui agit sous des `case_id` différents
    pour Iran (case 1057) et Russie (case 1006) → **EU comptée 0 fois**
    comme sanctionneur commun de Iran-Russie alors qu'elle les sanctionne
    tous deux.
  - elle ne dédoublonne pas correctement les cases multi-cibles.

  **Règle adoptée** : identifier chaque **coalition par la signature
  d'ensemble (set) de ses senders** observée dans les données.
  - on **explose** la chaîne `case_id` en cas atomiques individuels ;
  - pour chaque cas atomique, on récupère l'**ensemble trié des senders**
    qui y participent ;
  - deux cas atomiques avec **le même set de senders** → même
    `coalition_id` (factor numérique sur la signature).
  - capture correctement : EU sanctionne Iran (case 1057) ET Russie
    (case 1006) → même set des 27 membres UE → même `coalition_id` ✓ ;
    US unilatéral = coalition d'un seul membre, USA ; ONU = coalition
    de ~190 membres, identifiée une fois.
- **Formule** :
  ```
  n_common_sanctioners_ij,t = #{coalition k : k sanctionne i en t
                                              ET k sanctionne j en t}
  ```
- **Distribution post-correction** : mean = 1.27, médiane = 1, P90 = 2,
  **max = 14** (vs 189 avant la première correction et 9 après le
  premier patch incorrect par `case_id` brut).
- **Sanity check** :
  - Iran-Russie 2018-2021 : 3 sanctionneurs communs (US + UK + Canada,
    pré-Ukraine).
  - **Iran-Russie 2022 : 6 sanctionneurs communs** (UE rejoint
    post-invasion → US + UK + Canada + UE + ONU + ...).
  - Iran 2022 : partage 7 sanctionneurs avec Biélorussie, 5 avec Myanmar,
    4 avec Chine/Corée du Nord/Syrie/Ukraine/Zimbabwe.
- **Total** : 220 coalitions distinctes identifiées sur 1129 cas
  atomiques. 17.4% des paires-années ont `n > 0`.
- **Construction dyadique** : undirected.

### Note conceptuelle : sanctions vs. IPD

Les sanctions GSDB mesurent une **hostilité aiguë, binaire et rare** :
une décision politique discrète d'imposer un coût économique. À
l'inverse, l'IPD (UN voting alignment) est un **indicateur continu et
général** d'alignement diplomatique mesuré par la convergence sur
l'ensemble des résolutions ONU.

Ces deux variables relèvent donc de **construits conceptuellement
différents** :
- l'IPD capture le positionnement diplomatique routinier et général ;
- les sanctions capturent l'hostilité explicite et exceptionnelle.

→ Utiliser les sanctions à côté de l'IPD relève de la **validité
convergente** (deux mesures différentes du même *concept large* de
distance géopolitique doivent corréler partiellement), **pas du
substitut interchangeable**. Une corrélation modérée ne disqualifie ni
l'une ni l'autre.

---

## Instrument du script `05_gravity_iv.R` (cautionary)

### `instrument_l2` (reconstruit inline dans `07c`)

- **Mesure** : distance euclidienne entre les vecteurs d'alignement de
  i et j aux trois pôles géopolitiques USA, CHN, RUS, à l'année t−2.
- **Comme mesure** : « position dans l'espace d'alignement aux
  superpuissances », laguée.
- **Source** : IPD Bailey-Strezhnev-Voeten 1946-2025, exploitée via
  l'agrégation des relations bilatérales avec USA/CHN/RUS.
- **Formule** :
  ```
  instrument_l2_ij,t = sqrt(
    (IPD(i, USA, t−2) − IPD(j, USA, t−2))²
    + (IPD(i, CHN, t−2) − IPD(j, CHN, t−2))²
    + (IPD(i, RUS, t−2) − IPD(j, RUS, t−2))²
  )
  ```
- **Construction dyadique** : undirected.
- **⚠️ Caveat fondamental** : la variable est *construite à partir de
  l'IPD elle-même*, juste à un lag différent. Corrélation avec IPD à t
  = 0.90 → instrument fort mais quasi-indépendance illusoire.
- **Rôle** : **cautionary** dans le tableau `07c`. À ne pas considérer
  comme une mesure indépendante.

---

## Choix d'échantillon récapitulatif

| Famille | Borne temporelle | Source binding | Couverture |
|---|---|---|---|
| institutional | 1995–2023 | DPI | 16% (intersection V-Dem ∩ DPI) ; 56% pour V-Dem seul |
| strategic_relations | 1995–2014 | dyadic_mid v4.03 | 66.7% |
| sanctions GSDB | 1995–2023 | GSDB v4 | 96.7% |
| Polity (annex) | 1995–2018 | Polity5 v2018 | 36.6% |
| instrument_l2 | 1997–2024 | IPD lagué | 86% |

---

## Conventions de nommage et notes finales

- Toutes les valeurs `NA` hors fenêtre temporelle sont propagées
  explicitement à `NA_integer_` (cf. Section 8 de
  `06_build_geopol_measures.R`) pour distinguer « pas de sanction »
  (0) de « hors fenêtre » (NA).
- Le panel `iv_panel.parquet` est *self-contained* : il inclut aussi
  `trade_value`, `ipd`, `rta`, `log_dist`, `contig`, `comlang_off`,
  `colony`.
- **Toutes ces variables sont des mesures alternatives / robustesse**,
  pas des instruments validés. Les diagnostics de `07b` et `07c` ont
  montré que la stratégie IV ne converge pas : signes hétérogènes
  entre instruments, Hansen J rejette pour la plupart des familles
  combinées, et précisions trop faibles pour distinguer un effet
  IV non-nul de zéro. Le résultat principal du mémoire reste le
  **PPML 3-way FE** (Spec 4 du script `04_gravity_estimation.R`).
