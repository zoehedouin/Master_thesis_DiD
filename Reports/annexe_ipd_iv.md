# Annexe documentée — l'IPD et l'IV (deux résultats réutilisables)

> Annexe de transparence du mémoire. Elle **sauve hors du dépôt actif** deux
> acquis de l'époque « IPD / IV » (scripts archivés sous `_archive/iv/` et
> `_archive/ipd_robustness/`) qui restent utiles au texte final :
> (1) l'IV comme **impasse assumée**, (2) l'**hétérogénéité temporelle de l'IPD**
> comme **motivation** du focus Russie 2014→2022.
>
> Sources distillées (désormais dans `Reports/_archive/`) :
> `2026-06-16_recap_analyse.md`, `2026-06-22_verifications_variables_robustesse.md`,
> et `_archive/output_legacy/Tables/Estimation_IV/iv_synthesis_report.md`.

---

## 1. L'IV comme impasse assumée

**Constat.** Aucun instrument bilatéral testé (alliances ATOP, rivalités/MID,
distance idéologique DPI, distance de régime Polity/V-Dem) n'est *simultanément* :

1. **fort** (au sens Kleibergen-Paap / Sargan-Hansen, sur cluster paire) ;
2. **excluable** sans hypothèse contestable — sanctions et alliances ont des
   canaux directs sur le commerce ; démocratie et idéologie aussi ;
3. **stable** entre instruments et entre échantillons.

L'instabilité observée résulte d'un **mélange** de trois causes — LATEs/compliers
différents, violations partielles d'exclusion, composition d'échantillon — que
les données ne permettent pas de séparer (cohérent avec Cevik, FMI 2024).
Illustration : l'estimation IV centrale donne un coefficient IPD de **+0.0432**
(SE 0.0336, p = 0.199), non significatif et de signe opposé au baseline PPML.

**Décision méthodologique.** L'IV ne sert plus d'identification principale. Elle
est rétrogradée en **sonde de transparence en annexe** : montrer (a) qu'on a
essayé, (b) pourquoi ça ne marche pas, (c) que le design DiD (sanctions/votes,
PPML + dCDH) est le résultat le plus défendable. C'est ce qui justifie d'avoir
archivé toute la famille IV (`05`, `05c`, `07`, `07b`, `07c`).

---

## 2. L'hétérogénéité temporelle de l'IPD comme motivation

Le fait saillant de l'époque robustesse n'est **pas** une discordance de mesure,
mais une **hétérogénéité temporelle** de l'effet de l'alignement (IPD) sur le
commerce, robuste à toute la famille FE three-way :

| Sous-période | N | Coef IPD | p |
|---|---|---|---|
| ≤ 2014 | 512 570 | **+0.042** | 0.021 |
| ≤ 2014 (famille MID) | 607 569 | +0.043 | 0.018 |
| **2015–2024** | 322 238 | **−0.198** | < 10⁻⁵ |

Lecture : l'IPD passe de **positif** (≤2014, ≤2018) à **fortement négatif**
(2015–2024 : −0.198, p < 10⁻⁵). Le basculement vient du **temps**, pas de la
composition d'échantillon (SMD des covariables tous < 0.17, cf. `8d`). Le
retournement de signe observé sur « l'échantillon commun » aux mesures
alternatives est un **artefact de fenêtre** (≤2014, troncature MID), et non un
problème de mesure. Le baseline PPML (−0.066) **agrège donc deux régimes**.

**Implication.** L'effet géopolitique négatif sur le commerce est un **phénomène
récent, post-2014** : il y a une longue ère de mondialisation où proximité
géopolitique et commerce sont faiblement liés, puis une ère post-2014 de
découplage marqué. C'est précisément ce qui **justifie le focus Russie 2014→2022**
du design DiD : on va chercher l'effet là où et quand il existe (le choc russe),
au lieu de le diluer sur 1995–2024.

---

*Cette annexe ne fait partie d'aucun script exécutable : c'est une note de
synthèse. Les chiffres ci-dessus proviennent des rapports archivés cités en
en-tête ; ils ne sont pas recalculés ici.*
