# =============================================================================
# 11_robustness.R   (etape 6 — robustesses et tests de falsification)
# -----------------------------------------------------------------------------
# Tests de robustesse et de falsification du design DiD (feuille de route §6),
# une fois les estimations principales obtenues (08_ppml.R, 09_dcdh.R) :
#   * PLACEBOS : dates de traitement factices (cutoffs avances/recules) ;
#     unites factices (paires jamais traitees assignees au traitement).
#   * GROUPES DE CONTROLE / FENETRES alternatifs : restreindre/ponderer les
#     controles (cf. exposition pre-2014 de 05) ; varier la fenetre temporelle.
#   * TRIANGULATION PPML vs LOGS : table cote a cote ; comparer signes et ordres
#     de grandeur (semi-elasticite PPML vs elasticite log — PAS les coefs au
#     centieme : unites differentes).
#   * HETEROGENEITE PAR SENDER : USA/Canada plus stricts, Japon/UK plus indulgents
#     (cf. GSDB-R4 Fig. 7) ; heterogeneite intra-UE.
#   * CAVEAT REORIENTATION : baisse des couts Russie-Chine/Inde/Turquie (tiers
#     compensateurs) — a discuter comme limite, voire explorer en analyse tiers.
#   * INFERENCE : cluster paire NON ordonnee ; robustesse au three-way
#     (exportateur, importateur, temps).
#
# Output : Output/Tables/Robustness/ , Output/Figures/Robustness/
# Entrees : master_panel_with_strategic.parquet, iv_panel.parquet ; specs de 08/09.
# Chemins / wrappers I/O / helpers : 00_setup.R.
# =============================================================================

# ---- Section 0 : Setup ------------------------------------------------------

need <- c("data.table", "arrow", "fixest", "DIDmultiplegtDYN", "ggplot2")
miss <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(miss)) message("Packages manquants (a installer) : ",
                          paste(miss, collapse = ", "))

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(fixest)
  library(ggplot2)
})

source("00_setup.R")  # PATH_*, wrappers, out_tab/out_fig, log_step/tic/toc

PATH_TAB <- out_tab("Robustness")
PATH_FIG <- out_fig("Robustness")

log_step("11_robustness : setup OK.")

df <- read_parquet_safe(PATH_STRATEGIC)
log_step(sprintf("panel : %d lignes.", nrow(df)))


## TODO (feuille de route §6 — Robustesses et falsification)
## ---------------------------------------------------------------------------
## 1. PLACEBOS :
##    - Dates factices : decaler le cutoff de traitement (+/- 2,3 ans avant le
##      vrai onset) -> l'effet "placebo" doit etre nul.
##    - Unites factices : assigner le traitement a des paires JAMAIS traitees
##      -> effet nul attendu.
##
## 2. GROUPES DE CONTROLE / FENETRES :
##    - Restreindre/ponderer les controles aux partenaires comparables
##      (matching/ponderation sur exposition pre-2014 + dependance energetique, 05).
##    - Varier la fenetre temporelle (ex. 2010-2023, 2012-2024) et verifier la
##      stabilite des estimations.
##
## 3. TRIANGULATION PPML vs LOGS (table cote a cote) :
##    - Memes traitements, deux estimateurs : fepois (PPML, 08) vs feols sur
##      log(trade+1) (cadre dCDH, 09). Comparer SIGNES et ORDRES DE GRANDEUR.
##    - Sortie : Output/Tables/Robustness/tab_triangulation_ppml_logs.{csv,tex}.
##
## 4. HETEROGENEITE PAR SENDER :
##    - Interagir le traitement avec l'identite du sender (USA, CAN, JPN, GBR,
##      UE...) ; documenter USA/CAN stricts vs JPN/GBR indulgents (GSDB-R4 Fig.7)
##      et l'heterogeneite intra-UE.
##
## 5. CAVEAT REORIENTATION (tiers) :
##    - Discuter / explorer la compensation par Chine/Inde/Turquie (baisse des
##      couts commerciaux Russie-tiers) comme limite a l'interpretation.
##
## 6. INFERENCE :
##    - Cluster paire NON ordonnee (pkey) ; robustesse au three-way clustering
##      (exportateur, importateur, temps) via vcov.
##
## Packages : fixest, DIDmultiplegtDYN, ggplot2, data.table, arrow.
## NE RIEN INVENTER : squelette uniquement.

log_step("11_robustness : squelette charge. Implementer apres les estimations principales.")
