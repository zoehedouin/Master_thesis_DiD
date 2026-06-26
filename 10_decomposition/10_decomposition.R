# =============================================================================
# 10_decomposition.R   (etape 5 — decomposition strategique / non-strategique)
# -----------------------------------------------------------------------------
# A FAIRE UNE FOIS un resultat propre obtenu sur le commerce TOTAL (08/09).
# Re-tourne les MEILLEURES specs (event study PPML de 08_ppml.R + AVSQ paliers
# de 09_dcdh.R) sur les deux outcomes decomposes du panel strategique (02) :
#   * strategic_trade_value      (commerce strategique, HS6 — Aiyar et al. 2024)
#   * non_strategic_trade        (= trade_value - strategic_trade_value)
#
# Hypothese de mecanisme (feuille de route §5) : l'effet se concentre sur le
# NON-STRATEGIQUE / hors-energie (l'energie est largement exemptee et reroutee
# vers Chine/Inde). C'est aussi le test de TAUTOLOGIE : si l'effet deborde sur le
# commerce non directement vise par l'embargo, c'est de la vraie fragmentation.
# Contribution propre vs GSDB-R4 (qui declare ne pas pouvoir identifier les
# secteurs touches par les sanctions commerciales partielles — possible ici via HS6).
#
# Output : Output/Tables/Decomposition/ , Output/Figures/Decomposition/
# Entrees : master_panel_with_strategic.parquet (02), iv_panel.parquet (03).
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

# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})  # PATH_*, wrappers, out_tab/out_fig, log_step/tic/toc
PART <- "10_decomposition"   # co-localisation des sorties de cette partie (out_*)

PATH_TAB <- out_tab("Decomposition")
PATH_FIG <- out_fig("Decomposition")

log_step("10_decomposition : setup OK.")

df <- read_parquet_safe(PATH_STRATEGIC)
stopifnot("strategic_trade_value" %in% names(df))
df[, non_strategic_trade := pmax(trade_value - strategic_trade_value, 0)]
log_step(sprintf("panel strategique : %d lignes.", nrow(df)))


## TODO (feuille de route §5 — Decomposition strategique / non-strategique)
## ---------------------------------------------------------------------------
## 1. RE-TOURNER l'event study PPML (Sun & Abraham, spec exacte de 08_ppml.R :
##    FE exp^year + imp^year + pair, cluster ~pair, zeros gardes) sur DEUX
##    outcomes separement : strategic_trade_value puis non_strategic_trade.
##    -> comparer les profils dynamiques (k=+1, +2, ...) entre les deux.
##
## 2. RE-TOURNER l'AVSQ did_multiplegt_dyn (paliers d'intensite 0/1/2-5/6+,
##    spec de 09_dcdh.R, en logs) sur log(strategic+1) et log(non_strategic+1).
##    Reutiliser pkey, fenetre, et l'option SAMPLE_CONTROLS de 09.
##
## 3. LIRE LE MECANISME :
##    - Tester l'hypothese "effet concentre sur le non-strategique/hors-energie".
##    - Test de tautologie : un effet sur le commerce NON vise par l'embargo =
##      fragmentation reelle (pas mecanique).
##    - Optionnel : isoler l'energie (HS27) du reste du strategique.
##
## 4. SORTIES (table cote a cote strategique vs non-strategique) :
##    - Output/Tables/Decomposition/tab_decomp_eventstudy.{csv,tex}
##    - Output/Tables/Decomposition/tab_decomp_avsq.csv
##    - Output/Figures/Decomposition/fig_decomp_eventstudy.png
##
## Packages : fixest, DIDmultiplegtDYN, ggplot2, data.table, arrow.
## NE RIEN INVENTER : squelette uniquement (re-utilise les specs validees de 08/09).

log_step("10_decomposition : squelette charge. Implementer apres resultat propre sur le total.")
