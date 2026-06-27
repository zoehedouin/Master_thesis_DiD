# =============================================================================
# 00_setup.R
# -----------------------------------------------------------------------------
# Source unique de verite du pipeline (memoire M2 — DiD sanctions/votes ONU,
# centre Russie). A SOURCER EN TETE DE CHAQUE SCRIPT :  source("00_setup.R")
#
# Fournit :
#   * Detection des racines (DATA_ROOT, PROJECT_ROOT) sans chemin code en dur,
#     avec replis OneDrive puis Desktop pour les anciennes machines.
#   * Tous les chemins derives : PATH_RAW / PATH_CLEAN / PATH_BACI / PATH_GRAV /
#     PATH_IPD / PATH_IV / PATH_OUT, et les helpers out_tab() / out_fig() /
#     out_map() / out_rep() qui creent le sous-dossier Output au besoin.
#   * Les wrappers I/O robustes au chemin accentue NFD (OneDrive) :
#     read_parquet_safe / write_parquet_safe / read_dta_safe / read_excel_safe.
#   * Les helpers de journalisation : log_step / tic / toc.
#   * Le reglage du parallelisme (data.table, fixest si present).
#
# Historique : centralise ce qui etait duplique dans 01..11 (anciens PATH_ROOT
# "/Users/zoe/Desktop/Master_thesis" pour le legacy, et l'OneDrive accentue
# pour 06/10/11). Aucun script ne doit plus redefinir chemins ni wrappers.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

# ---- Detection des racines --------------------------------------------------
# DATA_ROOT     : dossier contenant Data/Clean/ (panels parquet, CENTRAL).
# ANALYSIS_ROOT : racine du depot analytique (contient 00_setup.R, .git, les
#                 dossiers de partie 01_*..11_*, Reports/). Les sorties de
#                 SECTION sont co-localisees sous ANALYSIS_ROOT/<PART>/.
# Sur cette machine les deux DIFFERENT (Data un niveau au-dessus de la racine
# analytique) ; sur les layouts OneDrive/Desktop ils coincident. On detecte
# chacun en remontant depuis getwd(), avec une liste de replis explicites.

.ascend_candidates <- function() {
  # getwd() et ses ancetres (jusqu'a 6 niveaux) + replis machines connues.
  here <- normalizePath(getwd(), mustWork = FALSE)
  ups  <- here
  for (i in 1:6) ups <- c(ups, dirname(ups[length(ups)]))
  fallbacks <- c(
    "/Users/zoe/Thesis/Master_thesis_DiD",
    "/Users/zoe/Thesis",
    "/Users/zoe/Library/CloudStorage/OneDrive-UniversiteParis-Dauphine/Master_thesis",
    "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis",
    "/Users/zoe/Desktop/Master_thesis"
  )
  unique(c(ups, fallbacks))
}

# Renvoie le premier ancetre contenant l'UN des marqueurs (fichier ou dossier).
.find_root <- function(markers, label) {
  for (cand in .ascend_candidates()) {
    if (any(file.exists(file.path(cand, markers)))) return(cand)
  }
  stop(sprintf("00_setup.R : impossible de localiser %s (marqueurs : %s). ",
               label, paste(markers, collapse = ", ")),
       "Lancez les scripts depuis la racine analytique ou un dossier de partie, ",
       "ou ajoutez la racine a .ascend_candidates().")
}

DATA_ROOT     <- .find_root(file.path("Data", "Clean"), "DATA_ROOT")
ANALYSIS_ROOT <- .find_root(c(".git", "00_setup.R"), "ANALYSIS_ROOT")
PROJECT_ROOT  <- ANALYSIS_ROOT   # alias retro-compatible

# ---- Chemins derives --------------------------------------------------------
PATH_RAW   <- file.path(DATA_ROOT, "Data", "Raw")
PATH_CLEAN <- file.path(DATA_ROOT, "Data", "Clean")
PATH_BACI  <- file.path(PATH_RAW, "BACI_HS92_V202601")
PATH_GRAV  <- file.path(PATH_RAW, "Gravity")
PATH_IPD   <- file.path(PATH_RAW, "IPD")
PATH_IV    <- file.path(PATH_RAW, "IV")

# Panels canoniques (entrees des scripts d'analyse).
PATH_MASTER         <- file.path(PATH_CLEAN, "master_panel.parquet")
PATH_STRATEGIC      <- file.path(PATH_CLEAN, "master_panel_with_strategic.parquet")
PATH_SANCTIONS_PANEL <- file.path(PATH_CLEAN, "sanctions_panel.parquet")
PATH_UN_VOTES        <- file.path(PATH_CLEAN, "un_votes.parquet")
PATH_COVARIATES      <- file.path(PATH_CLEAN, "covariates_panel.parquet")
# NB : l'ancien iv_panel.parquet est devenu un artefact LEGACY (distances IV),
# produit par _archive/iv_legacy/build_iv_panel.R dans Data/Clean/_archive/.
# Aucun script actif ne le lit -> PATH_IV_PANEL retire de 00_setup.R.

dir.create(PATH_CLEAN, showWarnings = FALSE, recursive = TRUE)

# Helpers de sortie : CO-LOCALISATION PAR PARTIE -----------------------------
# Chaque script d'analyse (06-10) declare en tete : PART <- "07_ppml".
# out_tab/out_fig/out_map renvoient le DOSSIER de sortie de la partie courante
# (ANALYSIS_ROOT/<PART>/{tables,figures,maps}), cree au besoin. L'argument
# (ancien theme : "EventStudy", "Estimation"...) est ACCEPTE pour compatibilite
# mais IGNORE : le routage se fait par PART. Consequence : aucun site d'appel
# out_*() n'a besoin d'etre modifie, et le split EventStudy entre 07 et 08 se
# fait automatiquement (07 a PART="07_ppml", 08 a PART="08_dcdh").
.part_dir <- function(kind) {
  if (!exists("PART", inherits = TRUE) || is.null(PART) || !nzchar(PART))
    stop("00_setup.R : variable PART non definie. Ajoutez PART <- \"NN_partie\" ",
         "en tete du script (apres le source) avant tout appel a out_*().")
  p <- file.path(ANALYSIS_ROOT, PART, kind)
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
  p
}
out_tab <- function(sub = NULL) .part_dir("tables")
out_fig <- function(sub = NULL) .part_dir("figures")
out_map <- function(sub = NULL) .part_dir("maps")
# Rapports : CENTRAUX (synthese / transversaux) sous ANALYSIS_ROOT/Reports/.
# Les rapports de PARTIE (NN_report.md) vivent dans le dossier de la partie et
# sont rediges a la main (pas via ce helper).
out_rep <- function(sub = NULL) {
  p <- file.path(ANALYSIS_ROOT, "Reports")
  if (!is.null(sub)) p <- file.path(p, sub)
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
  p
}

# ---- I/O robuste au chemin accentue NFD (OneDrive) --------------------------
# Les lecteurs C++ (arrow / haven / readxl) n'ouvrent pas un chemin contenant
# un 'e' accentue normalise NFD. On copie d'abord vers un tempfile ASCII (la
# copie de base R gere le NFD), puis on lit/ecrit. fread/fwrite/ggsave de base
# fonctionnent directement et ne sont pas wrappes.
read_parquet_safe <- function(path, ...) {
  tmp <- tempfile(fileext = ".parquet")
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(arrow::read_parquet(tmp, ...))
}
write_parquet_safe <- function(x, path, ...) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(x, tmp, ...)
  stopifnot(file.copy(tmp, path, overwrite = TRUE)); unlink(tmp)
}
read_dta_safe <- function(path, ...) {
  tmp <- tempfile(fileext = ".dta")
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(haven::read_dta(tmp, ...))
}
read_excel_safe <- function(path, ...) {
  ext <- paste0(".", tools::file_ext(path))
  tmp <- tempfile(fileext = ext)
  stopifnot(file.copy(path, tmp, overwrite = TRUE)); on.exit(unlink(tmp))
  as.data.table(readxl::read_excel(tmp, ...))
}

# ---- Journalisation et chrono ----------------------------------------------
log_step <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
tic <- function() invisible(.GlobalEnv$.tic_t <- proc.time()[3])
toc <- function() round(proc.time()[3] - .GlobalEnv$.tic_t, 1)

# ---- Bornes temporelles par defaut du panel master -------------------------
YEAR_MIN <- 1995L
YEAR_MAX <- 2024L

# ---- Parallelisme -----------------------------------------------------------
setDTthreads(0)  # data.table : tous les coeurs
if (requireNamespace("fixest", quietly = TRUE)) {
  fixest::setFixest_nthreads(0)  # fixest : tous les coeurs (mono-thread sur cette machine)
}

log_step(sprintf("00_setup.R OK | DATA_ROOT=%s | ANALYSIS_ROOT=%s",
                 DATA_ROOT, ANALYSIS_ROOT))
