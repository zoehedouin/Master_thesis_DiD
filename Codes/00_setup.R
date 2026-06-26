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
# DATA_ROOT    : dossier contenant Data/Clean/  (les panels parquet).
# PROJECT_ROOT : dossier contenant Codes/ et Output/ (les scripts et sorties).
# Sur cette machine les deux DIFFERENT (Data un niveau au-dessus de Codes) ;
# sur les layouts OneDrive/Desktop ils coincident. On detecte chacun
# separement en remontant depuis getwd(), avec une liste de replis explicites.

.ascend_candidates <- function() {
  # getwd() et ses ancetres (jusqu'a 5 niveaux) + replis machines connues.
  here <- normalizePath(getwd(), mustWork = FALSE)
  ups  <- here
  for (i in 1:5) ups <- c(ups, dirname(ups[length(ups)]))
  fallbacks <- c(
    "/Users/zoe/Thesis",
    "/Users/zoe/Thesis/Master_thesis_DiD",
    "/Users/zoe/Library/CloudStorage/OneDrive-UniversiteParis-Dauphine/Master_thesis",
    "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis",
    "/Users/zoe/Desktop/Master_thesis"
  )
  unique(c(ups, fallbacks))
}

.find_root <- function(marker, label) {
  for (cand in .ascend_candidates()) {
    if (dir.exists(file.path(cand, marker))) return(cand)
  }
  stop(sprintf("00_setup.R : impossible de localiser %s (marqueur '%s'). ",
               label, marker),
       "Lancez les scripts depuis Codes/ ou la racine du projet, ou ajoutez ",
       "la racine a .ascend_candidates().")
}

DATA_ROOT    <- .find_root(file.path("Data", "Clean"), "DATA_ROOT")
PROJECT_ROOT <- tryCatch(.find_root("Output", "PROJECT_ROOT"),
                         error = function(e) DATA_ROOT)

# ---- Chemins derives --------------------------------------------------------
PATH_RAW   <- file.path(DATA_ROOT, "Data", "Raw")
PATH_CLEAN <- file.path(DATA_ROOT, "Data", "Clean")
PATH_BACI  <- file.path(PATH_RAW, "BACI_HS92_V202601")
PATH_GRAV  <- file.path(PATH_RAW, "Gravity")
PATH_IPD   <- file.path(PATH_RAW, "IPD")
PATH_IV    <- file.path(PATH_RAW, "IV")
PATH_OUT   <- file.path(PROJECT_ROOT, "Output")

# Panels canoniques (entrees des scripts d'analyse).
PATH_MASTER     <- file.path(PATH_CLEAN, "master_panel.parquet")
PATH_STRATEGIC  <- file.path(PATH_CLEAN, "master_panel_with_strategic.parquet")
PATH_IV_PANEL   <- file.path(PATH_CLEAN, "iv_panel.parquet")

dir.create(PATH_CLEAN, showWarnings = FALSE, recursive = TRUE)

# Helpers de sortie : renvoient un chemin dans Output/<famille>/<sub> en creant
# le dossier au besoin. Ex. out_tab("EventStudy") -> .../Output/Tables/EventStudy
.out_dir <- function(family, sub = NULL) {
  p <- file.path(PATH_OUT, family)
  if (!is.null(sub)) p <- file.path(p, sub)
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
  p
}
out_tab <- function(sub = NULL) .out_dir("Tables",  sub)
out_fig <- function(sub = NULL) .out_dir("Figures", sub)
out_map <- function(sub = NULL) .out_dir("Maps",    sub)
out_rep <- function(sub = NULL) .out_dir("Reports", sub)

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

log_step(sprintf("00_setup.R OK | DATA_ROOT=%s | PROJECT_ROOT=%s",
                 DATA_ROOT, PROJECT_ROOT))
