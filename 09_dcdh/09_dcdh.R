# =============================================================================
# 09_dcdh.R — fusion of 11 + 11b + 11c + 11d, + skeleton dist_lag_het
#             (feuille de route §4).
# -----------------------------------------------------------------------------
# §4.1  AVSQ : effet de l'INTENSITE des sanctions (dose) sur le commerce
#       bilateral via de Chaisemartin & D'Haultfoeuille (did_multiplegt_dyn,
#       DIDmultiplegtDYN), traitement non-binaire NON-ABSORBANT -> capte
#       l'escalade 2014->2022 et la reversibilite, ce que l'event study sunab
#       absorbant (binaire/onset) ne peut pas. Dose = sanc_n_active_core,
#       DISCRETISEE EN PALIERS (0 / 1 / 2-5 / 6+). Estimateur en LOGS
#       (log(trade+1)). Groupe = paire non ordonnee (pkey) ; temps = year ;
#       cluster = paire (gid). Placebos = pre-tendances.
#       Fusionne, dans l'ordre :
#         [11 ]  build panel pkey + dCDH principal (paliers).
#         [11c]  effets PAR PALIER (normalise + binaire par seuil).
#         [11b]  outputs : table event-study + figure du dCDH principal.
#         [11d]  robustesses (dose alternative, dose continue, lecture par type).
#
# §4.2  dist_lag_het / DistLagHet : raffinement distributed-lag heterogene
#       (SKELETON — header + TODO uniquement, voir derniere section).
#
# -----------------------------------------------------------------------------
# Decouverte motivante (cf. report Partie 1) : pour la Russie tous les types de
# sanctions s'allument en 2014 ; 2022 n'est PAS un nouvel onset mais une
# INTENSIFICATION (n_active_core 8 -> 38 -> 46). Le binaire/type/onset sature ;
# seule la dose distingue 2022 de 2014. Paliers : GSDB-R4 note 1 : + de
# sanctions != impact proportionnel -> ordinaliser plutot que lineaire.
# =============================================================================

# ---- Setup consolide : une seule union de library() + 00_setup.R -----------
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(DIDmultiplegtDYN)
  library(ggplot2)
  library(haven)
})
# --- bootstrap : remonte jusqu'au dossier de 00_setup.R (racine analytique) --
local({
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "00_setup.R")) && dirname(.d) != .d) .d <- dirname(.d)
  if (!file.exists(file.path(.d, "00_setup.R")))
    stop("00_setup.R introuvable en remontant depuis ", getwd())
  source(file.path(.d, "00_setup.R"))  # local=FALSE -> objets dans .GlobalEnv
})
PART <- "09_dcdh"   # co-localisation des sorties de cette partie (out_*)

# ---- Echantillonnage des controles (contrainte memoire 8 Go) ---------------
# Les scripts 11* sous-echantillonnent les paires jamais-traitees (controles)
# car dCDH alloue des matrices ~ nb de groupes (26 565 paires -> OOM sur 8 Go).
# Ce parametre expose ce comportement :
#   SAMPLE_CONTROLS <- TRUE  : reproduit EXACTEMENT le comportement legacy
#                              (toutes les ever-traitees + N_CTRL controles
#                              tires au sort, seed 1234).
#   SAMPLE_CONTROLS <- FALSE : passe a FALSE sur une machine plus grosse pour
#                              tourner sur l'echantillon COMPLET (aucun
#                              sous-echantillonnage des controles ; cf. feuille
#                              de route — run full sur machine a plus de RAM).
# Mettre TRUE ne change PAS la math d'echantillonnage (sortie identique au legacy).
SAMPLE_CONTROLS <- TRUE   # FALSE = run full sample on a bigger machine (no control subsampling)

# Bornes & hyperparametres dCDH communs (fenetre 2008-2023 : base pre-2014 pour
# les placebos, onset 2014, escalade 2022-2023). VERBATIM des scripts 11*.
YR_MIN <- 2008L; YR_MAX <- 2023L; N_CTRL <- 4000L; EFF <- 4L; PLA <- 2L

# Sorties co-localisees dans 09_dcdh/{tables,figures} (out_* routent par PART).
PATH_TAB <- out_tab("EventStudy")
PATH_FIG <- out_fig("EventStudy")


# ===== [from 11_intensity_dcdh.R] ============================================
# Build panel pkey + dCDH principal (dose en paliers).

# ---- Donnees : panel paire non ordonnee (pkey) -----------------------------
log_step("Chargement iv_panel + construction panel pkey.")
d <- read_parquet_safe(PATH_IV_PANEL)
d[, pkey := ifelse(exp_iso3 < imp_iso3,
                   paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]

# Fenetre : 2008-2023. Couvre une base pre-2014 (6 ans, pour les placebos),
# la vague d'onset 2014, et l'escalade 2022-2023. Restreindre la fenetre est
# rendu necessaire par les 8 Go de RAM (dCDH alloue des matrices ~ N).
# (YR_MIN / YR_MAX definis en tete.)

# Agregation a la paire non ordonnee : commerce = somme des deux directions ;
# dose symetrique -> max (identique dans les deux directions par construction).
pk <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot     = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core),
  n_active_all  = max(sanc_n_active_all)),
  by = .(pkey, year)]
pk[, log_trade := log(trade_tot + 1)]

# Paliers d'intensite (cale sur la distribution : median 1, p90 3, p95 4, p99 5)
pk[, tier := fcase(n_active_core == 0L, 0L,
                   n_active_core == 1L, 1L,
                   n_active_core <= 5L, 2L,
                   default = 3L)]

# CONTRAINTE MEMOIRE (8 Go) : dCDH alloue des matrices ~ nb de groupes (26 565
# paires -> OOM). On garde TOUTES les paires jamais-traitees ? non : on garde
# toutes les paires EVER-traitees (dose>0 au moins une annee) + un ECHANTILLON
# ALEATOIRE de paires jamais-traitees comme controles. L'echantillonnage des
# controles laisse l'ATE non biaise (juste moins precis) ; documente.
# -> Sous SAMPLE_CONTROLS=TRUE : comportement legacy exact. FALSE : sample complet.
if (SAMPLE_CONTROLS) {
  ever_treated <- pk[tier > 0L, unique(pkey)]
  never_treated <- setdiff(unique(pk$pkey), ever_treated)
  set.seed(1234)
  ctrl_keep <- sample(never_treated, min(N_CTRL, length(never_treated)))
  pk <- pk[pkey %in% c(ever_treated, ctrl_keep)]
} else {
  # Machine a plus de RAM : aucun sous-echantillonnage, on garde tout.
  ever_treated <- pk[tier > 0L, unique(pkey)]
  ctrl_keep    <- setdiff(unique(pk$pkey), ever_treated)
}
pk[, gid := .GRP, by = pkey]  # id numerique pour dCDH
cat("  - paires ever-traitees :", length(ever_treated),
    "| controles jamais-traites gardes :", length(ctrl_keep),
    "| total groupes :", uniqueN(pk$pkey), "\n")
cat("  - pkey-year obs :", nrow(pk), "\n")
cat("  - repartition paliers (year<=2023) :\n"); print(pk[, .N, by = tier][order(tier)])

# ---- dCDH principal : dose en paliers --------------------------------------
log_step("dCDH (paliers) : effects=4, placebo=2. Peut etre long (single-thread).")
set.seed(1234)
m_tier <- did_multiplegt_dyn(
  df = pk, outcome = "log_trade", group = "gid", time = "year",
  treatment = "tier", effects = 4, placebo = 2,
  cluster = "gid", graph_off = TRUE)  # gid <-> pkey 1:1 => cluster par paire

saveRDS(m_tier, file.path(PATH_TAB, "_dcdh_tier_obj.rds"))
log_step("dCDH paliers termine. Structure de l'objet :")
str(m_tier, max.level = 2)
cat("\n=== results$ATE / Effects / Placebos ===\n")
print(m_tier$results$Effects)
print(m_tier$results$Placebos)
print(m_tier$results$ATE)


# ===== [from 11c_dcdh_by_tier.R] =============================================
# BLOC A : lecture de l'effet PAR PALIER d'intensite.
#   (1) NORMALISE (normalized=TRUE) : effet par unite de dose (par cran de palier).
#   (2) dCDH BINAIRE par seuil 1{core >= s} : effet de FRANCHIR le palier s.
#       - s=1 : entree sous sanction core (~2014, onset).
#       - s=2 : palier intermediaire.
#       - s=6 : escalade lourde -> survient en 2022 pour les dyades Russie-Occident.
# Memes conventions que 11 : logs log(trade+1), groupe = paire non ordonnee,
# temps = year, cluster = paire (gid numerique), fenetre 2008-2023.

log_step("Chargement iv_panel + panel pkey (window 2008-2023).")
d <- read_parquet_safe(PATH_IV_PANEL)
d[, pkey := ifelse(exp_iso3 < imp_iso3, paste(exp_iso3, imp_iso3, sep = "_"),
                   paste(imp_iso3, exp_iso3, sep = "_"))]
pk0 <- d[year >= YR_MIN & year <= YR_MAX, .(
  trade_tot = sum(trade_value, na.rm = TRUE),
  n_active_core = max(sanc_n_active_core)), by = .(pkey, year)]
pk0[, log_trade := log(trade_tot + 1)]
pk0[, tier := fcase(n_active_core == 0L, 0L, n_active_core == 1L, 1L,
                    n_active_core <= 5L, 2L, default = 3L)]
rm(d); gc(verbose = FALSE)

# Sous-echantillonnage des controles relatif a un indicateur de traitement :
# garde toutes les paires "ever-traitees" (treat>0 une annee) + N_CTRL paires
# jamais-traitees tirees au sort. gid numerique recree dans le sous-echantillon.
# -> Sous SAMPLE_CONTROLS=FALSE : garde tout (aucun tirage), gid recree pareil.
make_sample <- function(dt, treatvar) {
  ever  <- dt[get(treatvar) > 0L, unique(pkey)]
  never <- setdiff(unique(dt$pkey), ever)
  if (SAMPLE_CONTROLS) {
    set.seed(1234)
    keep <- c(ever, sample(never, min(N_CTRL, length(never))))
  } else {
    keep <- c(ever, never)
  }
  s <- dt[pkey %in% keep]
  s[, gid := .GRP, by = pkey]
  list(s = s, n_ever = length(ever), n_ctrl = length(keep) - length(ever))
}

# Extrait placebos + effets + ATE d'un objet dCDH en table longue tidy.
tidy_dcdh <- function(m, label) {
  ef <- as.data.table(m$results$Effects,  keep.rownames = "term")
  pl <- as.data.table(m$results$Placebos, keep.rownames = "term")
  at <- as.data.table(m$results$ATE,      keep.rownames = "term")
  fix <- function(x){ setnames(x, c("LB CI","UB CI"), c("lb","ub"), skip_absent = TRUE); x }
  ef <- fix(ef); pl <- fix(pl); at <- fix(at)
  ef[, rel := as.integer(sub("Effect_","",term))]
  pl[, rel := -as.integer(sub("Placebo_","",term))]
  out <- rbind(
    pl[, .(model = label, term, rel, estimate = Estimate, se = SE, lb, ub)],
    data.table(model = label, term = "Ref_0", rel = 0L, estimate = 0, se = NA, lb = NA, ub = NA),
    ef[, .(model = label, term, rel, estimate = Estimate, se = SE, lb, ub)],
    at[, .(model = label, term = "ATE", rel = NA_integer_, estimate = Estimate, se = SE, lb, ub)],
    use.names = TRUE)
  out[]
}

results <- list()

# ---- (1) NORMALISE : effet par unite de dose (palier) ----------------------
log_step("(1) dCDH NORMALISE (per-dose). Patienter.")
smp <- make_sample(pk0, "tier")
cat("    groupes:", uniqueN(smp$s$pkey), "(ever", smp$n_ever, "+ ctrl", smp$n_ctrl, ")\n")
m_norm <- did_multiplegt_dyn(df = smp$s, outcome = "log_trade", group = "gid",
  time = "year", treatment = "tier", effects = 4, placebo = 2,
  normalized = TRUE, cluster = "gid", graph_off = TRUE)
results[["normalized_per_tier"]] <- tidy_dcdh(m_norm, "normalized_per_tier")
saveRDS(m_norm, file.path(PATH_TAB, "_dcdh_normalized_obj.rds")); rm(m_norm, smp); gc(verbose = FALSE)

# ---- (2) BINAIRE par seuil 1{core >= s} ------------------------------------
for (s in c(1L, 2L, 6L)) {
  lab <- sprintf("cross_ge%d", s)
  log_step(sprintf("(2) dCDH BINAIRE seuil core>=%d (%s). Patienter.", s, lab))
  pk0[, dthr := as.integer(n_active_core >= s)]
  smp <- make_sample(pk0, "dthr")
  cat("    groupes:", uniqueN(smp$s$pkey), "(ever", smp$n_ever, "+ ctrl", smp$n_ctrl, ")\n")
  m <- did_multiplegt_dyn(df = smp$s, outcome = "log_trade", group = "gid",
    time = "year", treatment = "dthr", effects = 4, placebo = 2,
    cluster = "gid", graph_off = TRUE)
  results[[lab]] <- tidy_dcdh(m, lab)
  rm(m, smp); gc(verbose = FALSE)
}

# ---- Sauvegarde table longue ------------------------------------------------
by_tier <- rbindlist(results, use.names = TRUE)
fwrite(by_tier, file.path(PATH_TAB, "tab_dcdh_by_tier.csv"))
log_step("Ecrit tab_dcdh_by_tier.csv. Apercu :")
print(by_tier[term != "Ref_0", .(model, term, rel,
      estimate = round(estimate,4), se = round(se,4),
      lb = round(lb,4), ub = round(ub,4))])

# ---- Figure : contraste onset (core>=1, ~2014) vs escalade lourde (>=6, 2022)
es <- by_tier[model %in% c("cross_ge1","cross_ge6") & term != "ATE"]
es[, Palier := factor(fifelse(model=="cross_ge1",
        "Onset : core >= 1 (~2014)", "Escalade lourde : core >= 6 (2022 Russie-Occ.)"),
        levels = c("Onset : core >= 1 (~2014)", "Escalade lourde : core >= 6 (2022 Russie-Occ.)"))]
p <- ggplot(es, aes(rel, estimate, color = Palier, fill = Palier)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = 0.5, lty = 3, color = "grey60") +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.12, color = NA, na.rm = TRUE) +
  geom_line(linewidth = 0.6, na.rm = TRUE) + geom_point(size = 2.4, na.rm = TRUE) +
  scale_x_continuous(breaks = min(es$rel):max(es$rel)) +
  scale_color_manual(values = c("#2166AC","#B2182B")) +
  scale_fill_manual(values = c("#2166AC","#B2182B")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=10, color="grey40"),
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA)) +
  labs(title = "Effet des sanctions PAR PALIER d'intensite (dCDH binaire par seuil)",
       subtitle = "log(trade+1). Cluster paire. rel<0 = placebos (pre-tendances). k=0 = annee du franchissement.",
       x = "Temps relatif au franchissement du palier (annees)",
       y = "Effet sur log(commerce)",
       caption = "de Chaisemartin & D'Haultfoeuille. Controles jamais-traites echantillonnes (8 Go). Seuil 6+ : 147 paires.")
ggsave(file.path(PATH_FIG, "es_fig02_dcdh_tiers.png"), p, width = 10, height = 6, dpi = 300)
log_step("Ecrit es_fig02_dcdh_tiers.png")


# ===== [from 11b_dcdh_outputs.R] =============================================
# Post-traitement de l'objet dCDH principal (script 11) : table event-study +
# figure, sans relancer l'estimation. Relit l'objet sauvegarde par le bloc 11.

m <- readRDS(file.path(PATH_TAB, "_dcdh_tier_obj.rds"))
eff <- as.data.table(m$results$Effects,   keep.rownames = "term")
pla <- as.data.table(m$results$Placebos,  keep.rownames = "term")
ate <- as.data.table(m$results$ATE,       keep.rownames = "term")
setnames(eff, c("LB CI","UB CI"), c("lb","ub")); setnames(pla, c("LB CI","UB CI"), c("lb","ub"))
setnames(ate, c("LB CI","UB CI"), c("lb","ub"))

eff[, rel := as.integer(sub("Effect_","",term))]
pla[, rel := -as.integer(sub("Placebo_","",term))]
ref <- data.table(term="Ref_0", Estimate=0, SE=NA, lb=NA, ub=NA, rel=0)

es <- rbind(pla[, .(term, rel, Estimate, SE, lb, ub)],
            ref[, .(term, rel, Estimate, SE, lb, ub)],
            eff[, .(term, rel, Estimate, SE, lb, ub)])[order(rel)]
fwrite(es,  file.path(PATH_TAB, "tab_dcdh_eventstudy.csv"))
fwrite(ate, file.path(PATH_TAB, "tab_dcdh_ate.csv"))
cat("=== event-study table ===\n"); print(es)
cat("\n=== ATE ===\n"); print(ate)

p <- ggplot(es, aes(rel, Estimate)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = 0.5, lty = 3, color = "grey60") +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.15, color = "#B2182B", na.rm = TRUE) +
  geom_line(color = "#B2182B", linewidth = 0.5, na.rm = TRUE) +
  geom_point(size = 2.4, color = "#B2182B", na.rm = TRUE) +
  scale_x_continuous(breaks = min(es$rel):max(es$rel)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(size=10, color="grey40"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA)) +
  labs(title = "Intensite des sanctions (dose en paliers) et commerce bilateral",
       subtitle = "de Chaisemartin & D'Haultfoeuille. log(trade+1). Cluster paire. Placebos = pre-tendances.",
       x = "Temps relatif au 1er changement de palier (annees)",
       y = "Effet sur log(commerce)",
       caption = "Dose = sanc_n_active_core en paliers 0/1/2-5/6+. Controles jamais-traites echantillonnes (8 Go RAM).")
ggsave(file.path(PATH_FIG, "es_fig02_dcdh_intensity.png"), p, width = 10, height = 6, dpi = 300)
cat("\nFigure ecrite : es_fig02_dcdh_intensity.png\n")


# ===== [from 11d_robustness.R] ===============================================
# BLOC C : robustesses de l'analyse d'intensite (dCDH).
# (i)  Dose alternative n_senders_target (nb de senders sanctionnant la cible) :
#      triangulation -- raconte-t-elle la meme histoire que le nb de cases ?
# (ii) Dose CONTINUE sanc_n_active_core (brute, non paliers) : montrer ce que les
#      paliers corrigent.
# (iii) Lecture PAR TYPE : quel canal porte l'escalade ? (decomposition du compte
#      de cases core par type, annee par annee, cible Russie.)
# Memes conventions/contraintes que 11/11c (logs, pkey, cluster paire, fenetre
# 2008-2023, seed 1234).

# ---- Panel pkey de base -----------------------------------------------------
log_step("Panel pkey + iv_panel.")
d <- read_parquet_safe(PATH_IV_PANEL)
d[, pkey := ifelse(exp_iso3<imp_iso3, paste(exp_iso3,imp_iso3,sep="_"), paste(imp_iso3,exp_iso3,sep="_"))]
pk0 <- d[year>=YR_MIN & year<=YR_MAX, .(
  trade_tot=sum(trade_value,na.rm=TRUE), n_active_core=max(sanc_n_active_core),
  exp_iso3=exp_iso3[1], imp_iso3=imp_iso3[1]), by=.(pkey, year)]
pk0[, log_trade := log(trade_tot+1)]

# ---- (i) n_senders_target depuis GSDB brut ---------------------------------
log_step("(i) Construction n_senders_target (GSDB brut).")
master_iso3 <- sort(unique(c(d$exp_iso3, d$imp_iso3)))
g <- read_dta_safe(file.path(PATH_IV, "gsdb_v4", "GSDB_V4_dyadic.dta"))
g <- g[year>=1995 & year<=2023 & sanctioning_state_iso3 %in% master_iso3 & sanctioned_state_iso3 %in% master_iso3]
nst <- g[, .(n_send = uniqueN(sanctioning_state_iso3)), by=.(iso=sanctioned_state_iso3, year)]
# pour une paire non ordonnee : prendre le partenaire le PLUS sanctionne
pk0 <- merge(pk0, nst[, .(exp_iso3=iso, year, ns_a=n_send)], by=c("exp_iso3","year"), all.x=TRUE)
pk0 <- merge(pk0, nst[, .(imp_iso3=iso, year, ns_b=n_send)], by=c("imp_iso3","year"), all.x=TRUE)
pk0[is.na(ns_a), ns_a:=0L]; pk0[is.na(ns_b), ns_b:=0L]
pk0[, n_senders := pmax(ns_a, ns_b)]
cat("  Russie : n_senders_target par annee (verif 40->46->48) :\n")
print(unique(pk0[exp_iso3=="RUS" | imp_iso3=="RUS", .(year, n=pmax(ns_a,ns_b))])[order(year)][n>0][, .(n=max(n)), by=year])
rm(d, g); gc(verbose=FALSE)

# Helper sampling + dCDH (literaux pour effects/placebo : NSE du package)
# -> Sous SAMPLE_CONTROLS=FALSE : garde tout (aucun tirage).
make_sample <- function(dt, treatvar){
  ever<-dt[get(treatvar)>0,unique(pkey)]; never<-setdiff(unique(dt$pkey),ever)
  if (SAMPLE_CONTROLS) {
    set.seed(1234); keep<-c(ever,sample(never,min(N_CTRL,length(never))))
  } else {
    keep<-c(ever,never)
  }
  s<-dt[pkey %in% keep]; s[,gid:=.GRP,by=pkey]; list(s=s,n_ever=length(ever))
}
tidy <- function(m,label){
  f<-function(x){setnames(x,c("LB CI","UB CI"),c("lb","ub"),skip_absent=TRUE);x}
  ef<-f(as.data.table(m$results$Effects,keep.rownames="term")); ef[,rel:=as.integer(sub("Effect_","",term))]
  pl<-f(as.data.table(m$results$Placebos,keep.rownames="term")); pl[,rel:=-as.integer(sub("Placebo_","",term))]
  at<-f(as.data.table(m$results$ATE,keep.rownames="term"))
  rbind(pl[,.(model=label,term,rel,estimate=Estimate,se=SE,lb,ub)],
        ef[,.(model=label,term,rel,estimate=Estimate,se=SE,lb,ub)],
        at[,.(model=label,term="ATE",rel=NA_integer_,estimate=Estimate,se=SE,lb,ub)],use.names=TRUE)
}
res <- list()

# (i) n_senders en binaire "large coalition" (>=20 senders) -> capte-t-il 2022 ?
log_step("(i) dCDH n_senders >= 20 (large coalition). Patienter.")
pk0[, d_send20 := as.integer(n_senders>=20L)]
smp<-make_sample(pk0,"d_send20"); cat("  groupes:",uniqueN(smp$s$pkey),"ever",smp$n_ever,"\n")
m<-did_multiplegt_dyn(df=smp$s,outcome="log_trade",group="gid",time="year",treatment="d_send20",effects=4,placebo=2,cluster="gid",graph_off=TRUE)
res[["senders_ge20"]]<-tidy(m,"senders_ge20"); rm(m,smp); gc(verbose=FALSE)

# (ii) dose CONTINUE (n_active_core brut)
log_step("(ii) dCDH dose continue n_active_core. Patienter.")
smp<-make_sample(pk0,"n_active_core"); cat("  groupes:",uniqueN(smp$s$pkey),"ever",smp$n_ever,"\n")
m<-did_multiplegt_dyn(df=smp$s,outcome="log_trade",group="gid",time="year",treatment="n_active_core",effects=4,placebo=2,cluster="gid",graph_off=TRUE)
res[["core_continuous"]]<-tidy(m,"core_continuous"); rm(m,smp); gc(verbose=FALSE)

robC <- rbindlist(res, use.names=TRUE)
fwrite(robC, file.path(PATH_TAB,"tab_dcdh_robustness.csv"))
log_step("Ecrit tab_dcdh_robustness.csv :"); print(robC[,.(model,term,rel,estimate=round(estimate,4),se=round(se,4),lb=round(lb,4),ub=round(ub,4))])

# ---- (iii) Lecture PAR TYPE : decomposition du compte de cases core ---------
log_step("(iii) Decomposition par type du compte de cases (cible Russie).")
g2 <- read_dta_safe(file.path(PATH_IV, "gsdb_v4", "GSDB_V4_dyadic.dta"))
g2 <- g2[sanctioned_state_iso3=="RUS" & year>=2008 & year<=2023]
g2[, dtr := fifelse(is.na(descr_trade),"",descr_trade)]
# explose case_id -> compte cases distincts actifs par annee et par type present
gex <- g2[, .(case_atomic=trimws(unlist(strsplit(case_id,",")))), by=.(year, arms,military,financial,travel,trade,dtr)]
by_type <- g2[, .(
  n_arms=uniqueN(case_id[arms==1]), n_military=uniqueN(case_id[military==1]),
  n_financial=uniqueN(case_id[financial==1]), n_travel=uniqueN(case_id[travel==1]),
  n_trade=uniqueN(case_id[trade==1]),
  n_trade_compl=uniqueN(case_id[grepl("compl",dtr)]),
  n_trade_part=uniqueN(case_id[grepl("part",dtr)])), by=year][order(year)]
fwrite(by_type, file.path(PATH_TAB,"tab_russia_cases_by_type.csv"))
cat("  Russie : nb de cases actifs par type et par annee :\n"); print(by_type)
log_step("Termine bloc C.")


# ===== §4.2 dist_lag_het / DistLagHet (SKELETON — a implementer) =============
# -----------------------------------------------------------------------------
## TODO (feuille de route §4.2)
#
# OBJECTIF. Raffiner le §4.1 avec l'estimateur "distributed-lag heterogeneous"
# de de Chaisemartin & D'Haultfoeuille. Ou le dCDH dynamique date l'effet au
# 1er changement de dose, dist_lag_het SEPARE l'effet CONTEMPORAIN (beta0, effet
# de la variation de dose de l'annee t) des effets RETARDES (beta1, beta2, ... :
# variations de dose des annees t-1, t-2, ...). C'est exactement la question
# "combien de l'impact 2022 vient du choc 2022 vs de l'accumulation depuis 2014".
#
# ARGUMENT (Theoreme 3, de Chaisemartin & D'Haultfoeuille). Une regression
# distributed-lag TWFE naive (Y sur D_t, D_{t-1}, ... avec EF) est CONTAMINEE
# sous heterogeneite des effets : chaque coefficient de lag melange des effets
# d'autres lags avec des poids potentiellement negatifs. dist_lag_het corrige
# en construisant des estimateurs robustes a l'heterogeneite, lag par lag.
#
# MISE EN OEUVRE (a coder ; aucune sortie / aucun chiffre fabrique ici) :
#   1. Installation :
#        remotes::install_github("chaisemartinpackages/dist_lag_het")
#   2. Travailler en PREMIERES DIFFERENCES : Delta Y = Y_t - Y_{t-1} (donc
#      Delta log_trade) et Delta D = D_t - D_{t-1} (variation de dose, p.ex.
#      Delta tier ou Delta n_active_core), sur le panel pkey du §4.1.
#   3. Variantes a estimer :
#        - "base"            : beta0 + quelques lags, specification minimale.
#        - "interactions"    : interactions (heterogeneite par covariables).
#        - "full_dynamics"   : profil dynamique complet (tous les lags retenus).
#   4. Inference : erreurs-standard par BOOTSTRAP (clustered au niveau paire pkey).
#   5. Reporter : beta0 (contemporain) vs beta1, beta2, ... (retardes), + IC
#      bootstrap, et comparer au profil event-study du §4.1.
#
# STUB (commente — le fichier doit parser ; AUCUNE analyse executee) :
#
# library(DistLagHet)                                  # nom du package R installe
# dlh_panel <- copy(pk)                                # panel pkey du §4.1
# setorder(dlh_panel, pkey, year)
# dlh_panel[, dY := log_trade - shift(log_trade), by = pkey]   # Delta Y
# dlh_panel[, dD := tier      - shift(tier),      by = pkey]   # Delta D (dose)
# m_dlh <- dist_lag_het(
#   data       = dlh_panel,
#   delta_y    = "dY",
#   delta_d    = "dD",
#   group      = "gid",
#   time       = "year",
#   n_lags     = 2,                  # beta0 + beta1 + beta2
#   variant    = "full_dynamics",    # ou "base" / "interactions"
#   bootstrap  = TRUE,
#   cluster    = "gid")
# # saveRDS(m_dlh, file.path(PATH_TAB, "_dist_lag_het_obj.rds"))
# # print(m_dlh)   # beta0 (contemporain) vs beta1, beta2 (retardes) + IC bootstrap
#
# FIN SKELETON §4.2.
# =============================================================================
