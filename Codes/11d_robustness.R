# =============================================================================
# 11d_robustness.R  -- BLOC C : robustesses de l'analyse d'intensite (dCDH)
# -----------------------------------------------------------------------------
# (i)  Dose alternative n_senders_target (nb de senders sanctionnant la cible) :
#      triangulation -- raconte-t-elle la meme histoire que le nb de cases ?
# (ii) Dose CONTINUE sanc_n_active_core (brute, non paliers) : montrer ce que les
#      paliers corrigent.
# (iii) Lecture PAR TYPE : quel canal porte l'escalade ? (decomposition du compte
#      de cases core par type, annee par annee, cible Russie.)
#
# Memes conventions/contraintes que 11/11c (logs, pkey, cluster paire, fenetre
# 2008-2023, controles echantillonnes 8 Go, seed 1234).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(haven); library(DIDmultiplegtDYN)
})
PATH_ROOT <- "/Users/zoe/Library/CloudStorage/OneDrive-UniversitéParis-Dauphine/Master_thesis"
PATH_TAB  <- file.path(PATH_ROOT, "Output", "Tables", "EventStudy")
log_step  <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), m))
read_parquet_safe <- function(p,...){t<-tempfile(fileext=".parquet");stopifnot(file.copy(p,t,overwrite=TRUE));on.exit(unlink(t));as.data.table(arrow::read_parquet(t,...))}
read_dta_safe     <- function(p,...){t<-tempfile(fileext=".dta");stopifnot(file.copy(p,t,overwrite=TRUE));on.exit(unlink(t));as.data.table(haven::read_dta(t,...))}

YR_MIN<-2008L; YR_MAX<-2023L; N_CTRL<-4000L

# ---- Panel pkey de base -----------------------------------------------------
log_step("Panel pkey + iv_panel.")
d <- read_parquet_safe(file.path(PATH_ROOT,"Data/Clean/iv_panel.parquet"))
d[, pkey := ifelse(exp_iso3<imp_iso3, paste(exp_iso3,imp_iso3,sep="_"), paste(imp_iso3,exp_iso3,sep="_"))]
pk0 <- d[year>=YR_MIN & year<=YR_MAX, .(
  trade_tot=sum(trade_value,na.rm=TRUE), n_active_core=max(sanc_n_active_core),
  exp_iso3=exp_iso3[1], imp_iso3=imp_iso3[1]), by=.(pkey, year)]
pk0[, log_trade := log(trade_tot+1)]

# ---- (i) n_senders_target depuis GSDB brut ---------------------------------
log_step("(i) Construction n_senders_target (GSDB brut).")
master_iso3 <- sort(unique(c(d$exp_iso3, d$imp_iso3)))
g <- read_dta_safe(file.path(PATH_ROOT,"Data/Raw/IV/gsdb_v4/GSDB_V4_dyadic.dta"))
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
make_sample <- function(dt, treatvar){
  ever<-dt[get(treatvar)>0,unique(pkey)]; never<-setdiff(unique(dt$pkey),ever)
  set.seed(1234); keep<-c(ever,sample(never,min(N_CTRL,length(never))))
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
g2 <- read_dta_safe(file.path(PATH_ROOT,"Data/Raw/IV/gsdb_v4/GSDB_V4_dyadic.dta"))
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
