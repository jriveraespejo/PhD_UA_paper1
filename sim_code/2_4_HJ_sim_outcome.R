# preliminar ####
rm(list=ls())

librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
               'reshape2','tinytex','gt','haven',
               'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
               'rethinking','rstan','coda','runjags','rjags',#'loo'
               'cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)


setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1')



# # loading sources
source( file.path( getwd(), 'sim_code', '0_sim_extra.R') )
# source( file.path( getwd(), 'sim_code', '2_3_HJ_sim_run.R') ) # run only once




# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_HJ low and equal for all children 
# Covariates: None
#
## data ####
data_nam = "HJ_sim1"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
Amin = min(mom$dS$dT$A)
mom_plot = with(mom$dL, data.frame(cid=cid, HS=HS[cid], A=Am[cid]+Amin, 
                                   E=E[cid], sPTA=sPTA[cid], HJ=HJ) )

par(mfrow=c(2,2))
with(mom_plot, plot(A, HJ, pch=19, col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==1,], plot(A, HJ, pch=19, main='HS==1', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==2,], plot(A, HJ, pch=19, main='HS==2', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==3,], plot(A, HJ, pch=19, main='HS==3', col=col.alpha('black', 0.02)) )
par(mfrow=c(1,1))


par(mfrow=c(2,2))
with(mom_plot, plot(sPTA, HJ, pch=19, main='', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==1,], plot(sPTA, HJ, pch=19, main='HS==1', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==2,], plot(sPTA, HJ, pch=19, main='HS==2', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==3,], plot(sPTA, HJ, pch=19, main='HS==3', col=col.alpha('black', 0.02)) )
par(mfrow=c(1,1))


with(mom_plot, plot(HS, HJ, pch=19, xaxt="n", col=col.alpha('black', 0.02)) )
axis(side=1, at=1:3, labels = T)

with(mom_plot, plot(E, HJ, pch=19, xaxt="n", col=col.alpha('black', 0.02)) )
axis(side=1, at=1:4, labels = T)



## parameters ####
re_j = unique(mom$dS$dO[,c('judge_id','re_j')])
re_j = re_j[order(re_j$judge_id),]
par_true = with(mom$dS, c(par$a, par$m_i, par$s_i, par$m_j, par$s_j,
                          unique(dT$s_SI), dT$re_i, re_j$re_j, dT$m_SI) )
data_true = with(mom$dL, data.frame(HJ=HJ, cid=cid, jid=jid))




## centered ####
model_nam = "HJ_C_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','m_i','s_i','m_j','s_j','s_HJ','re_i','re_j','SI')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# really bad samples for all parameters 

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 98% true parameters inside CI

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for re_i[30] (not the most extreme)
# correct on the signs
# with(mom$dS$dT, which( re_i == max( abs(re_i) ) ) )


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=NULL)


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_j','s_j','s_HJ'))
tri_plot(stan_object=res_C, pars=c('a'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )


# # distributional plots
# dist_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=10)





## non-centered ####
model_nam = "HJ_NC_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# better samples for all parameters, except s_i, s_j 

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 98% true parameters inside CI

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for SI[4] (not the most extreme)
# better samples for this model, now has the right direction and is in CI
# with(mom$dS$dT, which( m_SI == max( abs(m_SI) ) ) )


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=NULL)


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_j','s_j','s_HJ'))
tri_plot(stan_object=res_NC, pars=c('a'))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )


# # distributional plots
# dist_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=10)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_j','s_j','s_HJ') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_j' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )






# simulation 2: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_HJ low and equal for all children 
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   positive (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## data ####
data_nam = "HJ_sim2"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
Amin = min(mom$dS$dT$A)
mom_plot = with(mom$dL, data.frame(cid=cid, HS=HS[cid], A=Am[cid]+Amin, 
                                   E=E[cid], sPTA=sPTA[cid], H=H) )

par(mfrow=c(2,2))
with(mom_plot, plot(A, H, pch=19, col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==1,], plot(A, H, pch=19, main='HS==1', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==2,], plot(A, H, pch=19, main='HS==2', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==3,], plot(A, H, pch=19, main='HS==3', col=col.alpha('black', 0.15)) )
par(mfrow=c(1,1))


par(mfrow=c(2,2))
with(mom_plot, plot(sPTA, H, pch=19, main='', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==1,], plot(sPTA, H, pch=19, main='HS==1', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==2,], plot(sPTA, H, pch=19, main='HS==2', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==3,], plot(sPTA, H, pch=19, main='HS==3', col=col.alpha('black', 0.15)) )
par(mfrow=c(1,1))


with(mom_plot, plot(HS, H, pch=19, xaxt="n", col=col.alpha('black', 0.15)) )
axis(side=1, at=1:3, labels = T)

with(mom_plot, plot(E, H, pch=19, xaxt="n", col=col.alpha('black', 0.15)) )
axis(side=1, at=1:4, labels = T)



## parameters ####
par_true = with( mom$dS,
                 c( #par$aE * c(1:4), # four groups
                   par$a, par$aHS * c(1:3), # three groups
                   par$bP, par$bA, par$m_i, par$s_i,
                   dT$re_i, dT$m_SI, dT$m_H) )

diff_true = mom$dS$par$aHS * c(1:2,1)

data_true = with(mom$dL, data.frame(H=H, child=cid))



## approximate effects ####
# # (E + PTA -> HS)
# require(nnet)
# 
# # data mom
# data_test = mom$dS$dT[,c('E','PTA','HS')]
# data_test$HS = factor(data_test$HS)
# data_test$E = factor(data_test$E)
# data_test$PTA = standardize(data_test$PTA)
# 
# # probability, reference HS=NH[1]
# model_test = multinom(HS ~ E + PTA, data=data_test)
# res_test = summary(model_test)
# z <- res_test$coefficients/res_test$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# 
# round(inv_logit(coef(model_test)), 5)
# p
# # intercept -> non-significant
# # E=2 -> less prob of HS=3
# # E=3 -> non significant (Ho: b=1)
# # E=4 -> less prob of HS=2
# # PTA -> non-significant (if you use E)
# 
# 
# # probability, reference HS=NH[1]
# model_test = multinom(HS ~ PTA, data=data_test)
# res_test = summary(model_test)
# z <- res_test$coefficients/res_test$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# 
# round(inv_logit(coef(model_test)), 5)
# p
# # intercept -> non-significant
# # PTA -> significant (if you do not use E)




## centered ####
model_nam = "HJ_C_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','m_i','s_i','re_i','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# poor samples for a, m_i and re_i, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for the rest (no matter E, HS, or both in model)

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 88.5% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for re_i[30] (not the extreme)
# completely different in sign
# with(mom$dS$dT, which( re_i == max( abs(re_i) ) ) )



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts are slightly over/under estimated
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )


# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=10)





## non-centered ####
model_nam = "HJ_NC_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# great samples for all parameters, except s_i
# great samples for re_i, SI, Ht

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 87.5% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for re_i[30] (not the extreme)
# completely different effect
# with(mom$dS$dT, which( re_i == max( abs(re_i) ) ) )



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS, contrasts come slightly over/under estimated
#   even when we break multicollinearity
# when use E and HS, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i'))
tri_plot(stan_object=res_NC, pars=c( 'a',paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )


# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=10)


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )






# simulation 3: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_HJ different for all children
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   dSI/dAm > 0 (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## data ####
data_nam = "HJ_sim3"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
Amin = min(mom$dS$dT$A)
mom_plot = with(mom$dL, data.frame(cid=cid, HS=HS[cid], A=Am[cid]+Amin, 
                                   E=E[cid], sPTA=sPTA[cid], H=H) )

par(mfrow=c(2,2))
with(mom_plot, plot(A, H, pch=19, col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==1,], plot(A, H, pch=19, main='HS==1', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==2,], plot(A, H, pch=19, main='HS==2', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==3,], plot(A, H, pch=19, main='HS==3', col=col.alpha('black', 0.15)) )
par(mfrow=c(1,1))


par(mfrow=c(2,2))
with(mom_plot, plot(sPTA, H, pch=19, main='', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==1,], plot(sPTA, H, pch=19, main='HS==1', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==2,], plot(sPTA, H, pch=19, main='HS==2', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==3,], plot(sPTA, H, pch=19, main='HS==3', col=col.alpha('black', 0.15)) )
par(mfrow=c(1,1))


with(mom_plot, plot(HS, H, pch=19, xaxt="n", col=col.alpha('black', 0.15)) )
axis(side=1, at=1:3, labels = T)

with(mom_plot, plot(E, H, pch=19, xaxt="n", col=col.alpha('black', 0.15)) )
axis(side=1, at=1:4, labels = T)


## parameters ####
par_true = with( mom$dS,
                 c( #par$aE * c(1:4), # four groups
                   par$a, par$aHS*c(1:3), # three groups
                   par$bP, par$bA, 
                   par$m_i, par$s_i, par$m_M, par$s_M,
                   dT$re_i, dT$M, dT$m_SI, dT$m_H) )

diff_true = mom$dS$par$aHS * c(1:2,1)

data_true = with(mom$dL, data.frame(H=H, child=cid))



## centered ####
model_nam = "HJ_C_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','m_i','s_i','m_M','s_M','re_i','M','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# poor samples for a, m_i and re_i, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for all the rest 
#   (no matter E, HS, or both in model)

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 80.4% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for M[6] (NOT one of the two extreme values)
# with(mom$dS$dT, which( M == max( abs(M) ) ) )



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts more over/under estimated
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=NULL)




## non-centered ####
model_nam = "HJ_NC_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# better samples for all parameters
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 80.4% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for M[6] (NOT one of the two extreme)
# with(mom$dS$dT, which( M == max(M) ) )



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts come over/under estimated
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)



# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_M','s_M') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )









# simulation 4: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_HJ different for all children
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   dSI/dAm > 0 (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# Am * HS -> SI: 
#   dSI/dAm[HS=NH] = dSI/dAm[HI/CI] = dSI/dAm[HS=HI/HA] = 0 
#   (no different evolution)
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## data ####
data_nam = "HJ_sim4"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
Amin = min(mom$dS$dT$A)
mom_plot = with(mom$dL, data.frame(cid=cid, HS=HS[cid], A=Am[cid]+Amin, 
                                   E=E[cid], sPTA=sPTA[cid], H=H) )

par(mfrow=c(2,2))
with(mom_plot, plot(A, H, pch=19, col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==1,], plot(A, H, pch=19, main='HS==1', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==2,], plot(A, H, pch=19, main='HS==2', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==3,], plot(A, H, pch=19, main='HS==3', col=col.alpha('black', 0.15)) )
par(mfrow=c(1,1))


par(mfrow=c(2,2))
with(mom_plot, plot(sPTA, H, pch=19, main='', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==1,], plot(sPTA, H, pch=19, main='HS==1', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==2,], plot(sPTA, H, pch=19, main='HS==2', col=col.alpha('black', 0.15)) )
with(mom_plot[mom_plot$HS==3,], plot(sPTA, H, pch=19, main='HS==3', col=col.alpha('black', 0.15)) )
par(mfrow=c(1,1))


with(mom_plot, plot(HS, H, pch=19, xaxt="n", col=col.alpha('black', 0.15)) )
axis(side=1, at=1:3, labels = T)

with(mom_plot, plot(E, H, pch=19, xaxt="n", col=col.alpha('black', 0.15)) )
axis(side=1, at=1:4, labels = T)



## parameters ####
par_true = with( mom$dS,
                 c( #par$aE * c(1:4), # four groups
                   par$a, par$aHS * c(1:3), # three groups
                   par$bP, par$bAHS*c(1:3),
                   par$m_i, par$s_i, par$m_M, par$s_M,
                   #1, 0 , 0, 1,
                   dT$re_i, dT$M, dT$m_SI, dT$m_H) )

diff_true = with(mom$dS$par, c( aHS*c(1:2,1), bAHS*c(1:2,1) ) )

data_true = with(mom$dL, data.frame(H=H, child=cid))


## centered no cor ####
model_nam = "HJ_C_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bAHS','m_i','s_i','m_M','s_M','re_i','M','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# poor samples for a, m_i, s_M, and re_i
#   worst samples for the same, when E and HS are in the model
# good samples for the rest
#   (no matter E, HS, or both in model)

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 81.4% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for M[13] (one of the two extreme)
# with(mom$dS$dT, which( M == max(M) ) )




# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = c('aHS','bAHS'),
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts come out downward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP'))
tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=NULL)




## non-centered no cor ####
model_nam = "HJ_NC_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# great samples for all parameters (except s_M)
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 82.9% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for M[13] (one of the two extreme)
# with(mom$dS$dT, which( M == max(M) ) )




# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = c('aHS', 'bAHS'),
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts come downward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)



# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP'))
tri_plot(stan_object=res_NC, pars=paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_M','s_M') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bAHS') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )








# simulation 5: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_HJ different for all children, reduced data
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   dSI/dAm > 0 (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# Am * HS -> SI: 
#   dSI/dAm[HS=NH] = dSI/dAm[HI/CI] = dSI/dAm[HS=HI/HA] = 0 
#   (no different evolution)
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## data ####
data_nam = "HJ_sim5"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
Amin = min(mom$dS$dT$A)
mom_plot = with(mom$dL, data.frame(cid=rcid, HS=HS[cid], A=Am[cid]+Amin, 
                                   E=E[cid], sPTA=sPTA[cid], m_HJ=m_HJ, s_HJ=s_HJ) )

par(mfrow=c(2,2))
with(mom_plot, plot(A, m_HJ, pch=19, col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==1,], plot(A, m_HJ, pch=19, main='HS==1', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==2,], plot(A, m_HJ, pch=19, main='HS==2', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==3,], plot(A, m_HJ, pch=19, main='HS==3', col=col.alpha('black', 0.02)) )
par(mfrow=c(1,1))


par(mfrow=c(2,2))
with(mom_plot, plot(sPTA, m_HJ, pch=19, main='', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==1,], plot(sPTA, m_HJ, pch=19, main='HS==1', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==2,], plot(sPTA, m_HJ, pch=19, main='HS==2', col=col.alpha('black', 0.02)) )
with(mom_plot[mom_plot$HS==3,], plot(sPTA, m_HJ, pch=19, main='HS==3', col=col.alpha('black', 0.02)) )
par(mfrow=c(1,1))


with(mom_plot, plot(HS, m_HJ, pch=19, xaxt="n", col=col.alpha('black', 0.02)) )
axis(side=1, at=1:3, labels = T)

with(mom_plot, plot(E, m_HJ, pch=19, xaxt="n", col=col.alpha('black', 0.02)) )
axis(side=1, at=1:4, labels = T)



## parameters ####
par_true = with( mom$dS,
                 c( #par$aE * c(1:4), # four groups
                   par$a, par$aHS * c(1:3), # three groups
                   par$bP, par$bA, #par$bAHS*c(1:3),
                   par$m_i, par$s_i, #par$m_j, par$s_j,
                   unique(dT$s_SI), 
                   dT$re_i, #dT$re_j, 
                   dT$m_SI) )

diff_true = with(mom$dS$par, aHS*c(1:2,1)  )

data_true = with(mom$dL, data.frame(m_HJ=m_HJ, s_HJ=s_HJ, cid=rcid))




## centered no cor ####
model_nam = "HJ_C_sim5"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','m_i','s_i','s_SI','re_i','SI')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# good samples for a, aHS, bP, bA, 
# not so good for m_i, s_i, s_SI and re_i
# good samples for the rest

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 93.2% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for re_i[27] (one of the extremes)
# with(mom$dS$dT, which( re_i == max(re_i) ) )




# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts come out downward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','s_SI'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
# tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )



# # distributional plots
# dist_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=NULL)




## non-centered no cor ####
model_nam = "HJ_NC_sim5"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# great samples for everything, except s_i, s_SI and some re_i

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 90.4% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for re_i[27] (one of the extremes)
# with(mom$dS$dT, which( re_i == max(re_i) ) )




# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts come downward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)



# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','s_SI'))
tri_plot(stan_object=res_NC, pars=c( 'a',paste0('aHS[',1:3,']'),'bP','bA'))
# tri_plot(stan_object=res_NC, pars=paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )



# # distributional plots
# dist_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )


