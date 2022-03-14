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
# source( file.path( getwd(), 'sim_code', '3_3_CJD_sim_run.R') ) # run only once




# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_HJ low and equal for all children 
# Covariates: None
#
## data ####
data_nam = "CJD_sim1"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
data_plots(d=mom, xdata='A', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='PTA', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='HS', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='E', ydata='CJD', alpha=0.35)
# notice wrong relationship (spurious)



## parameters ####
par_est = c('a','m_i','s_i','m_j','s_j','s_SI','re_i','re_j','SI')
par_true = data_detect_par(d=mom, par_int=par_est)
data_true = with(mom$dL, data.frame(CJD=CJD, cid1=cid1, cid2=cid2, uid=uid, jid=jid))




## centered ####
model_nam = "CJD_C_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# samples below 300 for all par, except for m_j, some re_j and SI

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# still 93.3% true parameters inside CI

sum(par_recovery_C$diff_0)/nrow(par_recovery_C)
# 25.3% of power

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for re_i[13] (the most extreme)
# correct sign, underestimated magnitude
# with(mom$dS$dT, which( abs(re_i) == max( abs(re_i) ) ) )


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=NULL)
# it recovers well the parameters, but not the s_SI and re_j


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_C, pars=c('s_SI'))
tri_plot(stan_object=res_C, pars=c('a'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
# no convergence for s_i, SI
# bad mixing for all except reg par
# no lack of autocorrelation, except for m_j, s_j, reg par, re_j
# one chain does not work (sometimes)


# # distributional plots
# distHJ_plot( stan_object=res_C, true_data=data_true,
#            par_object=par_recovery_C, M=10)





## non-centered ####
model_nam = "CJD_NC_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# improved samples for all parameters (some above 300), except s_SI
# but they are still not good

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# still 92.7% true parameters inside CI

sum(par_recovery_NC$diff_0)/nrow(par_recovery_NC)
# 25.3% of power

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for re_i[30] (NOT the most extreme)
# correct sign, way underestimated
# with(mom$dS$dT, which( abs(re_i) == max( abs(re_i) ) ) )


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=NULL)
# even better job retrieving true data, except s_SI and some re_j


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_NC, pars=c('s_SI'))
tri_plot(stan_object=res_NC, pars=c('a'))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
# good convergence for all, except for m_j, s_j
# bad mixing for s_i, s_SI, SI
# lack of autocorrelation only for m_i, m_j, s_j, re_j


# # distributional plots
# disHJ_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=10)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_j','s_j') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_j' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
# not a clear winner, except for re_j 





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
data_nam = "CJD_sim2"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
data_plots(d=mom, xdata='A', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='PTA', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='HS', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='E', ydata='CJD', alpha=0.35)
# notice we see relationships



## parameters ####
par_est = c('aHS','a','bP','bA','m_i','s_i','m_j','s_j','s_SI','re_i','re_j','SI')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = mom$dS$par$aHS * c(1:2,1)
data_true = with(mom$dL, data.frame(CJD=CJD, child=cid1))



# # approximate effects ####
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
model_nam = "CJD_C_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# samples below 300 for b_P, b_A, m_i, s_i, s_SI, re_i, some re_j
# the others are not great either

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# still 93.6% true parameters inside CI (depends on reference)

sum(par_recovery_C$diff_0)/nrow(par_recovery_C)
# 31.6% of power

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for SI[2] (NOT the extreme)
# correct sign, but way underestimated magnitude
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# contrast severely downward biased
# first and second cannot reject contrast=0 


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)
# manages to capture the parameters, issues with s_SI, re_i, SI


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_C, pars=c('s_SI'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
# good convergence for all, except SI
# bad mixing for m_i, s_i, s_SI, re_i, SI
# lack of autocorrelation only m_j, s_j, reg par, re_j
# one chain does work


# # distributional plots
# disHJ_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=10)





## non-centered ####
model_nam = "CJD_NC_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# samples below 300 for all except, aHS, a, s_j, some re_j

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 92.3% true parameters inside CI (depends on reference)

sum(par_recovery_NC$diff_0)/nrow(par_recovery_NC)
# 33.5% of power

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for SI[22] (NOT the extreme)  
# correct sign, but way underestimated magnitude
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# contrast way downward biased
# none can reject contrast=0 


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)
# still good recovery, but same issue with s_I, re_i, SI


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_NC, pars=c('s_SI'))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
# good convergence on all, except for s_SI, SI
# bad mixing, except for reg pars, re_i
# lack of autocorrelation only for m_j, s_j, reg par, re_i, re_j
# one chain does not work


# # distributional plots
# disHJ_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=10)


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_j','s_j') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_j' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
# similar n_eff and Rhat for both, BAD







# simulation 2: RE ####
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
data_nam = "CJD_sim2"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
data_plots(d=mom, xdata='A', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='PTA', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='HS', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='E', ydata='CJD', alpha=0.35)
# notice we see relationships



## parameters ####
par_est = c('aHS','a','bP','bA','m_i','s_i','m_j','s_j','s_SI','re_i','re_j','m_SI')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = mom$dS$par$aHS * c(1:2,1)
data_true = with(mom$dL, data.frame(CJD=CJD, child=cid1))



## centered ####
model_nam = "CJD_C_sim2_re"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# samples below 300 for m_i, m_j, re_i, re_j

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 97.4% true parameters inside CI (depends on reference)

sum(par_recovery_C$diff_0)/nrow(par_recovery_C)
# 16.2% of power

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for m_SI[22] (NOT the extreme)
# correct sign, but way underestimated magnitude
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# contrast way downward biased
# none can reject contrast=0 

# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)
# manages to capture the parameters, except s_SI, SI, contrasts


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_C, pars=c('s_SI'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('m_SI[', 1:5,']') )
# good convergence for all, except m_i, m_j, re_i, re_j
# bad mixing for all, except reg par
# no lack of autocorrelation, except s_i, s_j, reg par, m_SI


# # distributional plots
# disHJ_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=10)





## non-centered ####
model_nam = "CJD_NC_sim2_re"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# great samples for all parameters except s_j (around 600)

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 98.1% true parameters inside CI (depends on reference)

sum(par_recovery_NC$diff_0)/nrow(par_recovery_NC)
# 14.2% of power

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for m_SI[22] (NOT the extreme) 
# correct sign, but largely underestimated magnitude
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# contrast way downward biased
# none can reject contrast=0 


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)
# still good recovery, but issues with s_SI, SI, contrasts


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_NC, pars=c('s_SI'))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('m_SI[', 1:5,']') )
# goof convergence on all parameters 
# good mixing for all
# lack of autocorrelation for all


# # distributional plots
# disHJ_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=10)


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_j','s_j') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_j' )
stat_plot(par_recovery_C, par_recovery_NC, pars='m_SI' )
# way better for non-centered
# way better than the models in simulation 2


# RESULT ####
# this is the go to model









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
data_nam = "CJD_sim3"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
data_plots(d=mom, xdata='A', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='PTA', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='HS', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='E', ydata='CJD', alpha=0.35)
# notice the relationships


## parameters ####
par_est = c('aHS','a','bP','bA','m_i','s_i','m_j','s_j','r','s_SI','re_i','re_j','SI')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = mom$dS$par$aHS * c(1:2,1)
data_true = with(mom$dL, data.frame(CJD=CJD, child=cid1))



## centered ####
model_nam = "CJD_C_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# samples below 300 for m_i, re_i, some s_SI, re_j and SI

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 83.9% true parameters inside CI (depends on reference)

sum(par_recovery_C$diff_0)/nrow(par_recovery_C)
# 37.4% of power

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for r 
# correct sign, way overestimated
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )


# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# most contrasts are upward biased
# third can reject contrast=0 


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)
# good recovery for all and contrast, but not SI and re_i


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_C, pars=c('r', paste0('s_SI[',1:4,']')))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
# good convergence for all, except r, SI
# good mixing, except for re_i, SI
# lack of autocorrelation for s_i, m_j, s_j, s_SI, reg par, re_j
# one chain does not work



# # distributional plots
# disHJ_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=NULL)




## non-centered ####
model_nam = "CJD_NC_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# samples above 300

sum(pa_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 80.8% true parameters inside CI (depends on reference)

sum(par_recovery_NC$diff_0)/nrow(par_recovery_NC)
# 37.4% of powers

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for r (it should be 3)
# way underestimated



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# all contrasts are slightly upward biased
# third can reject contrast=0 


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)
# good recovery, r is way underestimated and SI are wrong


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_NC, pars=c('r', paste0('s_SI[',1:4,']')))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
# good convergence for all, except r
# improved mixing, except for r, SI
# lack of autocorrelation only for m_i, s_i, m_j, s_j, s_SI, reg par, re_i, re_j



# # distributional plots
# disHJ_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_j','s_j') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('r','s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_j' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
# mostly better for non-centered









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
data_nam = "CJD_sim4"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
data_plots(d=mom, xdata='A', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='PTA', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='HS', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='E', ydata='CJD', alpha=0.35)
# notice relationships



## parameters ####
par_est = c('aHS','bAHS','a','bP','m_i','s_i','m_j','s_j','r','s_SI','re_i','re_j','SI')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = with(mom$dS$par, c( aHS * c(1:2,1), bAHS*c(1:2,1)) )
data_true = with(mom$dL, data.frame(CJD=CJD, child=cid1))




## centered no cor ####
model_nam = "CJD_C_sim4_nocor"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# samples below 300 for re_i

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 84.1% true parameters inside CI (depends on reference)

sum(par_recovery_C$diff_0)/nrow(par_recovery_C)
# 33.9% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for r
# correct sign, overestimated
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = c('aHS','bAHS'),
                                    true_diff = diff_true)
cont_recovery_C
# wrong contrasts, except fro the third


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)
# still good recovery, except for SI


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_C, pars=c('r', paste0('s_SI[',1:4,']')))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP'))
tri_plot(stan_object=res_C, pars=c( paste0('bAHS[',1:3,']')))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
# good convergence for all, except re_i, SI
# good mixing for all, except m_i, SI
# lack of autocorrelation for s_i, m_j, s_j, s_SI, reg par, re_j



# # distributional plots
# disHJ_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=NULL)




## non-centered no cor ####
model_nam = "CJD_NC_sim4_nocor"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# samples above 300 for all

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 83.1% true parameters inside CI (depends on reference)

sum(par_recovery_NC$diff_0)/nrow(par_recovery_NC)
# 33.9% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for r (should be 3, per simulation)
# way too underestimated
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = c('aHS', 'bAHS'),
                                     true_diff = diff_true)
cont_recovery_NC
# biased contrasts, 
# almost none can reject contrast=0


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)
# except for r, re_i, and SI, all works


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_j','s_j'))
tri_plot(stan_object=res_NC, pars=c('r', paste0('s_SI[',1:4,']')))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP'))
tri_plot(stan_object=res_NC, pars=c( paste0('bAHS[',1:3,']')))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_j[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
# good convergence for all, except r, SI
# most have good mixing, except m_j, r, re_j and SI 
# lack of autocorrelation for all except m_j, s_j, r, re_j and SI



# # distributional plots
# disHJ_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_j','s_j') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('r','s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bAHS') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_j' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
# better for centered








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
data_nam = "CJD_sim2"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
data_plots(d=mom, xdata='A', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='PTA', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='HS', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='E', ydata='CJD', alpha=0.35)
# relationships are more diluted


## parameters ####
par_est = c('aHS','a','bP','bA','m_i','s_i','s_SI','re_i','SI')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = mom$dS$par$aHS * c(1:2,1)
data_true = with(mom$dL, data.frame(CJD=CJD, child=cid1))





## centered ####
model_nam = "CJD_C_sim5"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# samples below 300 for s_i, s_SI

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 52.1% true parameters inside CI (depends on reference)

sum(par_recovery_C$diff_0)/nrow(par_recovery_C)
# 27.4% of power

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for SI[22] (NOT the extreme)
# correct sign, way underestimated magnitude
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI)) ) )




# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# contrast way underestimated, 
# none can reject contrast=0


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)
# it captures the patterns, but not the scales


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','s_SI'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
# tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
# good convergence, except for m_i, s_i and s_SI
# good mixing, except for m_i, s_i, s_SI and re_i
# lack of autocorrelation only for reg par and SI



# # distributional plots
# disHJ_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=NULL)





## non-centered no cor ####
model_nam = "CJD_NC_sim5"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# samples below 300 for almost all

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 53.4% true parameters inside CI (depends on reference)

sum(par_recovery_NC$diff_0)/nrow(par_recovery_NC)
# 28.8% of power

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for SI[22] (NOT the extreme)
# correct sign, way underestimated
# with(mom$dS$dT, which( abs(m_SI) == max( abs(m_SI) ) ) )




# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# way downward biased contrast
# none can reject contrast 0


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)
# still good recovery of SI, but not m_i, s_SI, re_i, and contrasts


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','s_SI'))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
# tri_plot(stan_object=res_NC, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
# good convergence for all, except s_i, s_SI 
# bad mixing for all
# lack of autocorrelation only for m_i, reg par, re_i




# # distributional plots
# disHJ_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
# in general non-centered is worst







# simulation 5: RE ####
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
data_nam = "CJD_sim2"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


## plotting data ####
data_plots(d=mom, xdata='A', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='PTA', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='HS', ydata='CJD', alpha=0.35)
data_plots(d=mom, xdata='E', ydata='CJD', alpha=0.35)
# relationships are more diluted


## parameters ####
par_est = c('aHS','a','bP','bA','m_i','s_i','s_SI','re_i','m_SI')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = mom$dS$par$aHS * c(1:2,1)
data_true = with(mom$dL, data.frame(CJD=CJD, child=cid1))



## centered ####
model_nam = "CJD_C_sim5_re"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# all samples above 400

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 72.6% true parameters inside CI (depends on reference)

sum(par_recovery_C$diff_0)/nrow(par_recovery_C)
# 23.3% of power

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for m_Si[22] (NOT one of the extremes)
# correct sign, way underestimated magnitude
# with(mom$dS$dT, which( abs(re_i) == max( abs(re_i) ) ) )



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# contrast way underestimated, 
# none can reject contrast=0


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)
# still good recovery, except for s_SI, m_SI and contrasts


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','s_SI'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
# tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('m_SI[', 1:5,']') )
# good convergence, except for m_i
# bad mixing, except for reg par
# lack of autocorrelation only s_i, s_SI, reg par



# # distributional plots
# disHJ_plot( stan_object=res_C, true_data=data_true, 
#            par_object=par_recovery_C, M=NULL)




## non-centered no cor ####
model_nam = "CJD_NC_sim5_re"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# all samples above 3000

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 60.3% true parameters inside CI (depends on reference)

sum(par_recovery_NC$diff_0)/nrow(par_recovery_NC)
# 19.2% of power

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for m_SI[22] (NOT one of the extremes)
# correct sign, way underestimated effect
# with(mom$dS$dT, which( re_i == max(re_i) ) )



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# way downward biased
# none can reject contrast=0


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)
# still good recovery but for the pattern, not the scale


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','s_SI'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
# tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('re_i[',1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('m_SI[',1:5,']') )
# good convergence, except for m_i, s_SI
# good mixing, except for m_i, s_SI, re_i, m_SI
# lack of autocorrelation for s_i, s_SI, reg par




# # distributional plots
# disHJ_plot( stan_object=res_NC, true_data=data_true, 
#            par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','s_SI') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='m_SI' )
# in general non-centered is way better


# RESULT ####
# this is the go to model
