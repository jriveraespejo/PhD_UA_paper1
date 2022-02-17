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
source( file.path( getwd(), 'sim_code', '1_0_beta_sim_extra.R') )
# source( file.path( getwd(), 'sim_code', '1_3_beta_sim_run.R') ) # run only once




# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
# read simulation
data_nam = "Hbeta_sim1"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )

par_true = with( data_storage$data_true, c(0, par$mu_a, par$s_a, par$a, Ht$SI, Ht$Ht) )
data_true = with(data_storage$data_list, data.frame(H=H, child=cid))


## centered ####
model_nam = "Hbeta_C_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','a_i','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery_C
# not so good samples for a, mu_a, but good for sigma_a, 
# a_i's are also not so good

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 86.9% true parameters inside CI

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for a_i[32] (the most extreme)


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=NULL)


# triplot
tri_plot(stan_object=res_C, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res_C, pars=c('a'))
tri_plot(stan_object=res_C, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )


# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=10)





## non-centered ####
model_nam = "Hbeta_NC_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','a_i','SI','Ht')
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery_NC
# better samples for a and mu_a, worst for sigma_a 
# a_i's better than centered model

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 100% true parameters inside CI

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for a_i[32] (the most extreme)
# better samples for this model (a bit more precise)


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=NULL)


# triplot
tri_plot(stan_object=res_NC, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res_NC, pars='a')
tri_plot(stan_object=res_NC, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )


# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=10)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','mu_a','sigma_a') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )






# simulation 2: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   positive (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# read simulation
data_nam = "Hbeta_sim2"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )

par_true = with( data_storage$data_true,
                 c( #par$aE * c(1:4), # four groups
                   0, par$par$aHS * c(1:3), # three groups
                   par$par$bP, par$par$bA, par$par$mu_a, par$par$s_a,
                   par$a, Ht$SI, Ht$Ht) )

diff_true = data_storage$data_true$par$par$aHS * c(1:2,1)

data_true = with(data_storage$data_list, data.frame(H=H, child=cid))


## approximate effects ####
# # (E + PTA -> HS)
# require(nnet)
# 
# # data mom
# data_test = data_storage$data_true$Ht[,c('E','PTA','HS')]
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
model_nam = "Hbeta_C_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','a_i','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery_C
# poor samples for a, mu_a, sigma_a and a_i, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for SI, Ht (no matter E, HS, or both in model)

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 62.5% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for a_i[5]
# not extreme however it estimates it completely different



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts come good,
#   even with I=32
#   because we break multicollinearity

# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)


# triplot
tri_plot(stan_object=res_C, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_C, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )


# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=10)





## non-centered ####
model_nam = "Hbeta_NC_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','a_i','SI','Ht')
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery_NC
# great samples for all parameters
# great samples for SI, Ht

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 63.5% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for a_i[5]
# not extreme however it estimates it completely different



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery_NC
# when use only HS, contrasts come good, even with I=32
#   because we break multicollinearity
# when use E and HS, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)


# triplot
tri_plot(stan_object=res_NC, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res_NC, pars=c( 'a',paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_NC, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )


# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=10)


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA','mu_a','sigma_a') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )





 
# simulation 3: ####
#
# details:
# Model: 2 types
# Outcome: complex generation different M, zero/one values
# Covariates:
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   dSI/dA > 0 (more A, more SI)
# HS -> SI:
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H]
#   E severity: N=none, L=low, M=mid, H=high
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# read simulation
data_nam = "Hbeta_sim3"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )

par_true = with( data_storage$data_true,
                 c( #par$aE * c(1:4), # four groups
                   0, par$par$aHS * c(1:3), # three groups
                   par$par$bP, par$par$bA, 
                   par$par$mu_a, par$par$s_a,
                   par$par$mu_the, par$par$s_the,
                   par$a, Ht$M, Ht$SI, Ht$Ht) )

diff_true = data_storage$data_true$par$par$aHS * c(1:2,1)

data_true = with(data_storage$data_list, data.frame(H=H, child=cid))


## centered ####
model_nam = "Hbeta_C_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery_C
# poor samples for a, somo aHS, mu_a, sigma_a, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for bP, bA, mu_the, sigma_the, M, SI, Ht 
#   (no matter E, HS, or both in model)

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 73.9% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for M[27]
# not extreme however it estimates it too far




# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts come good (slightly down biased)
#   even with I=32
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)


# triplot
tri_plot(stan_object=res_C, pars=c('mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_C, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=NULL)




## non-centered ####
model_nam = "Hbeta_NC_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery_NC
# better samples for all parameters (except sigma_a)
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 73.9% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for M[27]
# not extreme however it estimates it completely different




# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts come good (slightly downward biased)
#   even with I=32
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)



# triplot
tri_plot(stan_object=res_NC, pars=c('mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_NC, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA','mu_a',
                                                  'sigma_a','mu_the','sigma_the') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )








# simulation 4: ####
#
# details:
# Model: 2 types
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
# read simulation
data_nam = "Hbeta_sim4"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )

data_true = with(data_storage$data_list, data.frame(H=H, child=cid))


## centered ####
model_nam = "Hbeta_C_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                   est_par = par_est,
                                   true_par = rep(NA, 133) ) # no true par
par_recovery_C
# no good samples for a, mu_a, sigma_a, mu_the, or sigma_the,
# no good samples for a_i's or M's
# NO NEED to test CI


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=NULL)


# triplot
tri_plot(stan_object=res_C, pars=c('a','mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res_C, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=NULL)




## non-centered ####
model_nam = "Hbeta_NC_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                   est_par = par_est,
                                   true_par = rep(0, 133) ) # no true par
par_recovery_NC
# great samples for a, mu_a, sigma_a, mu_the, or sigma_the,
# great samples for a's or M's
# NO NEED to test CI


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=NULL)


# triplot
tri_plot(stan_object=res_NC, pars=c('a','mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res_NC, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','mu_a','sigma_a',
                                                  'mu_the','sigma_the') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )




# simulation 5: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation different M, zero/one values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   dSI/dA > 0 (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# A * HS -> SI: 
#   dSI/dA[HS=NH] = dSI/dA[HI/CI] = dSI/dA[HS=HI/HA] = 0 
#   (no different evolution)
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# read simulation
data_nam = "Hbeta_sim5"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )

par_true = with( data_storage$data_true,
                 c( #par$aE * c(1:4), # four groups
                   0, par$par$aHS * c(1:3), # three groups
                   par$par$bP, rep(par$par$bA, 3),
                   par$par$mu_a, par$par$s_a,
                   par$par$mu_the, par$par$s_the,
                   #1, 0 , 0, 1,
                   par$a, Ht$M, Ht$SI, Ht$Ht) )

diff_true = c( data_storage$data_true$par$par$aHS * c(1:2,1), rep(0, 3) ) 

data_true = with(data_storage$data_list, data.frame(H=H, child=cid))


## centered no cor ####
model_nam = "Hbeta_C_sim5_nocor"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bAHS','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true)
par_recovery_C
# poor samples for almost all parameter
#   worst samples for the same, when E and HS are in the model
# good samples for some M, SI, Ht 
#   (no matter E, HS, or both in model)

sum(par_recovery_C$in_CI)/nrow(par_recovery_C)
# 76.4% true parameters inside CI (depends on reference)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for M[27]
# not extreme however it estimates it too far




# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = c('aHS','bAHS'),
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts come out not good (upward biased)
#   even with I=32
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, cont_object=cont_recovery_C)


# triplot
tri_plot(stan_object=res_C, pars=c('mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP'))
tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_C, true_data=data_true, 
           par_object=par_recovery_C, M=NULL)




## non-centered no cor ####
model_nam = "Hbeta_NC_sim5_nocor"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bAHS','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true)
par_recovery_NC
# better samples for all parameters (except sigma_a)
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

sum(par_recovery_NC$in_CI)/nrow(par_recovery_NC)
# 75.7% true parameters inside CI (depends on reference)

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for M[27]
# not extreme however it estimates it completely different




# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = c('aHS', 'bAHS'),
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts come out not good (upward biased)
#   even with I=32
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, cont_object=cont_recovery_NC)



# triplot
tri_plot(stan_object=res_NC, pars=c('mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP'))
tri_plot(stan_object=res_C, pars=paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_NC, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )



# distributional plots
dist_plot( stan_object=res_NC, true_data=data_true, 
           par_object=par_recovery_NC, M=NULL)



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, 
          pars=c('a','aHS','bP','bAHS','mu_a','sigma_a','mu_the','sigma_the') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )

