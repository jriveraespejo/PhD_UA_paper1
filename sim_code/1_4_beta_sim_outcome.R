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
source( file.path( getwd(), 'sim_code', '0_beta_sim_extra.R') )
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


## centered ####
model_nam = "Hbeta_C_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','a_i','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery
# not so good samples for a, mu_a, but good for sigma_a, 
# a_i's are also not so good

sum(par_recovery$in_CI)/nrow(par_recovery)
# 86.9% true parameters inside CI

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for a_i[32] (the most extreme)


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=NULL)


# triplot
tri_plot(stan_object=res, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res, pars=c('a'))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )





## non-centered ####
model_nam = "Hbeta_NC_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','a_i','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery
# better samples for a and mu_a, worst for sigma_a 
# a_i's better than centered model

sum(par_recovery$in_CI)/nrow(par_recovery)
# 100% true parameters inside CI

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for a_i[32] (the most extreme)
# better samples for this model (a bit more precise)


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=NULL)


# triplot
tri_plot(stan_object=res, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res, pars=c('a'))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )




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
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','a_i','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery
# poor samples for a, mu_a, sigma_a and a_i, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for SI, Ht (no matter E, HS, or both in model)

sum(par_recovery$in_CI)/nrow(par_recovery)
# 62.5% true parameters inside CI (depends on reference)

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for a_i[5]
# not extreme however it estimates it completely different



# contrast comparison
cont_recovery = contrast_recovery(stan_object = res,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery
# when use only HS in model, contrasts come good,
#   even with I=32
#   because we break multicollinearity

# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=cont_recovery)


# triplot
tri_plot(stan_object=res, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )





## non-centered ####
model_nam = "Hbeta_NC_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','a_i','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery
# great samples for all parameters
# great samples for SI, Ht

sum(par_recovery$in_CI)/nrow(par_recovery)
# 63.5% true parameters inside CI (depends on reference)

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for a_i[5]
# not extreme however it estimates it completely different



# contrast comparison
cont_recovery = contrast_recovery(stan_object = res,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery
# when use only HS, contrasts come good, even with I=32
#   because we break multicollinearity
# when use E and HS, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=cont_recovery)


# triplot
tri_plot(stan_object=res, pars=c('mu_a','sigma_a'))
tri_plot(stan_object=res, pars=c( 'a',paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )




 
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


## centered ####
model_nam = "Hbeta_C_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery
# poor samples for a, somo aHS, mu_a, sigma_a, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for bP, bA, mu_the, sigma_the, M, SI, Ht 
#   (no matter E, HS, or both in model)

sum(par_recovery$in_CI)/nrow(par_recovery)
# 73.9% true parameters inside CI (depends on reference)

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for M[27]
# not extreme however it estimates it too far




# contrast comparison
cont_recovery = contrast_recovery(stan_object = res,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery
# when use only HS in model, contrasts come good (slightly down biased)
#   even with I=32
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=cont_recovery)


# triplot
tri_plot(stan_object=res, pars=c('mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )




## non-centered ####
model_nam = "Hbeta_NC_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = par_true)
par_recovery
# better samples for all parameters (except sigma_a)
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

sum(par_recovery$in_CI)/nrow(par_recovery)
# 73.9% true parameters inside CI (depends on reference)

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for M[27]
# not extreme however it estimates it completely different




# contrast comparison
cont_recovery = contrast_recovery(stan_object = res,
                                  est_diff = 'aHS',
                                  true_diff = diff_true)
cont_recovery
# when use only HS in model, contrasts come good (slightly downward biased)
#   even with I=32
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=cont_recovery)



# triplot
tri_plot(stan_object=res, pars=c('mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )








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



## centered ####
model_nam = "Hbeta_C_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = rep(0, 133) ) # no true par
par_recovery
# no good samples for a, mu_a, sigma_a, mu_the, or sigma_the,
# no good samples for a_i's or M's
# NO NEED to test CI


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=NULL)


# triplot
tri_plot(stan_object=res, pars=c('a','mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )




## non-centered ####
model_nam = "Hbeta_NC_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','mu_a','sigma_a','mu_the','sigma_the','a_i','M','SI','Ht')
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = par_est,
                                   true_par = rep(0, 133) ) # no true par
par_recovery
# great samples for a, mu_a, sigma_a, mu_the, or sigma_the,
# great samples for a's or M's
# NO NEED to test CI


# recovery plot
recovery_plots(par_object=par_recovery, cont_object=NULL)


# triplot
tri_plot(stan_object=res, pars=c('a','mu_a','sigma_a','mu_the','sigma_the'))
tri_plot(stan_object=res, pars=paste0('a_i[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res, pars=paste0('Ht[', 1:5,']') )




# distribution Ht
post = extract.samples(res)

set.seed(12345)
index = sample(x=1:4000, size=100, replace=F)
Ht_mom = post$Ht[index,]
M_mom = post$M[index,]

Ht_mean = colMeans(post$Ht)
M_mean = colMeans(post$M)

index2 = row.names(compare_r) %in% paste0('Ht[',1:32,']')


par(mfrow=c(6,6))

# i=1
for(i in 1:ncol(Ht_mom) ){ # children

  # first sample
  curve( dbeta2(x, Ht_mom[1,i], M_mom[1,i]), from=0, to=1, ylim=c(0, 6),
         xlab='Entropy', ylab='Density', col='gray')

  # rest of samples
  for(s in 2:nrow(Ht_mom) ){
    curve( dbeta2(x, Ht_mom[s,i], M_mom[s,i]), from=0, to=1,
           xlab='Entropy', ylab='Density', col='gray', add=T)
  }

  # mean distribution
  curve( dbeta2(x, Ht_mean[i], M_mean[i]), from=0, to=1,
         xlab='Entropy', ylab='Density', col='black', lwd=2.5, add=T)
  points( H$value[H$child==i], rep(0, 10), pch=19, col='blue')
  points( compare_r$mean[index2][i], 0, pch=19, col='red')

}

par(mfrow=c(1,1))
