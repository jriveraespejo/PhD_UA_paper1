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
# source( file.path( getwd(), 'sim_code', '1_3_beta_sim_run.R') )
# # run only once




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

true_par = with( data_storage$data_true, c(par$mu_a, par$s_a, par$a, Ht$SI, Ht$Ht) )



## centered ####
model_nam = "Hbeta_C_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = c('mu_a','sigma_a','a','SI','Ht'),
                                   true_par = true_par)
par_recovery
# good samples for mu_a and sigma_a, 
# a's are  also good

sum(par_recovery$in_CI)/nrow(par_recovery)
# 100% true parameters inside CI

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for child 32 (the most extreme)





## non-centered ####
model_nam = "Hbeta_NC_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = c('mu_a','sigma_a','a','SI','Ht'),
                                   true_par = true_par)
par_recovery
# worst samples for mu_a and sigma_a 
# a's slightly better than model

sum(par_recovery$in_CI)/nrow(par_recovery)
# 100% true parameters inside CI

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for child 32 (the most extreme)
# better samples for this model





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

true_pars = with( data_storage$data_true,
                  c( #par$aE * c(1:4), # four groups
                    par$par$aHS * c(1:3), # three groups
                    par$par$bP, par$par$bA, par$par$mu_a, par$par$s_a,
                    par$a, Ht$SI, Ht$Ht) )



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




## centered (to be fixed) ####
model_nam = "Hbeta_C_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = c('aHS','bP','bA','mu_a','sigma_a','a','SI','Ht'),
                                   true_par = 0)
par_recovery
# good samples for mu_a and sigma_a, 
# a's are  also good

sum(par_recovery$in_CI)/nrow(par_recovery)
# 100% true parameters inside CI

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for child 32 (the most extreme)

# poor samples for all parameters, when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model
# good samples for SI, Ht (no matter E, HS, or both in model)
# 54.4% true parameters inside CI (depends on reference)


# # contrast comparison
# post = extract.samples(res)
# # names(post)
# 
# contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
# nam = c()
# 
# # for(p in 1:ncol(post$aE)){
# #   for(q in 1:ncol(post$aE)){
# #     if(q>p){
# #       contrast_par = cbind(contrast_par,
# #                            post$aE[,p] - post$aE[,q] )
# #       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
# #     }
# #   }
# # }
# 
# for(p in 1:ncol(post$aHS)){
#   for(q in 1:ncol(post$aHS)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aHS[,p] - post$aHS[,q] )
#       nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
#     }
#   }
# }
# 
# contrast_par = contrast_par[,-1]
# contrast_par = data.frame(contrast_par)
# names(contrast_par) = nam
# 
# compare2 = precis(contrast_par, depth=2, hist=F)
# compare2$true = with( data_mom$par, 
#                       c( #-par$aE * c(1:3, 1:2, 1),
#                         -par$aHS * c(1:2, 1) ) )
# compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
# compare2
# # when use E and HS in model, contrasts come all wrong, 
# #   because of multicollinearity
# #   it does not matter if you increase I or K
# # when use only HS in model, contrasts come good, 
# #   even with I=32
# #   because we break multicollinearity
# 
# 
# 
# for_contrast = data.frame( 
#   cbind(#post$aE,
#     post$aHS) )
# names(for_contrast) = c( 
#   #paste0('aE[',1:4,']'), 
#   paste0('aHS[',1:3,']') )
# 
# psych::pairs.panels( for_contrast )
# psych::pairs.panels( contrast_par )
# # notice the narrow ridge, when use E and HS 
# #   indication of multicollinearity (makes sense)
# # when use only HS, we also see narrow ridge
# #   which is also indication of multicollinearity (how?)





## non-centered ####
model_nam = "Hbeta_NC_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery = parameter_recovery( stan_object = res,
                                   est_par = c('aHS','bP','bA','mu_a','sigma_a','a','SI','Ht'),
                                   true_par = true_par)
par_recovery
# worst samples for mu_a and sigma_a 
# a's slightly better than model

sum(par_recovery$in_CI)/nrow(par_recovery)
# 100% true parameters inside CI

par_recovery[par_recovery$RMSE==max(par_recovery$RMSE),]
# maximum RMSE is for child 32 (the most extreme)
# better samples for this model

# great samples for all parameters
# great samples for SI, Ht
# 54.3% true parameters inside CI (depends on reference)


# # contrast comparison
# post = extract.samples(res)
# # names(post)
# 
# contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
# nam = c()
# 
# # for(p in 1:ncol(post$aE)){
# #   for(q in 1:ncol(post$aE)){
# #     if(q>p){
# #       contrast_par = cbind(contrast_par,
# #                            post$aE[,p] - post$aE[,q] )
# #       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
# #     }
# #   }
# # }
# 
# for(p in 1:ncol(post$aHS)){
#   for(q in 1:ncol(post$aHS)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aHS[,p] - post$aHS[,q] )
#       nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
#     }
#   }
# }
# 
# contrast_par = contrast_par[,-1]
# contrast_par = data.frame(contrast_par)
# names(contrast_par) = nam
# 
# compare2 = precis(contrast_par, depth=2, hist=F)
# compare2$true = with( data_mom$par, 
#                       c( #-par$aE * c(1:3, 1:2, 1),
#                         -par$aHS * c(1:2, 1) ) )
# compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
# compare2
# # when use E and HS, contrasts come all wrong, 
# #   because of multicollinearity
# #   it does not matter if you increase I or K
# # when use only HS, contrasts come good, even with I=32
# #   because we break multicollinearity
# 
# 
# 
# for_contrast = data.frame( 
#   cbind(#post$aE,
#     post$aHS) )
# names(for_contrast) = c( 
#   #paste0('aE[',1:4,']'), 
#   paste0('aHS[',1:3,']') )
# 
# psych::pairs.panels( for_contrast )
# psych::pairs.panels( contrast_par )
# # notice the narrow ridge, when use E and HS 
# #   indication of multicollinearity (makes sense)
# # when use only HS, we also see narrow ridge
# #   which is also indication of multicollinearity (how?)
















 
# # simulation 3: ####
# # 
# # details:
# # Model: 2 types
# # Outcome: complex generation different M, zero/one values
# # Covariates: 
# # E -> HS:
# #   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
# #   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# # PTA -> HS:
# #   positive
# #   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
# #   PTA range, L=low, M1<M2=mid, H=high
# # A -> SI: 
# #   positive (more A, more SI)
# # HS -> SI: 
# #   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# # E -> SI:
# #   negative (higher E, less SI)
# #   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
# #   E severity: N=none, L=low, M=mid, H=high 
# # PTA -> SI:
# #   negative (more PTA, less SI)
# #
# #   ideally is non-linear
# #   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
# #   PTA range, L=low, M1<M2=mid, H=high
# #
# # generate data
# data_mom = Hsim3() # default 32
# # data_mom = Hsim3(I = 100, prop=c(0.34, 0.33, 0.33) )
# # data_mom = Hsim3(K = 30 )
# Ht = data_mom$Ht
# H = data_mom$H
# 
# 
# # plot data
# plot( H$H, H$child, col='gray', pch=19, xlim=c(0,1), 
#       xlab='entropy', ylab='child ID')
# abline(h = H$child, lty=2, col='gray')
# abline(v = c(0, 1), lty=1, col='gray')
# points( Ht$Ht, 1:nrow(Ht), col='blue', pch=19)
# # notice now we have zeros/ones
# 
# 
# 
# # data list
# dlist = list(
#   N = nrow(H),
#   K = max(H$utterance), # utterances
#   I = nrow(Ht),
#   cHS = max(Ht$HS),
#   cE = max(Ht$E),
#   H = with(H, ifelse(H==0, 0.001, ifelse(H==1, 0.999, H)) ), # trick
#   cid = H$child,
#   HS = Ht$HS,
#   A = with(Ht, A - min(A) ),
#   E = Ht$E,
#   PTA = c( standardize( Ht$PTA ) ) )
# 
# 
# 
# 
# ## centered ####
# # cmdstan
# set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
# model_nam = "Hbeta_C_sim5.stan"
# mod = cmdstan_model( model_out )
# fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# res = rstan::read_stan_csv( fit$output_files() )
# # divergent transitions (between 100-300 of 4000)
# 
# 
# 
# # parameter comparison
# # compare1 = precis( res, depth=2, pars=c('aE','aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )
# compare1 = precis( res, depth=2, pars=c('aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a','M','SI','Ht') )
# 
# true_pars = with(data_mom$par,
#                  c( #par$aE * c(1:dlist$cE), 
#                    par$aHS * c(1:dlist$cHS),
#                    par$bP, par$bA, 
#                    par$mu_a, par$s_a, par$mu_the, par$s_the, 
#                    a, Ht$M, Ht$SI, Ht$Ht) )
# 
# compare1$true = true_pars
# compare1$withinCI = with(compare1, as.integer(true>=`5.5%` & true<=`94.5%`) )
# compare1
# sum(compare1$withinCI)/nrow(compare1)
# # poor samples for all parameters, when only HS is in the model
# #   worst samples for all parameters, when E and HS are in the model
# # good samples for M, SI, Ht (no matter E, HS, or both in model)
# # 65% true parameters inside CI (depends on reference)
# 
# 
# 
# 
# # contrast comparison
# post = extract.samples(res)
# # names(post)
# 
# contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
# nam = c()
# 
# # for(p in 1:ncol(post$aE)){
# #   for(q in 1:ncol(post$aE)){
# #     if(q>p){
# #       contrast_par = cbind(contrast_par,
# #                            post$aE[,p] - post$aE[,q] )
# #       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
# #     }
# #   }
# # }
# 
# for(p in 1:ncol(post$aHS)){
#   for(q in 1:ncol(post$aHS)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aHS[,p] - post$aHS[,q] )
#       nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
#     }
#   }
# }
# 
# contrast_par = contrast_par[,-1]
# contrast_par = data.frame(contrast_par)
# names(contrast_par) = nam
# 
# compare2 = precis(contrast_par, depth=2, hist=F)
# compare2$true = with( data_mom$par, 
#                       c( #-par$aE * c(1:3, 1:2, 1),
#                         -par$aHS * c(1:2, 1) ) )
# compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
# compare2
# # when use E and HS in model, contrasts come all wrong, 
# #   because of multicollinearity
# #   it does not matter if you increase I or K
# # when use only HS in model, contrasts come good, 
# #   even with I=32
# #   because we break multicollinearity
# 
# 
# 
# for_contrast = data.frame( 
#   cbind(#post$aE,
#     post$aHS) )
# names(for_contrast) = c( 
#   #paste0('aE[',1:4,']'), 
#   paste0('aHS[',1:3,']') )
# 
# psych::pairs.panels( for_contrast )
# psych::pairs.panels( contrast_par )
# # notice the narrow ridge, when use E and HS 
# #   indication of multicollinearity (makes sense)
# # when use only HS, we also see narrow ridge
# #   which is also indication of multicollinearity (how?)
# 
# 
# 
# 
# 
# 
# 
# 
# ## non-centered ####
# # cmdstan
# set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
# model_nam = "Hbeta_NC_sim5.stan"
# mod = cmdstan_model( model_out )
# fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# res = rstan::read_stan_csv( fit$output_files() )
# # divergent transitions (between 100-300 of 4000)
# 
# 
# 
# # parameter comparison
# # compare1 = precis( res, depth=2, pars=c('aE','aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )
# compare1 = precis( res, depth=2, pars=c('aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a','M','SI','Ht') )
# 
# true_pars = with(data_mom$par,
#                  c( #par$aE * c(1:dlist$cE), 
#                    par$aHS * c(1:dlist$cHS),
#                    par$bP, par$bA, 
#                    par$mu_a, par$s_a, par$mu_the, par$s_the, 
#                    a, Ht$M, Ht$SI, Ht$Ht) )
# 
# compare1$true = true_pars
# compare1$withinCI = with(compare1, as.integer(true>=`5.5%` & true<=`94.5%`) )
# compare1
# sum(compare1$withinCI)/nrow(compare1)
# # great samples for all parameters, when only HS is in the model
# #   worst samples for all parameters, when E and HS are in the model
# # great samples for M, SI, Ht (no matter E, HS, or both in model)
# # 65% true parameters inside CI (depends on reference)
# 
# 
# 
# 
# # contrast comparison
# post = extract.samples(res)
# # names(post)
# 
# contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
# nam = c()
# 
# # for(p in 1:ncol(post$aE)){
# #   for(q in 1:ncol(post$aE)){
# #     if(q>p){
# #       contrast_par = cbind(contrast_par,
# #                            post$aE[,p] - post$aE[,q] )
# #       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
# #     }
# #   }
# # }
# 
# for(p in 1:ncol(post$aHS)){
#   for(q in 1:ncol(post$aHS)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aHS[,p] - post$aHS[,q] )
#       nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
#     }
#   }
# }
# 
# contrast_par = contrast_par[,-1]
# contrast_par = data.frame(contrast_par)
# names(contrast_par) = nam
# 
# compare2 = precis(contrast_par, depth=2, hist=F)
# compare2$true = with( data_mom$par, 
#                       c( #-par$aE * c(1:3, 1:2, 1),
#                         -par$aHS * c(1:2, 1) ) )
# compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
# compare2
# # when use E and HS in model, contrasts come all wrong, 
# #   because of multicollinearity
# #   it does not matter if you increase I or K
# # when use only HS in model, contrasts come good, 
# #   even with I=32
# #   because we break multicollinearity
# 
# 
# 
# for_contrast = data.frame( 
#   cbind(#post$aE,
#     post$aHS) )
# names(for_contrast) = c( 
#   #paste0('aE[',1:4,']'), 
#   paste0('aHS[',1:3,']') )
# 
# psych::pairs.panels( for_contrast )
# psych::pairs.panels( contrast_par )
# # notice the narrow ridge, when use E and HS 
# #   indication of multicollinearity (makes sense)
# # when use only HS, we also see narrow ridge
# #   which is also indication of multicollinearity (how?)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # simulation 4: ####
# # 
# # details:
# # Model: 2 types
# # Outcome = no known process behind (no known M)
# # Covariates: not modeled
# #
# # generate data
# data_mom = H_sim4()
# H = melt(data_mom$entropy_data, id.vars = 'child')
# H = H[order(H$child),]
# 
# 
# # plot data
# plot( H$value, H$child, pch=19, xlim=c(0,1),
#       xlab='entropy', ylab='child id') 
# abline(h = H$child, lty=2)
# 
# 
# 
# # data list
# dlist = list(
#   N = nrow( H ),
#   K = length( unique( H$variable ) ), # utterances
#   I = max( H$child ),
#   H = with(H, ifelse(value==0, 0.001, ifelse(value==1, 0.999, value)) ),
#   cid = H$child )
# # notice trick to handle zeroes/ones
# 
# 
# 
# ## centered ####
# # cmdstan
# set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
# model_nam = "Hbeta_C_sim7.stan"
# mod = cmdstan_model( model_out )
# fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# res = rstan::read_stan_csv( fit$output_files() )
# 
# 
# # final comparison
# compare_r = precis( res, depth=2, pars=c('mu_a','sigma_a','mu_the','sigma_the','a','M','SI','Ht') )
# compare_r
# # good samples for mu_a and sigma_a, 
# # a's are  also good
# # 100% true parameters inside CI
# # no need non-centered because no covariates
# 
# 
# 
# 
# 
# 
# ## non-centered ####
# # cmdstan
# set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
# model_nam = "Hbeta_NC_sim7.stan"
# mod = cmdstan_model( model_out )
# fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# res = rstan::read_stan_csv( fit$output_files() )
# 
# 
# # final comparison
# compare_r = precis( res, depth=2, pars=c('mu_a','sigma_a','mu_the','sigma_the','a','M','SI','Ht') )
# compare_r
# # good samples for mu_a and sigma_a, 
# # a's are  also good
# # 100% true parameters inside CI
# # no need non-centered because no covariates
# 
# 
# 
# 
# # distribution Ht
# post = extract.samples(res)
# 
# set.seed(12345)
# index = sample(x=1:4000, size=100, replace=F)
# Ht_mom = post$Ht[index,]
# M_mom = post$M[index,]
# 
# Ht_mean = colMeans(post$Ht)
# M_mean = colMeans(post$M)
# 
# index2 = row.names(compare_r) %in% paste0('Ht[',1:32,']')
# 
# 
# par(mfrow=c(6,6))
# 
# # i=1
# for(i in 1:ncol(Ht_mom) ){ # children
#   
#   # first sample
#   curve( dbeta2(x, Ht_mom[1,i], M_mom[1,i]), from=0, to=1, ylim=c(0, 6),
#          xlab='Entropy', ylab='Density', col='gray')
#   
#   # rest of samples
#   for(s in 2:nrow(Ht_mom) ){ 
#     curve( dbeta2(x, Ht_mom[s,i], M_mom[s,i]), from=0, to=1,
#            xlab='Entropy', ylab='Density', col='gray', add=T)
#   }
#   
#   # mean distribution
#   curve( dbeta2(x, Ht_mean[i], M_mean[i]), from=0, to=1,
#          xlab='Entropy', ylab='Density', col='black', lwd=2.5, add=T)
#   points( H$value[H$child==i], rep(0, 10), pch=19, col='blue')
#   points( compare_r$mean[index2][i], 0, pch=19, col='red')
#   
# }
# 
# par(mfrow=c(1,1))
