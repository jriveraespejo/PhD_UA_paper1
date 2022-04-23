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
# source( file.path( getwd(), 'sim_code', '1_3_E_sim_run.R') ) # run only once




# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
## prior ####
model_nam = "E_C_sim1_prior"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_prior = rstan::read_stan_csv( file.path( model_out, model_fit ) )
par_prior = precis(res_prior, depth=2)


# plots
layout(matrix(c(1,2,3,3), 2, 2, byrow=T), 
       widths=c(1,1), heights=c(1,1))
prior_plots(precis_obj=par_prior, var_str='SI', yrange=c(-3,3))
prior_plots(precis_obj=par_prior, var_str='Ht', yrange=c(0,1))
prior_plots(precis_obj=par_prior, var_str='H', yrange=c(0,1))



## data ####
data_nam = "E_sim1"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


# plotting data
data_plots(d=mom, xdata='A', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='PTA', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='HS', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='E', ydata='H', alpha=0.05, os=F)
# no relationship


# parameters 
par_est = c('a','m_i','s_i','re_i','SI','Ht')
par_true = data_detect_par(d=mom, par_int=par_est)
data_true = with(mom$dL, data.frame(H=H, child=cid))



## centered ####
model_nam = "E_C_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true,
                                     p=0.90)
par_recovery_C
# View(par_recovery_C)
# bad samples for a, m_i, re_i, but good for s_i
# SI and Ht are good

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for SI[140] (not the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(SI) == max( abs(SI) ) ) )

sum(par_recovery_C$sign)/nrow(par_recovery_C)
# 93.8% correct sign

sum(par_recovery_C$reject_null)/nrow(par_recovery_C)
# 42.4% reject the null

sum(par_recovery_C$accept_val)/nrow(par_recovery_C)
# 64.9% reject the null

sum(par_recovery_C$precision)/nrow(par_recovery_C)
# 36.3% reject the null



# recovery plot
recovery_plots(par_object=par_recovery_C, 
               cont_object=NULL,
               par_plot=c('a','m_i','s_i','SI'))
# still not bad recovery of SI and Ht


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i'))
tri_plot(stan_object=res_C, pars=c('a'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )
# good convergence only for s_i, re_i, SI, Ht
# good mixing only for s_i, SI, Ht
# lack of autocorrelation only for s_i, SI, Ht


# distributional plots
distH_plot( stan_object=res_C, 
            true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=10)
# well enough capture of the data




## non-centered ####
model_nam = "E_NC_sim1"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true,
                                      p=0.90)
par_recovery_NC
# View(par_recovery_NC)
# great samples for all except s_i (around 1200) 
# re_i and Ht better than centered model

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for SI[140] (not the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(SI) == max( abs(SI) ) ) )

sum(par_recovery_NC$sign)/nrow(par_recovery_NC)
# 94% correct sign

sum(par_recovery_NC$reject_null)/nrow(par_recovery_NC)
# 42.5% reject the null

sum(par_recovery_NC$accept_val)/nrow(par_recovery_NC)
# 65.4% reject the null

sum(par_recovery_NC$precision)/nrow(par_recovery_NC)
# 36.3% reject the null


# recovery plot
recovery_plots(par_object=par_recovery_NC, 
               cont_object=NULL,
               par_plot=c('a','m_i','s_i','SI'))
# still great recovery


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i'))
tri_plot(stan_object=res_NC, pars='a')
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )
# good convergence for all 
# good mixing only for all
# lack of autocorrelation for all


# distributional plots
distH_plot( stan_object=res_NC, 
            true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=10)
# great recovery of data


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i') )
stat_plot(par_recovery_C, par_recovery_NC, pars='a' )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )
# better n_eff and Rhat for all in non-centered (less for s_i)





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
## prior ####
model_nam = "E_C_sim2_prior"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_prior = rstan::read_stan_csv( file.path( model_out, model_fit ) )
par_prior = precis(res_prior, depth=2)


# plots
# pdf("prior_predictive.pdf")
layout(matrix(c(1,2,3,3), 2, 2, byrow=T), 
       widths=c(1,1), heights=c(1,1))
prior_plots(precis_obj=par_prior, var_str='SI', yrange=c(-3,3))
prior_plots(precis_obj=par_prior, var_str='Ht', yrange=c(0,1))
prior_plots(precis_obj=par_prior, var_str='H', yrange=c(0,1))
# dev.off()



## data ####
data_nam = "E_sim2"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


# plotting data
data_plots(d=mom, xdata='A', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='PTA', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='HS', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='E', ydata='H', alpha=0.05, os=F)
# notice now we have relationship



# parameters
par_est = c('a','aHS','bP','bA','m_i','s_i','m_M','re_i','SI','Ht')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = true_contrast(d=mom, par_int=c('aHS'))
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
model_nam = "E_C_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true,
                                     p=0.90)
par_recovery_C
# View(par_recovery_C)
# poor samples for a, aHS, bP, bA, m_i and re_i, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for the rest (no matter E, HS, or both in model)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for SI[140] (not the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(SI) == max( abs(SI) ) ) )

sum(par_recovery_C$sign)/nrow(par_recovery_C)
# 93.4% correct sign

sum(par_recovery_C$reject_null)/nrow(par_recovery_C)
# 46.7% reject the null

sum(par_recovery_C$accept_val)/nrow(par_recovery_C)
# 67.6% reject the null

sum(par_recovery_C$precision)/nrow(par_recovery_C)
# 45.1% reject the null



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object=res_C, 
                                    est_diff = 'aHS', 
                                    true_diff = diff_true, 
                                    p=0.90)
cont_recovery_C
# when use only HS in model, contrasts are slightly overestamated
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, 
               cont_object=cont_recovery_C,
               par_plot=c('a','m_i','s_i','m_M','aHS','aE','bP','bA','SI'))
# not good for random effects


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_M'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )
# good convergence for all except m_i 
# good mixing for all except reg par and re_i
# lack of autocorrelation only for s_i, m_M, reg par, SI, Ht



# distributional plots
distH_plot( stan_object=res_C, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=10)
# captures well the data




## non-centered ####
model_nam = "E_NC_sim2"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true,
                                      p=0.90)
par_recovery_NC
# View(par_recovery_NC)
# great samples for all parameters

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for re_i[140] (not the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(re_i) == max( abs(re_i) ) ) )

sum(par_recovery_NC$sign)/nrow(par_recovery_NC)
# 93.9% correct sign

sum(par_recovery_NC$reject_null)/nrow(par_recovery_NC)
# 47.4% reject the null

sum(par_recovery_NC$accept_val)/nrow(par_recovery_NC)
# 67.8% reject the null

sum(par_recovery_NC$precision)/nrow(par_recovery_NC)
# 45.2% reject the null



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS, contrasts come slightly overestamated
#   even when we break multicollinearity
# when use E and HS, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
# pdf("recovery.pdf")
recovery_plots(par_object=par_recovery_NC, 
               cont_object=cont_recovery_NC,
               par_plot=c('a','m_i','s_i','m_M','aHS','aE','bP','bA','SI'))
# dev.off()
# great recovery of contrasts


# triplot
# pdf("chains1.pdf")
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_M'))
# dev.off()

# pdf("chains2.pdf")
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 
                                     'bP', 'bA' ))
# dev.off()

tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )
# good convergence for all 
# good mixing only for all
# lack of autocorrelation for all



# distributional plots
# pdf("posterior_predictive.pdf")
distH_plot( stan_object=res_NC, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=10)
# dev.off()
# still captures the data


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_M') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )

# pdf("chain_stat.pdf")
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
# dev.off()

stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )
# most parameter have better n_eff and Rhat under non-centered




 
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
## prior ####
model_nam = "E_C_sim3_prior"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_prior = rstan::read_stan_csv( file.path( model_out, model_fit ) )
par_prior = precis(res_prior, depth=2)


# plots
layout(matrix(c(1,2,3,3), 2, 2, byrow=T), 
       widths=c(1,1), heights=c(1,1))
prior_plots(precis_obj=par_prior, var_str='SI', yrange=c(-3,3))
prior_plots(precis_obj=par_prior, var_str='Ht', yrange=c(0,1))
prior_plots(precis_obj=par_prior, var_str='H', yrange=c(0,1))



## data ####
data_nam = "E_sim3"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


# plotting data
data_plots(d=mom, xdata='A', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='PTA', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='HS', ydata='H', alpha=0.05, os=F)
data_plots(d=mom, xdata='E', ydata='H', alpha=0.05, os=F)
# notice relationship


# parameters
par_est = c('a','aHS','bP','bA','m_i','s_i','m_M','s_M','re_i','M','SI','Ht')
par_true = data_detect_par(d=mom, par_int = par_est)
diff_true = true_contrast(d=mom, par_int=c('aHS'))
data_true = with(mom$dL, data.frame(H=H, child=cid))



## centered ####
model_nam = "E_C_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true,
                                     p=0.90)
par_recovery_C
# poor samples for m_i, re_i and some M, when only HS is in the model
#   worst samples for the same, when E and HS are in the model
# good samples for all the rest 
#   (no matter E, HS, or both in model)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for M[241] (the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(M) == max( abs(M) ) ) )

sum(par_recovery_C$sign)/nrow(par_recovery_C)
# 92.9% correct sign

sum(par_recovery_C$reject_null)/nrow(par_recovery_C)
# 54.5% reject the null

sum(par_recovery_C$accept_val)/nrow(par_recovery_C)
# 51.8% reject the null

sum(par_recovery_C$precision)/nrow(par_recovery_C)
# 29.7% reject the null



# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = 'aHS',
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts slightly overestamated
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, 
               cont_object=cont_recovery_C,
               par_plot=c('m_i','s_i','m_M','a','aHS','aE','bP','bA','SI'))
# still good recovery


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_C, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )
# good convergence for all except m_i
# good mixing for all except m_i, reg par, re_i
# lack of autocorrelation for all excep m_i


# distributional plots
distH_plot( stan_object=res_C, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=NULL)
# seems it overfits the data



## non-centered ####
model_nam = "E_NC_sim3"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                   est_par = par_est,
                                   true_par = par_true,
                                   p=0.90)
par_recovery_NC
# better samples for all parameters
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for M[241] (the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(M) == max( abs(M) ) ) )

sum(par_recovery_NC$sign)/nrow(par_recovery_NC)
# 92.9% correct sign

sum(par_recovery_NC$reject_null)/nrow(par_recovery_NC)
# 54.3% reject the null

sum(par_recovery_NC$accept_val)/nrow(par_recovery_NC)
# 51.2% reject the null

sum(par_recovery_NC$precision)/nrow(par_recovery_NC)
# 29.7% reject the null



# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = 'aHS',
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts slightly overestamated
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, 
               cont_object=cont_recovery_NC,
               par_plot=c('m_i','s_i','m_M','a','aHS','aE','bP','bA','SI'))
# still good recovery


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_NC, pars=c( 'a', paste0('aHS[',1:3,']'), 'bP', 'bA' ))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )
# good convergence for all 
# good mixing only for all
# lack of autocorrelation for all



# distributional plots
distH_plot( stan_object=res_NC, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=NULL)
# it might overfit the data


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_M','s_M') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aHS','bP','bA') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )
# slightly better n_eff and Rhat for non-centered







# simulation 4: ####
#
# details:
# Model: 2 types
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
## prior ####
model_nam = "E_C_sim4_prior"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_prior = rstan::read_stan_csv( file.path( model_out, model_fit ) )
par_prior = precis(res_prior, depth=2)


# plots
layout(matrix(c(1,2,3,3), 2, 2, byrow=T), 
       widths=c(1,1), heights=c(1,1))
prior_plots(precis_obj=par_prior, var_str='SI', yrange=c(-3,3))
prior_plots(precis_obj=par_prior, var_str='Ht', yrange=c(0,1))
prior_plots(precis_obj=par_prior, var_str='H', yrange=c(0,1))



## data ####
data_nam = "E_sim4"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


# parameters
data_true = with(mom$dL, data.frame(H=H, child=cid))



## centered ####
model_nam = "E_C_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_est = c('a','m_i','s_i','m_M','s_M','re_i','M','SI','Ht')
par_recovery_C = precis( res_C, prob=0.90, depth=4 ) # no true par
par_recovery_C
# no good samples for a, m_i, s_i
# no good samples for re_i's or M's
# NO NEED to test CI


# # recovery plot
# recovery_plots(par_object=par_recovery_C, cont_object=NULL)


# triplot
tri_plot(stan_object=res_C, pars=c('a','m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )
# good convergence for all except a, m_i,re_i
# good mixing only for all except a, m_i, re_i
# lack of autocorrelation only for m_M, s_M, M, SI, Ht


# # distributional plots
# distH_plot( stan_object=res_C, true_data=data_true, 
#             csize=6, rplot=c(3,2),
#             par_object=par_recovery_C, M=NULL)
# # some children have a more uniform distribution



## non-centered ####
model_nam = "E_NC_sim4"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = precis( res_NC, prob=0.90, depth=4 ) # no true par
par_recovery_NC
# great samples for all 
# NO NEED to test CI


# # recovery plot
# recovery_plots(par_object=par_recovery_NC, cont_object=NULL)


# triplot
tri_plot(stan_object=res_NC, pars=c('a','m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )
# good convergence for all 
# good mixing only for all
# lack of autocorrelation for all


# # distributional plots
# distH_plot( stan_object=res_NC, true_data=data_true, 
#             csize=6, rplot=c(3,2),
#             par_object=par_recovery_C, M=NULL)
# # same as previous


# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','m_i','s_i','m_M','s_M') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )
# mostly better n_eff and Rhat in non-centered







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
#   PTArange, L=low, M1<M2=mid, H=high
#
## prior ####
model_nam = "E_C_sim5_prior"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_prior = rstan::read_stan_csv( file.path( model_out, model_fit ) )
par_prior = precis(res_prior, depth=3)


# plots
layout(matrix(c(1,2,3,3), 2, 2, byrow=T), 
       widths=c(1,1), heights=c(1,1))
prior_plots(precis_obj=par_prior, var_str='SI', yrange=c(-3,3))
prior_plots(precis_obj=par_prior, var_str='Ht', yrange=c(0,1))
prior_plots(precis_obj=par_prior, var_str='H', yrange=c(0,1))




## data ####
data_nam = "E_sim5"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


# plotting data
data_plots(d=mom, xdata='A', ydata='H', alpha=0.15, os=F)
data_plots(d=mom, xdata='PTA', ydata='H', alpha=0.15, os=F)
data_plots(d=mom, xdata='HS', ydata='H', alpha=0.15, os=F)
data_plots(d=mom, xdata='E', ydata='H', alpha=0.15, os=F)
# notice the relationships



# parameters
par_est = c('a','bP','aEHS','bAHS','m_i','s_i','m_M','re_i','SI','Ht')
par_true = data_detect_par(d=mom, par_int=par_est)
par_true[par_true==1.5] = exp(1.5) # is a log normal

diff_true = true_contrast(d=mom, par_int=c('aEHS','bAHS'))
data_true = with(mom$dL, data.frame(H=H, child=cid))



## centered ####
model_nam = "E_C_sim5"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true,
                                     p=0.90)
par_recovery_C
# View(par_recovery_C)
# poor samples for a, aHS, m_i and re_i
#   worst samples for the same, when E and HS are in the model
# good samples for the rest
#   (no matter E, HS, or both in model)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for SI[344] (not the most extreme)
# overestimated
# with(mom$dS$dT, which( abs(SI) == max( abs(SI) ) ) )

sum(par_recovery_C$sign)/nrow(par_recovery_C)
# 89.5% correct sign

sum(par_recovery_C$reject_null)/nrow(par_recovery_C)
# 39.4% reject the null

sum(par_recovery_C$accept_val)/nrow(par_recovery_C)
# 61.5% reject the null

sum(par_recovery_C$precision)/nrow(par_recovery_C)
# 40.6% reject the null




# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = c('aEHS','bAHS'),
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts come out down/up ward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, 
               cont_object=cont_recovery_C,
               par_plot=c('m_i','s_i','m_M','a','bP','aEHS','bAHS','SI'))
# cannot reject contrasts Ho: bX=0


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_M'))
tri_plot(stan_object=res_C, pars=c( 'a','bP') )
tri_plot(stan_object=res_C, pars= paste0('aEHS[',1:4,',',1,']') )
tri_plot(stan_object=res_C, pars= paste0('aEHS[',1:4,',',2,']') )
tri_plot(stan_object=res_C, pars= paste0('aEHS[',1:4,',',3,']') )
tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
# tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )
# good convergence for all except m_i
# good mixing for all except m_i, reg_par, re_i
# lack of autocorrelation only for s_i, m_M, reg par


# distributional plots
distH_plot( stan_object=res_C, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=exp(1.5))
# might overfit the data



## non-centered ####
model_nam = "E_NC_sim5"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true,
                                      p=0.90)
par_recovery_NC
# View(par_recovery_NC)
# great samples for all parameters, except s_M
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for SI[344] (the most extreme)
# overestimated
# with(mom$dS$dT, which( abs(SI) == max( abs(SI) ) ) )

sum(par_recovery_NC$sign)/nrow(par_recovery_NC)
# 88.9% correct sign

sum(par_recovery_NC$reject_null)/nrow(par_recovery_NC)
# 39.4% reject the null

sum(par_recovery_NC$accept_val)/nrow(par_recovery_NC)
# 61.9% reject the null

sum(par_recovery_NC$precision)/nrow(par_recovery_NC)
# 40.6% reject the null




# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = c('aEHS','bAHS'),
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts come down/up ward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, 
               cont_object=cont_recovery_NC,
               par_plot=c('m_i','s_i','m_M','a','bP','aEHS','bAHS','SI'))
# well enough parameters


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_M'))
tri_plot(stan_object=res_NC, pars=c( 'a','bP') )
tri_plot(stan_object=res_NC, pars= paste0('aEHS[',1:4,',',1,']') )
tri_plot(stan_object=res_NC, pars= paste0('aEHS[',1:4,',',2,']') )
tri_plot(stan_object=res_NC, pars= paste0('aEHS[',1:4,',',3,']') )
tri_plot(stan_object=res_NC, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
# tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )
# good convergence for all 
# good mixing only for all
# lack of autocorrelation for all


# distributional plots
distH_plot( stan_object=res_NC, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_NC, M=exp(1.5))
# might overfit the data



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_M') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aEHS','bP','bAHS') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
# stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )
# better n_eff and Rhat mostly under non-centered (except for SI)









# simulation 6: ####
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
#   PTArange, L=low, M1<M2=mid, H=high
#
## prior ####
model_nam = "E_C_sim6_prior"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_prior = rstan::read_stan_csv( file.path( model_out, model_fit ) )
par_prior = precis(res_prior, depth=3)


# plots
layout(matrix(c(1,2,3,3), 2, 2, byrow=T), 
       widths=c(1,1), heights=c(1,1))
prior_plots(precis_obj=par_prior, var_str='SI', yrange=c(-3,3))
prior_plots(precis_obj=par_prior, var_str='Ht', yrange=c(0,1))
prior_plots(precis_obj=par_prior, var_str='H', yrange=c(0,1))




## data ####
data_nam = "E_sim5"
model_data = file.path(getwd(), 'sim_data', paste0( data_nam, '.RData') )
load( model_data )
# mom


# plotting data
data_plots(d=mom, xdata='A', ydata='H', alpha=0.15, os=F)
data_plots(d=mom, xdata='PTA', ydata='H', alpha=0.15, os=F)
data_plots(d=mom, xdata='HS', ydata='H', alpha=0.15, os=F)
data_plots(d=mom, xdata='E', ydata='H', alpha=0.15, os=F)
# notice the relationships



# parameters
par_est = c('a','bP','aEHS','bAHS','m_i','s_i','m_M','s_M','re_i','M','SI','Ht')
par_true = data_detect_par(d=mom, par_int=par_est)
diff_true = true_contrast(d=mom, par_int=c('aEHS','bAHS'))
data_true = with(mom$dL, data.frame(H=H, child=cid))



## centered ####
model_nam = "E_C_sim6"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_C = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_C = parameter_recovery( stan_object = res_C,
                                     est_par = par_est,
                                     true_par = par_true,
                                     p=0.90)
par_recovery_C
# View(par_recovery_C)
# poor samples for a, aHS, m_i and re_i
#   worst samples for the same, when E and HS are in the model
# good samples for the rest
#   (no matter E, HS, or both in model)

par_recovery_C[par_recovery_C$RMSE==max(par_recovery_C$RMSE),]
# maximum RMSE is for M[182] (the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(M) == max( abs(M) ) ) )

sum(par_recovery_C$sign)/nrow(par_recovery_C)
# 92.9% correct sign

sum(par_recovery_C$reject_null)/nrow(par_recovery_C)
# 54.4% reject the null

sum(par_recovery_C$accept_val)/nrow(par_recovery_C)
# 52.6% reject the null

sum(par_recovery_C$precision)/nrow(par_recovery_C)
# 30.8% reject the null




# contrast comparison
cont_recovery_C = contrast_recovery(stan_object = res_C,
                                    est_diff = c('aEHS','bAHS'),
                                    true_diff = diff_true)
cont_recovery_C
# when use only HS in model, contrasts come out down/up ward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_C, 
               cont_object=cont_recovery_C,
               par_plot=c('m_i','s_i','m_M','s_M','a','bP','aEHS','bAHS','SI'))
# cannot reject contrasts Ho: bX=0


# triplot
tri_plot(stan_object=res_C, pars=c('m_i','s_i','m_M','s_M'))
tri_plot(stan_object=res_C, pars=c( 'a','bP') )
tri_plot(stan_object=res_C, pars= paste0('aEHS[',1:4,',',1,']') )
tri_plot(stan_object=res_C, pars= paste0('aEHS[',1:4,',',2,']') )
tri_plot(stan_object=res_C, pars= paste0('aEHS[',1:4,',',3,']') )
tri_plot(stan_object=res_C, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_C, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_C, pars=paste0('Ht[', 1:5,']') )
# good convergence for all except m_i
# good mixing for all except m_i, reg_par, re_i
# lack of autocorrelation only for s_i, m_M, reg par


# distributional plots
distH_plot( stan_object=res_C, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_C, M=NULL)
# might overfit the data



## non-centered ####
model_nam = "E_NC_sim6"
model_out = file.path(getwd(), 'sim_chain')
model_fit = file_id(model_out, model_nam) 
res_NC = rstan::read_stan_csv( file.path( model_out, model_fit ) )


# final comparison
par_recovery_NC = parameter_recovery( stan_object = res_NC,
                                      est_par = par_est,
                                      true_par = par_true,
                                      p=0.90)
par_recovery_NC
# View(par_recovery_NC)
# great samples for all parameters, except s_M
#   when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model

par_recovery_NC[par_recovery_NC$RMSE==max(par_recovery_NC$RMSE),]
# maximum RMSE is for M[182] (the most extreme)
# underestimated
# with(mom$dS$dT, which( abs(M) == max( abs(M) ) ) )

sum(par_recovery_NC$sign)/nrow(par_recovery_NC)
# 92.8% correct sign

sum(par_recovery_NC$reject_null)/nrow(par_recovery_NC)
# 53.9% reject the null

sum(par_recovery_NC$accept_val)/nrow(par_recovery_NC)
# 52.9% reject the null

sum(par_recovery_NC$precision)/nrow(par_recovery_NC)
# 30.7% reject the null




# contrast comparison
cont_recovery_NC = contrast_recovery(stan_object = res_NC,
                                     est_diff = c('aEHS','bAHS'),
                                     true_diff = diff_true)
cont_recovery_NC
# when use only HS in model, contrasts come down/up ward biased
#   because we break multicollinearity
# when use E and HS in model, contrasts come all wrong,
#   because of multicollinearity
#   it does not matter if you increase I or K


# recovery plot
recovery_plots(par_object=par_recovery_NC, 
               cont_object=cont_recovery_NC,
               par_plot=c('m_i','s_i','m_M','s_M','a','bP','aEHS','bAHS','SI'))
# well enough parameters


# triplot
tri_plot(stan_object=res_NC, pars=c('m_i','s_i','m_M'))
tri_plot(stan_object=res_NC, pars=c( 'a','bP') )
tri_plot(stan_object=res_NC, pars= paste0('aEHS[',1:4,',',1,']') )
tri_plot(stan_object=res_NC, pars= paste0('aEHS[',1:4,',',2,']') )
tri_plot(stan_object=res_NC, pars= paste0('aEHS[',1:4,',',3,']') )
tri_plot(stan_object=res_NC, pars= paste0('bAHS[',1:3,']') )
tri_plot(stan_object=res_NC, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('M[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=res_NC, pars=paste0('Ht[', 1:5,']') )
# good convergence for all 
# good mixing only for all
# lack of autocorrelation for all


# distributional plots
distH_plot( stan_object=res_NC, true_data=data_true, 
            csize=6, rplot=c(3,2),
            par_object=par_recovery_NC, M=NULL)
# might overfit the data



# chain stats
stat_plot(par_recovery_C, par_recovery_NC, pars=c('m_i','s_i','m_M','s_M') )
stat_plot(par_recovery_C, par_recovery_NC, pars=c('a','aEHS','bP','bAHS') )
stat_plot(par_recovery_C, par_recovery_NC, pars='re_i' )
stat_plot(par_recovery_C, par_recovery_NC, pars='M' )
stat_plot(par_recovery_C, par_recovery_NC, pars='SI' )
stat_plot(par_recovery_C, par_recovery_NC, pars='Ht' )
# better n_eff and Rhat mostly under non-centered (except for SI)