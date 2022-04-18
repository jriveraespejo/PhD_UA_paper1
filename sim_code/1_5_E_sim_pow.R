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



# loading sources
source( file.path( getwd(), 'sim_code', '0_sim_extra.R') )
source( file.path( getwd(), 'sim_code', '1_2_beta_sim_function.R') )






# testing power ####
# run only once
#
Epower(power_save=file.path(getwd(), 'sim_chain'), # file_save need to include getwd()
       sim_name='Hbeta_sim2_power.RData', # file_name need to include '.RData'
       sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
       model_name='Hbeta_NC_sim2', # model for which we want to calculate power
       model_in = file.path(getwd(), 'sim_models'), # location load models
       model_out = file.path(getwd(), 'sim_chain'), # location to save results
       seed=NULL, # seed
       Nsim=100, # number of simulation for power
       I_grid=c(32, 40, 50), # experimental units (children)
       K_grid=c(10, 15, 20), # replicates (utterances)
       par_int=c('aHS','bP','bA','m_i','s_i','m_M','SI'), # parameter to analyze power
       p=c(0.36, 0.32, 0.32) ) # children prop. on each group



# load results
load(file.path( getwd(), 'sim_chain', 'Hbeta_NC_sim2_power.RData'))
# par_res


# plot results
plot_power(d=par_res, exclude='SI', plotN=1)
plot_power(d=par_res, exclude='SI', plotN=2)
plot_power(d=par_res, exclude='SI', plotN=3)


  
