# preliminar ####
rm(list=ls())

librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
               'reshape2','tinytex','gt','haven',
               'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
               'rethinking','rstan','coda','runjags','rjags',#'loo'
               'cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)



# setting paths
setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1')
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it



# loading sources
source( file.path( getwd(), 'sim_code', '1_1_beta_sim_models.R') )
source( file.path( getwd(), 'sim_code', '1_2_beta_sim_function.R') )






# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
# loading data
data_nam = 'Hbeta_sim1.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions


## non-centered ####
model_nam = "Hbeta_NC_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions






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
data_nam = 'Hbeta_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 1-8 of 4000


## non centered ####
model_nam = "Hbeta_NC_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions






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
data_nam = 'Hbeta_sim3.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 200-400 of 4000


## non-centered ####
model_nam = "Hbeta_NC_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions





# simulation 4: ####
# 
# details:
# Model: 2 types
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
data_nam = 'Hbeta_sim4.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim4.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 250-450 of 4000


## non-centered ####
model_nam = "Hbeta_NC_sim4.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=data_storage$data_list, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions
