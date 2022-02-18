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
# simulating and saving data
Esim(file_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     file_name='Hbeta_sim1.RData', # file_name need to include '.RData'
     I=32, K=10, seed=12345,
     p=c(0.38, 0.31, 0.31), # children prop. on each group
     par=list( m_c=0, s_c=1, # children's random effects
               m_M=10, s_M=NULL, # generation of df (M)
               a=0, aE=0, aHS=0, bP=0, bA=0, bAHS=0 ) )


# loading data
data_nam = 'Hbeta_sim1.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions


## non-centered ####
model_nam = "Hbeta_NC_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
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
# simulating and saving data
Esim(file_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     file_name='Hbeta_sim2.RData', # file_name need to include '.RData'
     I=32, K=10, seed=12345,
     p=c(0.38, 0.31, 0.31), # children prop. on each group
     par=list( m_c=0, s_c=0.5, # children's random effects
               m_M=10, s_M=NULL, # generation of df (M)
               a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ) )


# loading data
data_nam = 'Hbeta_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 0-8 of 4000


## non centered ####
model_nam = "Hbeta_NC_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
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
# simulating and saving data
Esim(file_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     file_name='Hbeta_sim3.RData', # file_name need to include '.RData'
     I=32, K=10, seed=12345,
     p=c(0.38, 0.31, 0.31), # children prop. on each group
     par=list( m_c=0, s_c=0.5, # children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ) )


# loading data
data_nam = 'Hbeta_sim3.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 200-400 of 4000


## non-centered ####
model_nam = "Hbeta_NC_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
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
# simulating 
Esim2(file_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
      file_name='Hbeta_sim4.RData', # file_name need to include '.RData'
      children=32, words=10, judges=100, max_occ=50)

# loading data
data_nam = 'Hbeta_sim4.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "Hbeta_C_sim4.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 250-450 of 4000


## non-centered ####
model_nam = "Hbeta_NC_sim4.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions





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
# simulating and saving data
Esim(file_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     file_name='Hbeta_sim5.RData', # file_name need to include '.RData'
     I=32, K=10, seed=12345,
     p=c(0.38, 0.31, 0.31), # children prop. on each group
     par=list( m_c=0, s_c=0.5, # children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=-0.05 ) )

# loading data
data_nam = 'Hbeta_sim5.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered no cor ####
model_nam = "Hbeta_C_sim5_nocor.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 150-450 of 4000


## non-centered no cor ####
model_nam = "Hbeta_NC_sim5_nocor.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# NO divergent transitions


## centered cor ####
model_nam = "Hbeta_C_sim5_cor.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 100-300 of 4000


## non-centered cor ####
model_nam = "Hbeta_NC_sim5_cor.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# YES divergent transitions: 90-150 of 4000
