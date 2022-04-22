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
source( file.path( getwd(), 'sim_code', '1_1_E_sim_mod.R') )
source( file.path( getwd(), 'sim_code', '1_2_E_sim_fun.R') )




# prevalences to consider (Raeve, 2016)
# In Belgium, 200 children born yearly with bilateral hearing loss
# 45% of 200 (90) have severe hearing loss, and qualify for CI
# but only 78% (70) of them receive the CI
# I assume the rest receive a hearing aid
#
# therefore we simulate
# a total of 350 individuals, 150 NH and 200 HI
# within HI, 70 are HI/CI and 130 are HI/HA
# then:
# round( c(150,70,130)/330, 2)
# sum( c(0.428,0.20,0.372) )
# 350 * c(0.428,0.20,0.372)



# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
#
## prior ####
Esim(sim_name='E_sim1.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=50, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.34, 0.33, 0.33), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=10, s_M=NULL, # generation of df (M)
               a=0,
               bP=0,
               aHS=rep(0,3),
               bA=0,
               bAHS=rep(0,3),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( rep(0,4), # NH 
                               rep(0,4), # HI/CI (1st is E=none)
                               rep(0,4) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


data_nam = 'E_sim1.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
mom$dL$H = NULL


model_nam = "E_C_sim1_prior.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=1, parallel_chains=1 ) #,init=0, adapt_delta=0.95




## centered ####
# simulating and saving data
Esim(sim_name='E_sim1.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=350, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.428,0.20,0.372), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=10, s_M=NULL, # generation of df (M)
               a=0,
               bP=0,
               aHS=rep(0,3),
               bA=0,
               bAHS=rep(0,3),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( rep(0,4), # NH 
                               rep(0,4), # HI/CI (1st is E=none)
                               rep(0,4) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


# loading data
data_nam = 'E_sim1.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


# running model
model_nam = "E_C_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions



## non-centered ####
model_nam = "E_NC_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
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
## prior ####
Esim(sim_name='E_sim2.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=50, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.34, 0.33, 0.33), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=10, s_M=NULL, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=c(0.4,0,-0.4),
               bA=0.15,
               bAHS=rep(0,3),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( rep(0,4), # NH
                               rep(0,4), # HI/CI (1st is E=none)
                               rep(0,4) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )



data_nam = 'E_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
mom$dL$H = NULL


model_nam = "E_C_sim2_prior.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=1, parallel_chains=1 ) #,init=0, adapt_delta=0.95




## centered ####
# simulating and saving data
Esim(sim_name='E_sim2.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=350, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.428,0.20,0.372), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=10, s_M=NULL, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=c(0.4,0,-0.4),
               bA=0.15,
               bAHS=rep(0,3),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( rep(0,4), # NH
                               rep(0,4), # HI/CI (1st is E=none)
                               rep(0,4) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


# loading data
data_nam = 'E_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


# running model
model_nam = "E_C_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions


## non centered ####
model_nam = "E_NC_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
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
## prior ####
Esim(sim_name='E_sim3.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=50, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.34, 0.33, 0.33), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=c(0.4,0,-0.4),
               bA=0.15,
               bAHS=rep(0,3),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( rep(0,4), # NH
                               rep(0,4), # HI/CI (1st is E=none)
                               rep(0,4) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


data_nam = 'E_sim3.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
mom$dL$H = NULL


model_nam = "E_C_sim3_prior.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=1, parallel_chains=1 ) #,init=0, adapt_delta=0.95



## centered ####
# simulating and saving data
Esim(sim_name='E_sim3.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=350, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.428,0.20,0.372), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=c(0.4,0,-0.4),
               bA=0.15,
               bAHS=rep(0,3),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( rep(0,4), # NH
                               rep(0,4), # HI/CI (1st is E=none)
                               rep(0,4) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


# loading data
data_nam = 'E_sim3.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


# running model
model_nam = "E_C_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions


## non-centered ####
model_nam = "E_NC_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions





# simulation 4: ####
# 
# details:
# Model: 2 types
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
## prior ####
Esim2(sim_name='E_sim4.RData', # file_name need to include '.RData'
      sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
      seed=12345, # seed
      I=50, # experimental units (children)
      K=10, # replicates (utterances)
      J=100, # duplicates (judges)
      max_occ=50 )


data_nam = 'E_sim4.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
mom$dL$H = NULL


model_nam = "E_C_sim4_prior.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=1, parallel_chains=1 ) #,init=0, adapt_delta=0.95




## centered ####
# simulating 
Esim2(sim_name='E_sim4.RData', # file_name need to include '.RData'
      sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
      seed=12345, # seed
      I=350, # experimental units (children)
      K=10, # replicates (utterances)
      J=100, # duplicates (judges)
      max_occ=50 )

# loading data
data_nam = 'E_sim4.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


# running model
model_nam = "E_C_sim4.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions


## non-centered ####
model_nam = "E_NC_sim4.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
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
## prior ####
Esim(sim_name='E_sim5.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=50, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.34, 0.33, 0.33), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=rep(0,3),
               bA=0,
               bAHS=c(0.20,0.15,0.10),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( c(0.4, rep(0,3) ), # NH
                               c(0, seq(0.1,-0.1,length.out=3) ), # HI/CI (1st is E=none)
                               c(0, seq(-0.3,-0.5,length.out=3) ) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


data_nam = 'E_sim5.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
mom$dL$H = NULL


model_nam = "E_C_sim5_prior.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=1, parallel_chains=1 ) #,init=0, adapt_delta=0.95





## centered ####
# simulating and saving data
Esim(sim_name='E_sim5.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=350, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.428,0.20,0.372), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=rep(0,3),
               bA=0,
               bAHS=c(0.20,0.15,0.10),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( c(0.4, rep(0,3) ), # NH
                               c(0, seq(0.1,-0.1,length.out=3) ), # HI/CI (1st is E=none)
                               c(0, seq(-0.3,-0.5,length.out=3) ) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


# loading data
data_nam = 'E_sim5.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


# running model
model_nam = "E_C_sim5.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions


## non-centered ####
model_nam = "E_NC_sim5.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions








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
#   PTA range, L=low, M1<M2=mid, H=high
#
## prior ####
Esim(sim_name='E_sim5.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=50, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.34, 0.33, 0.33), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=rep(0,3),
               bA=0,
               bAHS=c(0.20,0.15,0.10),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( c(0.4, rep(0,3) ), # NH
                               c(0, seq(0.1,-0.1,length.out=3) ), # HI/CI (1st is E=none)
                               c(0, seq(-0.3,-0.5,length.out=3) ) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


data_nam = 'E_sim5.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
mom$dL$H = NULL


model_nam = "E_C_sim6_prior.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=1, parallel_chains=1 ) #,init=0, adapt_delta=0.95





## centered ####
# simulating and saving data
Esim(sim_name='E_sim5.RData', # file_name need to include '.RData'
     sim_save=file.path(getwd(), 'sim_data'), # file_save need to include getwd()
     seed=12345, # seed
     I=350, # experimental units (children)
     K=10, # replicates (utterances)
     p=c(0.428,0.20,0.372), # children prop. on each group
     par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
               m_M=1.5, s_M=0.5, # generation of df (M)
               a=0,
               bP=-0.1,
               aHS=rep(0,3),
               bA=0,
               bAHS=c(0.20,0.15,0.10),
               aE=rep(0,4), # (1st is E=none)
               aEHS=matrix( c( c(0.4, rep(0,3) ), # NH
                               c(0, seq(0.1,-0.1,length.out=3) ), # HI/CI (1st is E=none)
                               c(0, seq(-0.3,-0.5,length.out=3) ) ),  # HI/HA (1st is E=none)
                            ncol=3, byrow=F) ) )


# loading data
data_nam = 'E_sim5.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


# running model
model_nam = "E_C_sim6.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions


## non-centered ####
model_nam = "E_NC_sim6.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions
