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
source( file.path( getwd(), 'sim_code', '2_1_HJ_sim_models.R') )
source( file.path( getwd(), 'sim_code', '2_2_HJ_sim_function.R') )






# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_SI low and equal for all children 
# Covariates: None
#
# simulating and saving data
HJsim(sim_name='HJ_sim1.RData', # sim_name need to include '.RData'
      sim_save=file.path(getwd(), 'sim_data'), # sim_save need to include getwd()
      seed=12345, # seed
      I=32, # experimental units (children)
      K=10, # replicates (utterances)
      D=20, # duplicates (comparisons)
      J=80, # judges
      p=c(0.36, 0.32, 0.32), # children prop. on each group
      par=list( m_i=0, s_i=0.5, # children's random effects
                m_j=0, s_j=0.5, # judges' random effects
                r=NULL, # rate for s_SI
                s_SI=0.1, # variability in children's observed SIs (vector[I] or constant)
                s_HJ=0.1, # var. in observed HJo (constant)
                a=0, aE=0, aHS=0, bP=0, bA=0, bAHS=0 ) )


# loading data
data_nam = 'HJ_sim1.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
# mom


## centered ####
model_nam = "HJ_C_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2) 
# 5-10 out of 4000 hit the max_treedepth, good speed
# 7-15 out of 4000 were divergent transitions
#
# max_treedepth warnings are not as serious as divergent transitions
# (see https://mc-stan.org/docs/2_29/cmdstan-guide/diagnose.html)
# but still does not allow you to properly explore the posterior
# that is why we increase the max_treedepth (default max_treedepth=10),
# and the adapt_delta  (default adapt_delta=0.90) 
# in order to get a better posterior exploration
# (see https://mc-stan.org/docs/2_29/cmdstan-guide/mcmc-config.html)
#
# I also increase the number of warm ups and iterations
#
# after changing, 400-500/4000 were divergent



## non-centered ####
model_nam = "HJ_NC_sim1.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 1500-1600 out of 4000 hit the max_treedepth, really slow
# 2-5 out of 4000 were divergent
#
# after increasing it, 10-30/4000 divergence, but even slower





# simulation 2: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_SI low and equal for all children 
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
# simulating and saving data
HJsim(sim_name='HJ_sim2.RData', # sim_name need to include '.RData'
      sim_save=file.path(getwd(), 'sim_data'), # sim_save need to include getwd()
      seed=12345, # seed
      I=32, # experimental units (children)
      K=10, # replicates (utterances)
      D=20, # duplicates (comparisons)
      J=80, # judges
      p=c(0.36, 0.32, 0.32), # children prop. on each group
      par=list( m_i=0, s_i=0.5, # children's random effects
                m_j=0, s_j=0.5, # judges' random effects
                r=NULL, # rate for s_SI
                s_SI=0.1, # variability in children's observed SIs (vector[I] or constant)
                s_HJ=0.1, # var. in observed HJo (constant)
                a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ))


# loading data
data_nam = 'HJ_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)


## centered ####
model_nam = "HJ_C_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 50-100 divergent transitions


## non-centered ####
model_nam = "HJ_NC_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 10-50/4000 divergent transitions





# simulation 2: RE ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_SI low and equal for all children 
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
# loading data
data_nam = 'HJ_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
# mom


## centered ####
model_nam = "HJ_C_sim2_re.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 0/4000 divergent transitions


## non-centered ####
model_nam = "HJ_NC_sim2_re.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 0/4000 divergent transitions








# simulation 3: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_SI different for all children
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
# simulating and saving data
HJsim(sim_name='HJ_sim3.RData', # sim_name need to include '.RData'
      sim_save=file.path(getwd(), 'sim_data'), # sim_save need to include getwd()
      seed=12345, # seed
      I=32, # experimental units (children)
      K=10, # replicates (utterances)
      D=20, # duplicates (comparisons)
      J=80, # judges
      p=c(0.36, 0.32, 0.32), # children prop. on each group
      par=list( m_i=0, s_i=0.5, # children's random effects
                m_j=0, s_j=0.5, # judges' random effects
                r=3, # rate for s_SI
                s_SI=NULL, # variability in children's observed SIs (vector[I] or constant)
                s_HJ=0.1, # var. in observed HJo (constant)
                a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ))


# loading data
data_nam = 'HJ_sim3.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
# mom


## centered ####
model_nam = "HJ_C_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 400-600/4000 divergent transitions


## non-centered ####
model_nam = "HJ_NC_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 500-600/4000 divergent transitions












# simulation 4: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_SI different for all children
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
# simulating and saving data
HJsim(sim_name='HJ_sim4.RData', # sim_name need to include '.RData'
      sim_save=file.path(getwd(), 'sim_data'), # sim_save need to include getwd()
      seed=12345, # seed
      I=32, # experimental units (children)
      K=10, # replicates (utterances)
      D=20, # duplicates (comparisons)
      J=80, # judges
      p=c(0.36, 0.32, 0.32), # children prop. on each group
      par=list( m_i=0, s_i=0.5, # children's random effects
                m_j=0, s_j=0.5, # judges' random effects
                r=3, # rate for s_SI
                s_SI=NULL, # variability in children's observed SIs (vector[I] or constant)
                s_HJ=0.1, # var. in observed HJo (constant)
                a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=-0.05 ) )


# loading data
data_nam = 'HJ_sim4.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
# mom


## centered ####
model_nam = "HJ_C_sim4_nocor.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 600-800/4000 divergent transitions



## non-centered ####
model_nam = "HJ_NC_sim4_nocor.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 400-500/4000 divergent transitions





# simulation 5: ####
#
# details:
# Model: 2 types
# Outcome: simple generation, s_SI equal for all children, reduced data
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
# loading data
data_nam = 'HJ_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
# mom


## centered ####
model_nam = "HJ_C_sim5.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 150-300/4000 divergent transitions



## non-centered ####
model_nam = "HJ_NC_sim5.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 300-500/4000 divergent transitions





# simulation 5: RE ####
#
# details:
# Model: 2 types
# Outcome: simple generation, s_SI equal for all children, reduced data
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
# loading data
data_nam = 'HJ_sim2.RData'
model_data = file.path(getwd(), 'sim_data', data_nam)
load(model_data)
# mom


## centered ####
model_nam = "HJ_C_sim5_re.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 100-200/4000 divergent transitions



## non-centered ####
model_nam = "HJ_NC_sim5_re.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'sim_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=mom$dL, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95, #,init=0
            iter_warmup=2000, iter_sampling=2000, thin=2)
# 0/4000 divergent transitions


