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
source( file.path( getwd(), 'sim_code', '0_sim_extra.R') )
source( file.path( getwd(), 'real_code', '1_0_E_real_mod.R') )



# data ####
data_dir = file.path('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/#data')
load( file.path(data_dir, 'E_data.RData') )



# models ####

## NC1 ####
model_nam = "E_NC1.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# YES divergent transitions: 40-800/4000




## NC2a ####
model_nam = "E_NC2a.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC2b ####
model_nam = "E_NC2b.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC3 ####
model_nam = "E_NC3.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC5a1 ####
model_nam = "E_NC5a1.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC5a2 ####
model_nam = "E_NC5a2.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC5a3 ####
model_nam = "E_NC5a3.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC5b1 ####
model_nam = "E_NC5b1.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# YES divergent transitions: 10/4000




## NC5b2 ####
model_nam = "E_NC5b2.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC5b3 ####
model_nam = "E_NC5b3.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC6a ####
model_nam = "E_NC6a.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC6b ####
model_nam = "E_NC6b.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions




## NC6c ####
model_nam = "E_NC6c.stan"
model_in = file.path(getwd(), 'real_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace(model_nam, '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions

