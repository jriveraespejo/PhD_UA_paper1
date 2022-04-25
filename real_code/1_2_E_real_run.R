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
source( file.path( getwd(), 'real_code', '1_1_E_real_mod.R') )




# load data ####
# entropy
data_dir = file.path('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/#data')
data_name = 'data_entropy.csv'
data_entropy = read.csv(file.path(data_dir, data_name), stringsAsFactors=F)
# names(data_entropy)


# metadata
data_dir = file.path('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/#data')
data_name = 'meta_data.csv'
metadata = read.csv(file.path(data_dir, data_name), 
                    stringsAsFactors=F)
# names(metadata)
# table(metadata$Type.of.CI)


# modify data ####
# select appropriate info
d = data_entropy[,c(2:5,8,11,13,15)]
# table(d$reeks,d$kind) 

# merge etiology
d = merge(d, metadata[,c(1,27)], by.x='kind', by.y='ï..CI', all.x=T)
# table(d$Etiology, d$hoorstatus)


# children id
cid = unique(d$kind)
for(i in 1:length(cid)){
  idx=d$kind==cid[i]
  d$kind[idx] = i
}
d$kind = as.integer(d$kind)


# children id
hsid = unique(d$hoorstatus)
idx=d$hoorstatus=='NH'
d$hoorstatus[idx] = 1
idx=d$hoorstatus=='CI'
d$hoorstatus[idx] = 2
d$hoorstatus = as.integer(d$hoorstatus)



# etiology
eid = unique(d$Etiology)
enam = c(2,1,3,4,3,5,3)
for(i in 1:length(cid)){
  idx=d$Etiology==eid[i]
  d$Etiology[idx] = enam[i]
}
d$Etiology = as.integer(d$Etiology)
d$Etiology[is.na(d$Etiology)] = 1
# str(d)



# sort
d = d[with(d, order(kind,FRAGMENTNUMMER)),]
# names(d)
# table(d$kind, d$reeks)



# convert to list ####
HS = unique(d[,c('kind','hoorstatus')])

E = unique(d[,c('kind','Etiology')])

PTA = unique(d[,c('kind','unaided')])

A = unique(d[,c('kind','hearing.age')])
A$hearing.age = A$hearing.age/12 # in years (easier to spot effects)



dlist = list(
  # dimensions
  N = nrow(d), # observations
  I = max(d$kind), # children
  K = max(d$FRAGMENTNUMMER), # utterances
  B = max(d$reeks),
  
  # category numbers
  cHS = max(d$hoorstatus),
  cE = max(d$Etiology),
  
  # child's data
  HS = HS$hoorstatus,
  Am = A$hearing.age - min(A$hearing.age), # centered at minimum
  E = E$Etiology,
  sPTA = c( standardize( PTA$unaided ) ),
  
  
  # observed data
  H = with(d, ifelse(entropiescore==0, 0.0001, 
                     ifelse(entropiescore==1, 0.9999, 
                            entropiescore)) ), # trick
  bid = d$reeks, # block id
  cid = d$kind,
  uid = d$FRAGMENTNUMMER 
)

save(dlist, file=file.path(data_dir, 'E_data.RData') )





# run models ####

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
# NO divergent transitions




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
# NO divergent transitions




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

