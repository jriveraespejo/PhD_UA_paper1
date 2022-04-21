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
source( file.path( getwd(), 'sim_code', '1_1_E_sim_mod.R') )




# load data ####
# entropy
data_dir = file.path('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/#data')
data_name = 'data_entropy.csv'
data_entropy = read.csv(file.path(data_dir, data_name), 
                        stringsAsFactors=F)
# names(data_entropy)


# metadata
data_dir = file.path('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/#data')
data_name = 'meta_data.csv'
metadata = read.csv(file.path(data_dir, data_name), 
                        stringsAsFactors=F)
# names(metadata)
table(metadata$Type.of.CI)


# modify data ####
# select appropriate info
d = data_entropy[,c(2:5,8,11,13,15)]

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
for(i in 1:length(cid)){
  idx=d$hoorstatus==hsid[i]
  d$hoorstatus[idx] = i
}
d$hoorstatus = as.integer(d$hoorstatus)



# etiology
# TO ASK
eid = unique(d$Etiology)
enam = c(2,1,3,6,4,7,5)
for(i in 1:length(cid)){
  idx=d$Etiology==eid[i]
  d$Etiology[idx] = enam[i]
}
d$Etiology = as.integer(d$Etiology)
d$Etiology[is.na(d$Etiology)] = 1
# str(d)





# convert to list ####
dlist = list(
  # dimensions
  N = nrow(d), # observations
  I = max(d$kind), # children
  K = max(d$FRAGMENTNUMMER), # utterances
  
  # category numbers
  cHS = max(d$hoorstatus),
  cE = max(d$Etiology),
  
  # child's data
  HS = d$hoorstatus,
  Am = with(d, hearing.age - min(hearing.age) ), # centered at minimum
  E = d$Etiology,
  sPTA = c( standardize( d$unaided ) ),
  
  # observed data
  H = with(d, ifelse(entropiescore==0, 0.0001, 
                     ifelse(entropiescore==1, 0.9999, 
                            entropiescore)) ), # trick
  cid = unique(d$kind),
  uid = unique(d$FRAGMENTNUMMER) 
)




# apply models ####

## type 2 ####
model_nam = "Hbeta_NC_sim2.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace('Hbeta_NC2', '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions



## type 3 ####
model_nam = "Hbeta_NC_sim3.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace('Hbeta_NC3', '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions



## type 5 ####
model_nam = "Hbeta_NC_sim5_nocor.stan"
model_in = file.path(getwd(), 'sim_models')
model_out = file.path(getwd(), 'real_chain')
mod = cmdstan_model( file.path(model_in, model_nam) )

print(model_nam)
mod$sample( data=dlist, 
            output_dir=model_out, 
            output_basename = str_replace('Hbeta_NC5', '.stan', ''),
            chains=4, parallel_chains=4,
            max_treedepth=20, adapt_delta=0.95) #,init=0
# NO divergent transitions



