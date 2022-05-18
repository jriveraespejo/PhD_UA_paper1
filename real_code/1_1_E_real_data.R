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

# merge etiology
d = merge(d, metadata[,c(1,27)], by.x='kind', by.y='ï..CI', all.x=T)


# children id
cid = unique(d$kind)
for(i in 1:length(cid)){
  idx=d$kind==cid[i]
  d$kind[idx] = i
}
d$kind = as.integer(d$kind)
# table(d$kind)



# children id
hsid = unique(d$hoorstatus)
idx=d$hoorstatus=='NH'
d$hoorstatus[idx] = 1
idx=d$hoorstatus=='CI'
d$hoorstatus[idx] = 2
d$hoorstatus = as.integer(d$hoorstatus)
# table(d$hoorstatus)



# etiology
eid = unique(d$Etiology)
enam = c(2,1,3,4,3,2,3)
for(i in 1:length(cid)){
  idx=d$Etiology==eid[i]
  d$Etiology[idx] = enam[i]
}
d$Etiology = as.integer(d$Etiology)
d$Etiology[is.na(d$Etiology)] = 1
# table(d$Etiology)



# sort
d = d[with(d, order(kind,FRAGMENTNUMMER)),]
# names(d)





# convert to list ####
HS = unique(d[,c('kind','hoorstatus')])

E = unique(d[,c('kind','Etiology')])
# E %>%
#   group_by(Etiology) %>%
#   summarise(n=n())

PTA = unique(d[,c('kind','unaided')])
PTA$sPTA = PTA$unaided
PTA$sPTA[!(PTA$unaided==0)] = c( standardize( PTA$sPTA[!(PTA$unaided==0)] ) )

A = unique(d[,c('kind','hearing.age')])
A$hearing.age = A$hearing.age/12 # in years (easier to spot effects)
# min(A$hearing.age)/12



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
  
  A = A$hearing.age,
  Am = A$hearing.age - min(A$hearing.age), # centered at minimum
  
  E = E$Etiology,
  
  PTA = PTA$unaided, 
  sPTA = PTA$sPTA,
  
  
  # observed data
  H = with(d, ifelse(entropiescore==0, 0.0001, 
                     ifelse(entropiescore==1, 0.9999, 
                            entropiescore)) ), # trick
  bid = d$reeks, # block id
  cid = d$kind,
  uid = d$FRAGMENTNUMMER 
)

save(dlist, file=file.path(data_dir, 'E_data.RData') )
