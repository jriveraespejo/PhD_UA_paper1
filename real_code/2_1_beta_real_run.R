# preliminar ####
rm(list=ls())

librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
               'reshape2','tinytex','gt','haven',
               'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
               'rethinking','rstan','coda','runjags','rjags',#'loo'
               'cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)


setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1/code')



# loading sources
source( file.path( getwd(), '4_beta_sim_outcome.R') )





# final data ####

# data load
file_dir = 'C:\Users\JRiveraEspejo\Desktop\1. Work\#Classes\PhD Antwerp\#thesis\#data'
dH = read_csv(file.path(file_dir, 'name.csv'))
# str(dH)



# data transform
var_int = c('children_id', 'hearing_status', 'hearing_age', 'etiology', 'pta')
data_mom = unique( dH[, var_int] )
data_mom = data_mom[order(data_mom$children_id),]



