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
# source( file.path( getwd(), 'real_code', '1_2_E_real_run.R') )



# model comparison ####




## data ####
data_dir = file.path('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/#data')
data_nam = "E_data.RData"
model_data = file.path(data_dir, data_nam )
load( model_data )
# dlist


# plotting data
data_plots1(d=dlist, xdata='Am', ydata='H', alpha=0.05, os=F)
data_plots1(d=dlist, xdata='sPTA', ydata='H', alpha=0.05, os=F)
data_plots1(d=dlist, xdata='HS', ydata='H', alpha=0.05, os=F)
data_plots1(d=dlist, xdata='E', ydata='H', alpha=0.05, os=F)
# no relationship



## NC1 ####
model_nam = "E_NC1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# str(E_NC1) # no data reporting


