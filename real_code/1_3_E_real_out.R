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





# data ####
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






# model load ####


## NC1 ####
model_nam = "E_NC1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# str(E_NC1) # no data reporting
# precis(E_NC1, depth=4)


## NC2a ####
model_nam = "E_NC2a"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC2a = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC2a, depth=4)


## NC2b ####
model_nam = "E_NC2b"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC2b = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC2b, depth=4)


## NC3 ####
model_nam = "E_NC3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC3, depth=4)


## NC5a1 ####
model_nam = "E_NC5a1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5a1, depth=4)


## NC5a2 ####
model_nam = "E_NC5a2"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a2 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5a2, depth=4)


## NC5a3 ####
model_nam = "E_NC5a3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5a3, depth=4)


## NC5b1 ####
model_nam = "E_NC5b1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5b1, depth=4)


## NC5b2 ####
model_nam = "E_NC5b2"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b2 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5b2, depth=4)


## NC5b3 ####
model_nam = "E_NC5b3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5b3, depth=4)


## NC6a ####
model_nam = "E_NC6a"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6a = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC6a, depth=4)


## NC6b ####
model_nam = "E_NC6b"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6b = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC6b, depth=4)


## NC6c ####
model_nam = "E_NC6c"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6c = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC6c, depth=4)







# model comparison ####
set.seed(12345)
comp_WAIC = rethinking::compare( E_NC1, E_NC2a, E_NC2b, E_NC3, 
                                 E_NC5a1, E_NC5a2, E_NC5a3,
                                 E_NC5b1, E_NC5b2, E_NC5b3,
                                 E_NC6a, E_NC6b, E_NC6c, 
                                 func=WAIC )

comp_PSIS = rethinking::compare( E_NC1, E_NC2a, E_NC2b, E_NC3, 
                                 E_NC5a1, E_NC5a2, E_NC5a3,
                                 E_NC5b1, E_NC5b2, E_NC5b3,
                                 E_NC6a, E_NC6b, E_NC6c,
                                 func=PSIS )






# model selection ####

## oulier check ####
set.seed(24071847)
WAIC_E_NC2b = WAIC(E_NC2b, pointwise=TRUE)
PSIS_E_NC2b = PSIS(E_NC2b, pointwise=TRUE)


# (figure 7.10)
plot( PSIS_E_NC2b$k , WAIC_E_NC2b$penalty, col=rangi2 , lwd=2 , 
      xlab="PSIS Pareto k", ylab="WAIC penalty"  )
abline(v=0.5, lty=2)
identify( x=PSIS_E_NC2b$k , y=WAIC_E_NC2b$penalty, labels=d$Loc )
