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
# precis(E_NC1, depth=4, 
#        pars=c('a','m_i','s_i','m_b','s_b','SI') )
# str(E_NC1) # no data reporting


## NC2a ####
model_nam = "E_NC2a"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC2a = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC2a, depth=4, 
#        pars=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','SI') )


## NC2b ####
model_nam = "E_NC2b"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC2b = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC2b, depth=4,
#        pars=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','m_M','SI') )


## NC3 ####
model_nam = "E_NC3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC3, depth=4, 
#        pars=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','m_M','s_M','SI') )


## NC5a1 ####
model_nam = "E_NC5a1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5a1, depth=4, 
#        pars=c('a','bP','aHS','bAHS','m_i','s_i','m_b','s_b','SI') )


## NC5a2 ####
model_nam = "E_NC5a2"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a2 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5a2, depth=4, 
#        pars=c('a','bP','aEHS','bA','m_i','s_i','m_b','s_b','SI') )


## NC5a3 ####
model_nam = "E_NC5a3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5a3, depth=4, 
#        pars=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','SI') )


## NC5b1 ####
model_nam = "E_NC5b1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5b1, depth=4,
#        pars=c('a','bP','aHS','bAHS','m_i','s_i','m_b','s_b','m_M','SI') )


## NC5b2 ####
model_nam = "E_NC5b2"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b2 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5b2, depth=4, 
#        pars=c('a','bP','aEHS','bA','m_i','s_i','m_b','s_b','m_M','SI') )


## NC5b3 ####
model_nam = "E_NC5b3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC5b3, depth=4, 
#        pars=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','m_M','SI') )


## NC6a ####
model_nam = "E_NC6a"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6a = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC6a, depth=4, 
#        pars=c('a','bP','aHS','bAHS','m_i','s_i','m_b','s_b','m_M','s_M','SI') )


## NC6b ####
model_nam = "E_NC6b"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6b = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC6b, depth=4, 
#        pars=c('a','bP','aEHS','bA','m_i','s_i','m_b','s_b','m_M','s_M','SI') )


## NC6c ####
model_nam = "E_NC6c"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6c = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# precis(E_NC6c, depth=4, 
#        pars=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','m_M','s_M','SI') )







# model analysis ####

## comparison ####
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

comp_WAIC
comp_PSIS




## selection ####
precis(E_NC2b, depth=4,
       pars=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','m_M','SI') )

precis(E_NC5b3, depth=4,
       pars=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','m_M','SI') )

# Results:
#
# interaction models are indistinguishable from non interaction models
# less evidence in robust models (diff. M for diff. i)
# no evidence in favor of M=10
# Notice E_NC5b3 model has less outliers (see below)



## contrasts ####
post = extract.samples(E_NC5b3)
# str(post)

cont = post$bAHS[,1] - post$bAHS[,2]
cont = cbind(cont, post$aEHS[,,1] - post$aEHS[,,2])
attr(cont, "dimnames")[[2]] = c('bAHS[1]-bAHS[2]', 
                                paste0('aEHS[',1:5,',1]-aEHS[',1:5,',2]'))


precis( as_tibble(cont), depth=4, hist=F, prob=0.90)





## chain quality ####
tri_plot(stan_object=E_NC5b3, pars=c('m_i','s_i','m_b','s_b','m_M'))
tri_plot(stan_object=E_NC5b3, pars=c('a','bP','bAHS[1]','bAHS[2]') )
tri_plot(stan_object=E_NC5b3, pars=paste0('aEHS[',1:5,',1]') )
tri_plot(stan_object=E_NC5b3, pars=paste0('aEHS[',1:5,',2]') )
tri_plot(stan_object=E_NC5b3, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=E_NC5b3, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=E_NC5b3, pars=paste0('Ht[', 1:5,']') )




## outlier check ####
WAIC_E = WAIC(E_NC5b3, pointwise=TRUE)
PSIS_E = PSIS(E_NC5b3, pointwise=TRUE)


plot( PSIS_E$k , WAIC_E$penalty, col=rangi2 , lwd=2 , 
      xlab="PSIS Pareto k", ylab="WAIC penalty"  )
abline(v=0.5, lty=2)
abline(v=0.7, lty=2, lwd=2)
# identify( x=PSIS_E$k , y=WAIC_E$penalty, labels=d$Loc )
# two observations are outlying



PSIS_E[PSIS_E$k>=0.5,]
dlist$cid[c(95,141)] # zero values (fixed with trick)
dlist$H[c(95,141)]

idx = dlist$cid %in% c(10,15)
data.frame(cid=dlist$cid[idx], uid=dlist$uid[idx], H=dlist$H[idx])
# it is because they have H=0 (perfect SI)





## distributional plots ####
data_true = with(dlist, data.frame(H=H, child=cid))
par_recovery = precis(E_NC5b3, depth=4)
distH_plot1( stan_object=E_NC5b3, 
             true_data=data_true, 
             csize=9, rplot=c(3,3),
             par_object=par_recovery, M=6)
# well enough capture of the data
