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
source( file.path( getwd(), 'sim_code', '1_2_E_sim_fun.R') )






# # testing power ####
# # run only once
# Epower( power_save=file.path(getwd(), 'sim_chain'), # power result dir need to include getwd()
#         sim_name='E_sim2_power.RData', # file_save need to include getwd()
#         sim_save=file.path(getwd(), 'sim_data'), # file_name need to include '.RData'
#         model_name='E_NC_sim2', # model for which we want to calculate power
#         model_in=file.path(getwd(), 'sim_models'), # location load models
#         model_out=file.path(getwd(), 'sim_chain'), # location to save results
#         Nsim=100, # number of simulation for power
#         prob=0.9, # significance
#         I_grid = c(48, 60), # experimental units (children)
#         K_grid = c(10, 20), # replicates (utterances)
#         p=c(0.34, 0.33, 0.33), # children prop. on each group
#         par_int=c('aHS','bP','bA','m_i','s_i','m_M','SI'), # parameter to analyze power
#         par_cont=c('aHS','SI') ) # parameters to contrast





# result analysis ####

## medium effects ####

# load results
load(file.path( getwd(), 'sim_chain', 'E_sim2_power.RData'))
# View( par_res )



# plot results
# unique(par_res$par_names)

# pdf("power_result11.pdf", width=6, height=10)
plot_power(d=par_res, # object from Epower() function
           par_plot=c('bA','bP','s_i','m_M'), # parameters to find power
           contrast=F, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 1) # number for the set of plot to show)
# dev.off()

par_res[par_res$par_names=='bA',]
par_res[par_res$par_names=='bP',]

par_res[par_res$par_names=='s_i',]
par_res[par_res$par_names=='m_M',]


# pdf("power_result12.pdf", width=6, height=10)
plot_power(d=par_res, # object from Epower() function
           par_plot='aHS', # parameters to find power
           contrast=F, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 2) # number for the set of plot to show)
# dev.off()

# pdf("power_result13.pdf", width=6, height=10)
plot_power(d=par_res, # object from Epower() function
           par_plot='aHS', # parameters to find power
           contrast=T, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 2) # number for the set of plot to show)
# dev.off()

par_res[ str_detect(par_res$par_names, '^aHS'),]



# pdf("power_result14.pdf", width=6, height=10)
plot_power(d=par_res, # object from Epower() function
           par_plot='SI', # parameters to find power
           contrast=T, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 1) # number for the set of plot to show)
# dev.off()

par_res[ str_detect(par_res$par_names, '^SI'),]







## small effects ####

# load results
load(file.path( getwd(), 'sim_chain', 'E_sim2_power2.RData'))
# View( par_res )



# plot results
# unique(par_res$par_names)

# pdf("power_result21.pdf", width=6, height=10)
plot_power(d=par_res, # object from Epower() function
           par_plot=c('bA','bP','s_i','m_M'), # parameters to find power
           contrast=F, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 1) # number for the set of plot to show)
# dev.off()

par_res[par_res$par_names=='bA',]
par_res[par_res$par_names=='bP',]

par_res[par_res$par_names=='s_i',]
par_res[par_res$par_names=='m_M',]



# pdf("power_result22.pdf", width=6, height=10)
plot_power(d=par_res, # object from Epower() function
           par_plot='aHS', # parameters to find power
           contrast=F, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 2) # number for the set of plot to show)
# dev.off()


# png("power_result23.png", width=600, height=800, res=100)
plot_power(d=par_res, # object from Epower() function
           par_plot='aHS', # parameters to find power
           contrast=T, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 2, # number for the set of plot to show),
           legend_loc='left')
# dev.off()


par_res[ str_detect(par_res$par_names, '^aHS'),]


# pdf("power_result24.pdf", width=6, height=10)
plot_power(d=par_res, # object from Epower() function
           par_plot='SI', # parameters to find power
           contrast=T, # plot contrast only
           Nprop = 0.33, # for x axis in plot
           plotN = 1) # number for the set of plot to show)
# dev.off()


par_res[ str_detect(par_res$par_names, '^SI'),]

