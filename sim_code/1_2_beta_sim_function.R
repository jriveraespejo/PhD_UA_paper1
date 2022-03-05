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




# simulation: ####
# 
# details:
#
# Outcome: complex generation (simple generation is nested)
#
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   dSI/dA > 0 (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# A * HS -> SI: 
#   dSI/dA[HS=NH] = dSI/dA[HI/CI] = dSI/dA[HS=HI/HA] = 0 
#   (no different evolution)
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# function
Esim = function(sim_name=NULL, # file_name need to include '.RData'
                sim_save=NULL, # file_save need to include getwd()
                seed=NULL, # seed
                I=32, # experimental units (children)
                K=10, # replicates (utterances)
                p=c(0.38, 0.31, 0.31), # children prop. on each group
                par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
                          m_M=10, s_M=NULL, # generation of df (M)
                          a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ) ){
  
  # # test
  # sim_name=NULL # file_name need to include '.RData'
  # sim_save=NULL # file_save need to include getwd()
  # seed=12345 # seed
  # I = 50 # experimental units (children)
  # K = 10 # replicates (utterances)
  # p=c(0.36, 0.32, 0.32) # children prop. on each group
  # par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
  #           m_M=10, s_M=NULL, # hyperprior generation of df (M)
  #           a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 )
  # #         m_M=1.5, s_M=0.5
  # #         a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0
  
  
  
  # 1. true data ####
  dT = data.frame(matrix(NA, nrow=I, ncol=9))
  names(dT) = c('child_id','E','PTA','A','HS','re_i','m_SI','m_H','M')
  dT$child_id = 1:I
  
  
  # assigning children to groups
  n = round( p*I )
  if( sum(n) != I ){
    if( I - sum(n[c(1,3)]) > n[2] ){
      n[2] = I - sum(n[c(1,3)]) # to sum the right amount
    } else {
      n[3] = I - sum(n[c(1,3)]) # to sum the right amount
    }
  }  
  
  
  # generating covariates
  if(!is.null(seed)){
    set.seed(seed+1)  
  }
  dT$HS = c( rep(1, n[1]), rep(2, n[2]), rep(3, n[3])) 
  dT$A = round( rnorm( sum(n), 5, 1) )
  dT$A = with(dT, ifelse(A>7, 7, A) )
  
  dT$E = c( rep(1, n[1]), # no way to know true effects
            sample(2:3, size=n[2], replace=T),
            sample(3:4, size=n[3], replace=T)) 
  
  dT$PTA = c( round(rnorm(n[1], 60, 15)), # first 12 NH 
              round(rnorm(n[2], 90, 15)), # next 10
              round(rnorm(n[3], 110, 15))) # last 10
  
  
  # children's random effects
  if(!is.null(seed)){
    set.seed(seed-1)  
  }
  par$re_i = rnorm(I, par$m_i, par$s_i)
  dT$re_i = par$re_i # children's random effects
  
  
  # linear predictor / SI index
  dT$m_SI = with(dT, re_i + par$a + par$aE*E + par$aHS*HS +
                   par$bA*(A - min(A)) +
                   par$bAHS*(A - min(A))*HS + 
                   par$bP * c( standardize(PTA) ) )
  dT$m_H = inv_logit(-dT$m_SI) # true entropy (SI -> Ht: negative)
  
  
  # variability of H
  if(!is.null(seed)){
    set.seed(seed+2)  
  }
  if( is.numeric(par$m_M) & !is.numeric(par$s_M) ){ 
    par$M = rep(par$m_M, I)
  } else{
    par$M = round( rlnorm(I, meanlog=par$m_M, sdlog= par$s_M) ) # dfs
  }
  dT$M = par$M # same df for all children (not same shape!!)
  
  
  # rounding
  dT[,6:ncol(dT)] = round( dT[,6:ncol(dT)], 5)
  
  
  
  # 2. observed data ####
  N = I*K
  dO = data.frame(matrix(NA, nrow=N, ncol=3))
  names(dO) = c('child_id','utt_id','H')
  dO$child_id = rep(1:I, each=K)
  dO$utt_id = rep(1:K, I)
  
  # generating observed H
  # i=1
  if(!is.null(seed)){
    set.seed(seed-2)  
  }
  for(i in 1:I){
    
    # identify data
    idx = dO$child_id == i
    
    # linear predictor
    dO$H[idx] = rbeta2(n=K, prob=dT$m_H[i], theta=dT$M[i])
  }
  
  # round
  dO$H = round( dO$H, 5)
  
  
  # 3. list data ####
  dL = list(
    # dimensions
    N = nrow(dO), # observations
    I = max(dO$child_id), # children
    K = max(dO$utt_id), # utterances
    
    # category numbers
    cHS = max(dT$HS),
    cE = max(dT$E),
    
    # child's data
    HS = dT$HS,
    Am = with(dT, A-min(A) ), # centered at minimum
    E = dT$E,
    sPTA = c( standardize( dT$PTA ) ),
    
    # observed data
    H = with(dO, ifelse(H==0, 0.0001, ifelse(H==1, 0.9999, H)) ), # trick
    cid = dO$child_id,
    uid = dO$utt_id
  )
  
  
  # 4. save data ####
  mom = list(dS=list( dT=dT, dO=dO, par=par), dL=dL)
  
  if( is.null(sim_save) | is.null(sim_name) ){
    return(mom)
  } else{
    save(mom, file=file.path(sim_save, sim_name) )
  }  
}  





# simulation 2: ####
# 
# details:
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
# simulation function
Esim2 = function( sim_name, # file_name need to include '.RData'
                  sim_save, # file_save need to include getwd()
                  seed=12345, # seed
                  I=32, # experimental units (children)
                  K=10, # replicates (utterances)
                  J=100, # duplicates (judges)
                  max_occ=50 ){ # maximum number of word occurrences
  
  # # data
  # I = 32
  # K = 10 # utterances
  # J = 100
  # max_occ=50
  
  # 1. full transcriptions ####
  # (diff. numbers count as diff. K)
  dF = data.frame(matrix(NA, nrow=I*J, ncol=K+2))
  names(dF) = c('child', 'judge', paste0('u_', 1:K))
  dF$child = rep(1:I, each=J)
  dF$judge = rep(1:J, I)
  # dim(dF)
  # View(dF)
  
  # c=1; w=10
  if(!is.null(seed)){
    set.seed(seed+1)  
  }
  for(i in 1:I){
    for(k in 1:K){
      index = with(dF, child==i) # identify child 
      word_occ = sample(size=1, x=1:max_occ, replace=T) # simulate word occurence
      dF[index, k+2] = round( runif(J, min=1, max=word_occ) )
    }
  }
  
  
  # 2. data event ####
  dE = data.frame(matrix(NA, nrow=I*max_occ, ncol=K+2))
  names(dE) = c('child', 'w_occ', paste0('u_', 1:K))
  dE$child = rep(1:I, each=max_occ)
  dE$w_occ = rep(1:max_occ, I)
  # dim(dE)
  # View(dE)
  
  # c=1; m=1
  for(i in 1:I){
    for(m in 1:max_occ){
      index1 = with(dF, child==i) # identify child in dF
      index2 = with(dE, child==i & w_occ==m) # identify child and event
      dE[index2, -c(1:2)] = colSums( dF[index1, -c(1:2)] == m )
    }
  }
  
  
  
  # 3. data probability ####
  dP = dE
  dP[,-c(1:2)] = dP[,-c(1:2)]/J
  # dim(dP)
  # View(dP)
  
  
  
  # 4. data entropy ####
  dH = data.frame(matrix(NA, nrow=I, ncol=K+1))
  names(dH) = c('child', paste0('u_', 1:K))
  dH$child = 1:I
  
  # c=1; w=8
  for(i in 1:I){
    index1 = with(dP, child==i)
    index2 = with(dH, child==i)
    dH[index2, -1] = apply( dP[index1, -c(1:2)], 2, Hfun, N=J)
  }
  
  
  # 5. list data ####
  H = melt(dH, id.vars='child')
  H = H[order(H$child),]
  
  dL = list(
    N = nrow( H ),
    K = length( unique( H$variable ) ), # utterances
    I = max( H$child ),
    H = with(H, ifelse(value==0, 0.0001, ifelse(value==1, 0.9999, value)) ),
    cid = H$child )
  # notice trick to handle zeroes/ones
  
  
  
  # 6. save data ####
  mom = list(dS=list(dF=dF, dE=dE, dP=dP, dH=dH), dL=dL )
  
  if( is.null(sim_save) | is.null(sim_name)){
    return(mom)
  } else{
    save(mom, file=file.path(sim_save, sim_name) )
  }
  
}






# Entropy power: ####
# you need a correspondence between:
#   1. selected model,
#   2. parameters of interest
#   3. contrast of interest
#
Epower = function(power_save=NULL, # file_save need to include getwd()
                  sim_name=NULL, # file_name need to include '.RData'
                  sim_save=NULL, # file_save need to include getwd()
                  model_name, # model for which we want to calculate power
                  model_in = file.path(getwd(), 'sim_models'), # location load models
                  model_out = file.path(getwd(), 'sim_chain'), # location to save results
                  seed=NULL, # seed
                  Nsim=100, # number of simulation for power
                  I_grid, # experimental units (children) 
                  K_grid, # replicates (utterances)
                  par_int, # parameter to analyze power
                  p=c(0.36, 0.32, 0.32), # children prop. on each group
                  par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
                            m_M=10, s_M=NULL, # hyperprior generation of df (M)
                            a=0, # test only intercept model
                            aE=0, # test par with 4 levels, and 6 contrasts 
                            aHS=0, # test par with 3 levels, and 3 contrasts 
                            bP=0, # continuous (standardized) variable
                            bA=0, # continuous (integer) variable
                            bAHS=0) ){ # continuous interaction (goes together with bA)
  
  
  # # test
  # power_save=file.path(getwd(), 'sim_chain') # power result dir need to include getwd()
  # sim_name='Hbeta_sim2_power.RData' # file_save need to include getwd()
  # sim_save=file.path(getwd(), 'sim_data') # file_name need to include '.RData'
  # model_name='Hbeta_NC_sim2' # model for which we want to calculate power
  # model_in=file.path(getwd(), 'sim_models') # location load models
  # model_out=file.path(getwd(), 'sim_chain') # location to save results
  # seed=NULL # seed
  # Nsim=10 # number of simulation for power
  # par_int=c('aHS','bP','bA','m_i','s_i','m_M','SI') # parameter to analyze power
  # I_grid = c(40, 50) # experimental units (children)
  # K_grid = c(10, 15, 20) # replicates (utterances)
  # p=c(0.38, 0.31, 0.31) # children prop. on each group
  # par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
  #           m_M=10, s_M=NULL, # hyperprior generation of df (M)
  #           a=0, # test only intercept model
  #           aE=0, # test par with 4 levels, and 6 contrasts
  #           aHS=-0.5, # test par with 3 levels, and 3 contrasts
  #           bP=-0.3, # continuous (standardized) variable
  #           bA=0.15, # continuous (integer) variable
  #           bAHS=0) # continuous interaction (goes together with bA)
  # #         m_M=1.5, s_M=0.5
  # #         a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0
  
  
  # expand grid
  par_grid = expand.grid(I_grid=I_grid, K_grid=K_grid)
  par_grid = par_grid[with(par_grid, order(I_grid, K_grid)),]
  
  # l=1
  for(l in 1:nrow(par_grid) ){
    
    # printing stuff
    writeLines( c( paste( rep('#', 50), collapse='' ),
                   paste( rep('#', 50), collapse='' ),
                   paste( rep('\n', 1), collapse='' ),
                   paste0('model: ', model_name, 
                          ', I = ', par_grid[l,1], 
                          ', K = ', par_grid[l,2]), 
                   paste( rep('\n', 1), collapse='' ),
                   paste( rep('#', 50), collapse='' ),
                   paste( rep('#', 50), collapse='' ) ) )
    
    
    
    # 1. model compilation ####
    mod = cmdstan_model( file.path(model_in, paste0(model_name, '.stan') ) )
    
    nsim = 1
    while(nsim <= Nsim){
      
      # 2. data simulation ####
      if( is.null(seed) ){
        seed1 = sample(1:500, 1)
        seed1 = seed1 + nsim*sample(1:100, 1)
      }
      if( is.null(sim_name)| is.null(sim_save) ){
        mom = Esim(sim_name=sim_name, 
                   sim_save=sim_save, 
                   seed=seed1, 
                   I=par_grid[l,1], 
                   K=par_grid[l,2], 
                   p=p, par=par )  
      } else{
        Esim(sim_name=sim_name, 
             sim_save=sim_save, 
             seed=seed1, 
             I=par_grid[l,1],
             K=par_grid[l,2], 
             p=p, par=par )
        load( file.path(sim_save, sim_name) )
      }
      
      
      # 3. model running ####
      mod$sample( data=mom$dL, 
                  output_dir=model_out, 
                  output_basename = str_replace(model_name, '.stan', ''),
                  chains=4, parallel_chains=4,
                  show_messages=F)
      #init=0, 
      # adapt_delta=0.95) 
      
      # load model results
      model_fit = file_id(model_out, model_name)
      res = rstan::read_stan_csv( file.path( model_out, model_fit ) )
      
      
      # 4. true parameter comparison ####
      # extract par_true
      par_true = data_detect_par(d=mom, par_int=par_int)
      # length(par_true)
      
      # parameter recovery
      par_recovery = parameter_recovery( stan_object = res,
                                         est_par = par_int,
                                         true_par = par_true )
      
      # named par_true
      names(par_true) = row.names(par_recovery)
      
      
      # 5. contrast comparison ####
      
      # true contrasts
      par_extra = which( par_int %in% c('aHS','aE','bAHS') )
      diff_true = c()
      if( length(par_extra)!=0 ){
        # m=1
        for( m in par_extra ){
          
          # select parameters
          idx = which( str_detect(names(par_true), paste0('^',par_int[m])) )
          
          # compare
          par_cont = expand.grid( idx, idx)
          par_cont = par_cont[with(par_cont, order(Var1,Var2)),]
          if(length(idx)==3){
            idx_not = c(1,4:5,7:9)
          } else{
            idx_not = c(1,5:6,9:11,13:16)
          }
          par_cont = par_cont[-idx_not,]
          par_cont$Var1 = par_true[par_cont$Var1]
          par_cont$Var2 = par_true[par_cont$Var2]
          
          diff_true = c(diff_true, with(par_cont, Var2-Var1))
        }
      }
      
      # contrast recovery
      cont_recovery = contrast_recovery(stan_object = res,
                                        est_diff = par_int[par_extra],
                                        true_diff = diff_true)
      
      
      # 6. storage simulations ####
      par_mom = par_recovery[,c('true','mean','in_CI','diff_0','RMSE')]
      par_mom = rbind(par_mom,
                      cont_recovery[,c('true','mean','in_CI','diff_0','RMSE')])
      if(nsim==1){
        par_comparison = data.frame(n=nsim, 
                                    par_names=rownames(par_mom),
                                    par_mom)
      } else{
        par_comparison = rbind(par_comparison, 
                               data.frame(n=nsim, 
                                          par_names=rownames(par_mom),
                                          par_mom) )
      }
      
      # counting
      nsim=nsim+1
    }
    
    
    # storage results
    row.names(par_comparison) = NULL
    par_mom = par_comparison %>%
      group_by(par_names) %>%
      summarize(true_mean=mean(true), sim_mean=mean(mean),  
                pCI=mean(in_CI), pdiff0=mean(diff_0), 
                mRMSE=mean(RMSE), sRMSE=sd(RMSE))
    
    if(l==1){
      par_res = data.frame(I=par_grid[l,1], K=par_grid[l,2], par_mom)
    } else{
      par_res = rbind(par_res,
                      data.frame(I=par_grid[l,1], K=par_grid[l,2], par_mom) )
    }
    
    
    # return object
    if( is.null(power_save) | is.null(model_name)){
      return(par_res)
    } else{
      save(par_res, file=file.path(power_save, paste0(model_name, '_power.Rdata')) )
    }
    
  }
  
}




# power plot ####
plot_power = function(d=par_res, # object from Epower() function
                      exclude='SI', # variable to exclude from plot
                      plotN = 1){ # number for the set of plot to show 
  
  # # test
  # d=par_res # object from Epower() function
  # exclude='SI' # variable to exclude from plot
  # plotN = 2 # number for the set of plot to show
  
  
  # identify unique parameters
  par_plot = unique( d$par_names )
  idx_plot = str_detect( par_plot, paste0('^', exclude) )
  par_plot = par_plot[!idx_plot]
  par_plot = par_plot[order(par_plot)]
  # length(par_plot)
  
  
  K = unique( d$K )
  
  pp = (1:4) + 4*(plotN-1)
  if( all(pp>length(par_plot)) ){
    plotN = floor(length(par_plot) / length(K))
    pp = (1:4) + 4*(plotN-1)
  }
  if( any(pp>length(par_plot)) ){
    idx = pp>length(par_plot)
    pp = pp[!idx]
  } 
  
  
  # plot
  par( mfrow=c( min(length(par_plot), 4), length(K) ) )
  # i=1
  for(i in pp){
    
    # identify parameter
    idx_par = d$par_names == par_plot[i]
    # d[ idx_par, c('I','K','pCI')]
    
    # k=10
    for(k in K){
      
      # identify plot
      idx = idx_par & d$K==k
      # d[ idx, c('I','K','pCI')]
      
      # main plot
      if( which(K==k)==1 ){
        mplot = paste0( par_plot[i],',   K=', k)
      } else{
        mplot = paste0('K=',k)
      }
      plot( NULL, ylim=c(0,1), xlim=range( d$I ), xaxt='n', main='',
            xlab='Experimental units (children)', 
            ylab= 'Probability (100 simulations) ' )
      mtext(mplot, 3, adj=0, cex=1.1)
      axis(side=1, at = unique( d$I ) )
      abline(h=c(0, 0.8), col='red', lty=2)
      abline(h=seq(0.2,0.6,0.2), col=col.alpha('gray',0.6), lty=2)
      legend('bottomright', legend = c('In CI', 'Power'), bty='n', 
             box.lwd='n', lty=rep(1,2), lwd=rep(2,2),
             col=c( col.alpha('black', 0.5), col.alpha('blue', 0.8) ) )
      
      lines( d[ idx, c('I','pCI')], lwd=1.5, 
             col=col.alpha('black', 0.3 * k/20) )
      points( d[ idx, c('I','pCI')], pch=19,
              col=col.alpha('black', 0.3 * k/20) )
      lines( d[ idx, c('I','pdiff0')], lwd=1.5, 
             col=col.alpha('blue', 0.5 * k/20) )
      points( d[ idx, c('I','pdiff0')], pch=19, 
              col=col.alpha('blue', 0.5 * k/20) )
    }
    
  }
  
  par(mfrow=c(1,1))
  
}
