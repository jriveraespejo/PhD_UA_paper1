# preliminar ####
rm(list=ls())

# librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
#                'reshape2','tinytex','gt','haven',
#                'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
#                'rethinking','rstan','coda','runjags','rjags',#'loo'
#                'cmdstanr','posterior','bayesplot')
# sapply(librerias, require, character.only=T)
# # sapply(librerias, install.packages, character.only=T)


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
# E: Etiology (causes for the hearing impairment)
# PTA: pure tone average (aided and unaided)
# A: age
# HS: hearing status (three categories)
#     NH: normal hearing, 
#     HI/CI: hearing impaired, cochlear implant
#     HI/HA: hearing impaired, hearing aid
#
# Hypothesis:
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
                I=350, # experimental units (children)
                K=10, # replicates (utterances)
                p=c(0.50, 0.175, 0.325), # children prop. on each group
                par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
                          m_M=10, s_M=NULL, # generation of df (M)
                          a=0, 
                          bP=-0.1,
                          aHS=c(0.4,0,-0.4),
                          bA=0.15,
                          bAHS=rep(0,3),
                          aE=rep(0,4),
                          aEHS=matrix( c( rep(0,4), # NH 
                                          c(0, seq(0.1,-0.1,length.out=3) ), # HI/CI (1st is E=none)
                                          c(0, seq(0.1,-0.1,length.out=3) ) ),  # HI/HA (1st is E=none)
                                       ncol=3, byrow=F) ) ){
  
  # # test
  # sim_name=NULL # file_name need to include '.RData'
  # sim_save=NULL # file_save need to include getwd()
  # seed=12345 # seed
  # I = 50 # experimental units (children)
  # K = 10 # replicates (utterances)
  # p=c(0.36, 0.32, 0.32) # children prop. on each group
  # par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
  #           m_M=10, s_M=NULL, # generation of df (M)
  #           a=0,
  #           bP=-0.1,
  #           aHS=c(0.4,0,-0.4),
  #           aE=rep(0,4), # (1st is E=none)
  #           aEHS=matrix( c( rep(0,4), # NH
  #                           c(0, seq(0.1,-0.1,length.out=3) ), # HI/CI (1st is E=none)
  #                           c(0, seq(0.1,-0.1,length.out=3) ) ),  # HI/HA (1st is E=none)
  #                        ncol=3, byrow=F),
  #           bA=0.15,
  #           bAHS=c(0.05,0,-0.05) )

  
  # packages
  require(rethinking)
  
  # 1. true data ####
  dT = data.frame(matrix(NA, nrow=I, ncol=1))
  names(dT) = c('child_id')
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
            sample(2:4, size=sum(n[2:3]), replace=T)) 
  
  dT$PTA = c( round(rnorm(n[1], 60, 15)), # first 12 NH 
              round(rnorm(n[2], 90, 15)), # next 10
              round(rnorm(n[3], 110, 15))) # last 10
  
  
  # children's random effects
  if(!is.null(seed)){
    set.seed(seed-1)  
  }
  par$re_i = rnorm(I, par$m_i, par$s_i)
  dT$re_i = par$re_i # children's random effects (between SI var.)
  
  
  # linear predictor / SI index
  dT$SI = NA
  A_bar = min(dT$A)
  sPTA = standardize( dT$PTA )
  for(i in 1:I){
    dT$SI[i] = with(dT, par$re_i[i] + 
                      par$a + 
                      par$bP*sPTA[i] +
                      par$aHS[ HS[i] ] +
                      par$aE[ E[i] ] + 
                      par$aEHS[ E[i], HS[i] ] +
                      par$bA*( A[i] - A_bar ) +
                      par$bAHS[ HS[i] ]*( A[i] - A_bar ) )
  }
  
  
  # true entropy
  dT$Ht = inv_logit(-dT$SI) # true entropy (SI -> Ht: negative)
  
  
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
    dO$H[idx] = rbeta2(n=K, prob=dT$Ht[i], theta=dT$M[i])
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
                  I=400, # experimental units (children)
                  K=10, # replicates (utterances)
                  J=100, # duplicates (judges)
                  max_occ=50 ){ # maximum number of word occurrences
  
  # # data
  # I = 32
  # K = 10 # utterances
  # J = 100
  # max_occ=50
  
  # packages
  require(reshape2)
  
  
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
                  Nsim=100, # number of simulation for power
                  prob=0.9, # significance
                  I_grid, # experimental units (children) 
                  K_grid, # replicates (utterances)
                  par_int, # parameter to analyze power
                  par_cont, # parameters to contrast
                  p=c(0.34, 0.33, 0.33) ){ # children prop. on each group

  
  # # test
  # power_save=file.path(getwd(), 'sim_chain') # power result dir need to include getwd()
  # sim_name='E_sim2_power2.RData' # file_save need to include getwd()
  # sim_save=file.path(getwd(), 'sim_data') # file_name need to include '.RData'
  # model_name='E_NC_sim2' # model for which we want to calculate power
  # model_in=file.path(getwd(), 'sim_models') # location load models
  # model_out=file.path(getwd(), 'sim_chain') # location to save results
  # Nsim=2 # number of simulation for power
  # prob=0.9 # significance
  # I_grid = c(48, 60) # experimental units (children)
  # K_grid = c(10, 20) # replicates (utterances)
  # p=c(0.34, 0.33, 0.33) # children prop. on each group
  # par_int=c('aHS','bP','bA','m_i','s_i','m_M','SI') # parameter to analyze power
  # par_cont=c('aHS','SI') # parameters to contrast

  
  # packages
  require(cmdstanr)
  require(rstan)
  require(rethinking)
  require(stringr)
  require(dplyr)
  require(posterior)
  require(bayesplot)
  
  
  # expand grid
  par_grid = expand.grid(I_grid=I_grid, K_grid=K_grid)
  par_grid = par_grid[with(par_grid, order(I_grid, K_grid)),]
  
  # to fit models
  pow_name = str_replace( sim_name, '.RData', '')
  
  # l=2
  for(l in 1:nrow(par_grid) ){
    
    # 1. model compilation ####
    mod = cmdstan_model( file.path(model_in, paste0(model_name, '.stan') ) )
    
    # 2. estimated parameters ####
    model_fit = file_id(model_out, model_name) 
    res = rstan::read_stan_csv( file.path( model_out, model_fit ) )
    post = extract.samples( res )
    pidx = sample( 1:nrow(post$a), Nsim ) # sample of parameters
    # str(post)
    
        
    nsim = 1
    while(nsim <= Nsim){
      
      # printing stuff
      writeLines( paste0('model: ', model_name, 
                         ', I = ', par_grid[l,1], 
                         ', K = ', par_grid[l,2],
                         ', N = ', nsim ) )
      
      # print( paste0('model: ', model_name, 
      #               ', I = ', par_grid[l,1], 
      #               ', K = ', par_grid[l,2],
      #               ', N = ', nsim ) )
      
      
      # 3. data simulation ####
      if( is.null(sim_name) | is.null(sim_save) ){
        
        mom = Esim(sim_name=NULL, 
                   sim_save=NULL, 
                   seed=NULL, 
                   I=par_grid[l,1], 
                   K=par_grid[l,2], 
                   p=p,
                   par=list( m_i=post$m_i[pidx[nsim]], 
                             s_i=post$s_i[pidx[nsim]],
                             m_M=post$m_M[pidx[nsim]], 
                             s_M=NULL,
                             a=post$a[pidx[nsim]],
                             bP=post$bP[pidx[nsim]],
                             aHS=post$aHS[pidx[nsim],],
                             aE=rep(0,4), 
                             aEHS=matrix( c( rep(0,4),
                                             rep(0,4),
                                             rep(0,4) ),
                                          ncol=3, byrow=F),
                             bA=post$bA[pidx[nsim]],
                             bAHS=rep(0,3) ) )
      } else{
        
        Esim(sim_name=sim_name, 
             sim_save=sim_save, 
             seed=NULL, 
             I=par_grid[l,1], 
             K=par_grid[l,2], 
             p=p,
             par=list( m_i=post$m_i[pidx[nsim]], 
                       s_i=post$s_i[pidx[nsim]],
                       m_M=post$m_M[pidx[nsim]], 
                       s_M=NULL,
                       a=post$a[pidx[nsim]],
                       bP=post$bP[pidx[nsim]],
                       aHS=post$aHS[pidx[nsim],],
                       aE=rep(0,4), 
                       aEHS=matrix( c( rep(0,4),
                                       rep(0,4),
                                       rep(0,4) ),
                                    ncol=3, byrow=F),
                       bA=post$bA[pidx[nsim]],
                       bAHS=rep(0,3) ) )
        load( file.path(sim_save, sim_name) )
      }
    
      
      # 4. model running ####
      mod$sample( data=mom$dL, 
                  output_dir=model_out, 
                  output_basename=pow_name,
                  chains=4, parallel_chains=4,
                  show_messages=F)
      
      
      # load model results
      model_fit = file_id( model_out, pow_name )
      res = rstan::read_stan_csv( file.path( model_out, model_fit ) )
      
      
      # 5. true parameter comparison ####
      par_true = data_detect_par(d=mom, par_int=par_int)
      par_recovery = parameter_recovery( stan_object = res,
                                         est_par = par_int,
                                         true_par = par_true,
                                         p=prob)
      
      
      # 6. contrast comparison ####
      diff_true = true_contrast(d=mom, par_int=par_cont)
      cont_recovery = contrast_recovery( stan_object=res, 
                                         est_diff = par_cont, 
                                         true_diff = diff_true, 
                                         p=prob)
      
      
      # 7. storage simulations ####
      var_pow = c('true','mean','RMSE','sign','reject_null','accept_val','precision')
      par_mom = par_recovery[,var_pow]
      par_mom = rbind(par_mom, cont_recovery[,var_pow])
      
      
      if(nsim==1){
        par_comparison = data.frame(par_names=rownames(par_mom),
                                    par_mom)
      } else{
        par_comparison = rbind(par_comparison, 
                               data.frame(par_names=rownames(par_mom),
                                          par_mom) )
      }
      row.names(par_comparison) = NULL
      
      
      # 8. intermediate storage ####
      par_mom1 = par_comparison %>%
        group_by(par_names) %>%
        summarize(true_mean=mean(true, na.rm=T), 
                  sim_mean=mean(mean, na.rm=T),  
                  mRMSE=mean(RMSE, na.rm=T), 
                  sRMSE=sd(RMSE, na.rm=T),
                  pSign=mean(sign, na.rm=T), 
                  pReject=mean(reject_null, na.rm=T),
                  pAccept=mean(accept_val, na.rm=T), 
                  pPrecision=mean(precision, na.rm=T))
      
      
      if(l==1){
        par_res = data.frame(I=par_grid[l,1], 
                             K=par_grid[l,2], 
                             Nsim=nsim,
                             par_mom1)
      } else{
        if(nsim==1){
          par_res = rbind(par_res,
                          data.frame(I=par_grid[l,1], 
                                     K=par_grid[l,2], 
                                     Nsim=nsim,
                                     par_mom1) )
        } else{
          start = nrow(par_res) - nrow(par_mom1) + 1
          end = nrow(par_res)
          par_res[start:end, ] = data.frame(I=par_grid[l,1], 
                                            K=par_grid[l,2], 
                                            Nsim=nsim,
                                            par_mom1)
        }
      }
      # View(par_res)
      
      
      # counting
      nsim=nsim+1
      
      
      # return object
      if( is.null(power_save) | is.null(model_name) ){
        return(par_res)
      } else{
        save(par_res, file=file.path(power_save, paste0(pow_name, '.Rdata')) )
      }
      
    }
    
  }
  
}






# power plot ####
plot_power = function(d, # object from Epower() function
                      par_plot, # parameters to find power
                      contrast=T,
                      Nprop = 0.33, # for x axis in plot
                      plotN = 1, # number for the set of plot to show 
                      legend_loc ='left'){
  
  # # test
  # d=par_res # object from Epower() function
  # par_plot=c('aHS') # parameters to find power
  # contrast=T # plot contrast only
  # Nprop = 0.33 # for x axis in plot
  # plotN = 2 # number for the set of plot to show
  
  
  # pacakges
  require(stringr)
  require(RColorBrewer)
  require(rethinking)
  
  
  # identify parameters of interest
  par_names = unique( d$par_names )
  
  idx_plot = c()
  for(i in 1:length(par_plot)){
    idx_plot = c(idx_plot, 
                 which( str_detect( par_names, 
                                    paste0('^', par_plot[i]) ) ) )  
  }
  par_plot = par_names[idx_plot]
  
  if(contrast){
    
    str_idx = '^[:alpha:]{2,3}[:punct:][:digit:]{1,3}[:punct:]{2}'
    idx = which( str_detect( par_plot, str_idx) )
  
  } else{
    
    str_idx = '^[:alpha:]{2,3}[:punct:][:digit:]{1,3}[:punct:]{2}'
    idx_not = which( str_detect( par_plot, str_idx) )
    
    str_idx1 = '^[:alpha:]{2,3}[:punct:][:digit:]{1,3}[:punct:]{1}$'
    idx1 = which( str_detect( par_plot, str_idx1) )
    
    str_idx2 = '^[:alpha:]{1,}[:punct:]*'
    idx2 = which( str_detect( par_plot, str_idx2) )
    idx2 = idx2[!(idx2 %in% idx_not)]
    idx2 = idx2[!(idx2 %in% idx1)]
    
    idx = c(idx1,idx2)
    
  }
  par_plot = par_plot[idx]
  
  
  # paremeters for plot
  K = unique( d$K )
  
  pp = (1:4) + 4*(plotN-1)
  if( all(pp > length(par_plot) ) ){
    plotN = floor(length(par_plot) / length(K))
    pp = (1:4) + 4*(plotN-1)
  }
  if( any( pp>length(par_plot) ) ){
    idx = pp > length(par_plot)
    pp = pp[!idx]
  }
  
  plot_col = rep_len(rethink_palette, 4) # colors
  
  
  
  # plot
  par( mfrow=c( min(length(par_plot), 4), length(K) ) )
  
  # i=5
  for(i in pp){
    
    # identify parameter
    idx_par = d$par_names == par_plot[i]
    # d[ idx_par, ]
    
    # k=1
    for(k in 1:length(K)){
      
      # identify plot
      idx = idx_par & d$K==K[k]
      # d$par_names[idx]
      
      # labels
      if( k==1 ){
        mplot = paste0( par_plot[i],',   K=', K[k])
        y_lab = 'Probability (100 simulations) '
      } else{
        mplot = paste0('K=',K[k])
        y_lab = ''
      }
      
      # plot
      plot( NULL, ylim=c(0,1), xlim=range( d$I ), 
            xaxt='n', main='',
            xlab='Experimental units (children)', 
            ylab= y_lab )
      mtext(mplot, 3, adj=0, cex=1.1)
      axis(side=1, 
           at = unique( d$I ), 
           labels = round( unique( d$I ) * Nprop ) )
      abline(h=c(0, 0.8), col='red', lty=2)
      abline(h=seq(0.2,0.6,0.2), col=col.alpha('gray',0.6), lty=2)
      
      # legend
      if(i==pp[1] & k==1){
        legend(legend_loc, 
               legend = c('Sign','Reject Null','Accept Val','Precision'), 
               bty='n', 
               box.lwd='n', lty=c(1,1,2,2), lwd=rep(2,4),
               col=plot_col )  
      }
      
      
      lines( d[ idx, c('I','pSign')], lwd=2, col=plot_col[1] )
      points( d[ idx, c('I','pSign')], pch=19, col=plot_col[1] )
      
      lines( d[ idx, c('I','pReject')], lwd=2, col=plot_col[2] )
      points( d[ idx, c('I','pReject')], pch=19, col=plot_col[2] )
      
      lines( d[ idx, c('I','pAccept')], lwd=2, lty=2, col=plot_col[3] )
      points( d[ idx, c('I','pAccept')], pch=19, col=plot_col[3] )
      
      lines( d[ idx, c('I','pPrecision')], lwd=2, lty=2, col=plot_col[4] )
      points( d[ idx, c('I','pPrecision')], pch=19, col=plot_col[4] )
  
    }
    
  }
  
  par(mfrow=c(1,1))
  
}
