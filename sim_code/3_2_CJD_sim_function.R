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
# Outcome: complex generation, (simple generation is nested)
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
CJDsim = function(sim_name=NULL, # sim_name need to include '.RData'
                  sim_save=NULL, # sim_save need to include getwd()
                  seed=NULL, # seed
                  I=32, # experimental units (children)
                  K=10, # replicates (utterances)
                  D=20, # duplicates (comparisons)
                  J=80, # judges
                  p=c(0.36, 0.32, 0.32), # children prop. on each group
                  par=list( m_i=0, s_i=0.5, # children's random effects
                            m_j=0, s_j=0.5, # judges' random effects
                            r=NULL, # rate for s_SI
                            s_SI=0.1, # variability in children's observed SIs (vector[I] or constant)
                            s_HJ=0.1, # var. in observed HJo (constant)
                            a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ) ){
  
  # # test
  # sim_name=NULL # sim_name need to include '.RData'
  # sim_save=NULL # sim_save need to include getwd()
  # seed=12345 # seed
  # I = 32 # experimental units (children)
  # K = 10 # replicates (utterances)
  # D = 20 # duplicates (comparisons/assessments per I and K)
  # J = 80 # number of judges
  # p=c(0.36, 0.32, 0.32)
  # par=list( m_i=0, s_i=0.5, # children's random effects
  #           m_j=0, s_j=0.5, # judges' random effects
  #           r=NULL, # rate for s_SI
  #           s_SI=0.1, # var. in children's observed SIs (vector[I] or constant)
  #           s_HJ=0.1, # var. in observed HJo (constant)
  #           a=0, aE=0, aHS=0, bP=0, bA=0, bAHS=0 )
  # #         a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15
  
  
  # 1. true data ####
  dT = data.frame(matrix(NA, nrow=I, ncol=8+K))
  names(dT) = c('child_id','E','PTA','A','HS','re_i','m_SI','s_SI',
                paste0('u',1:K,'_SI'))
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
  par$re_i = rnorm(I, mean=par$m_i,sd=par$s_i)
  dT$re_i = par$re_i 
  
  # linear predictor / SI index
  dT$m_SI = with(dT, re_i + par$a + par$aE*E + par$aHS*HS +
                   par$bA*(A - min(A)) +
                   par$bAHS*(A - min(A))*HS + 
                   par$bP * c( standardize(PTA) ) )
  
  # variability of SI
  if(!is.null(seed)){
    set.seed(seed+2)  
  }
  if( !is.null(par$r) & is.null(par$s_SI) ){
    par$s_SI = rexp(I, rate=par$r)
    dT$s_SI = par$s_SI
  } else{
    dT$s_SI = par$s_SI # constant
  }
  
  
  
  # generating utterance values
  start = min(which(str_detect(names(dT), 'u[:digit:]{1,2}[:punct:]')))-1
  if(!is.null(seed)){
    set.seed(seed-2)  
  }
  for(i in 1:I){
    dT[i, 1:K + start] = rnorm(n=K, mean=dT$m_SI[i], sd=dT$s_SI[i])
  }
  
  # rounding
  dT[,6:ncol(dT)] = round( dT[,6:ncol(dT)], 5)
  
  
  
  
  # 2. full data (NOT SAVED) ####
  
  # comparisons
  comp = expand.grid(child_id1=1:I, child_id2=1:I)
  idx = with(comp, child_id1==child_id2)
  comp = comp[!idx,] # removing equals
  comp$eq = with(comp, ifelse( child_id1<child_id2, 
                               paste(child_id1, child_id2),
                               paste(child_id2, child_id1) ) )
  comp = comp[with(comp, order(child_id1, child_id2)), ]
  # mom = data.frame(table(comp$eq))
  # comp = merge(comp, mom, by.x='eq', by.y='Var1')
  # comp = comp[with(comp, order(child_id1, child_id2)), c(2:3,1,4)]
  # comp = comp[!duplicated(comp$eq),]
  # comp = comp[,-3]
  Dt = nrow(comp)
  
  
  # storage
  N = Dt*K*J
  dF = data.frame(matrix(NA, nrow=N, ncol=10))
  names(dF) = c('child_id1','child_id2','utt_id','judge_id','re_j','SI1','SI2','m_CJD','p_CJD','CJD')
  dF[,c('child_id1','child_id2')] = comp[rep(1:nrow(comp), each=K*J), c('child_id1','child_id2')]
  dF$utt_id = rep(1:K, Dt*J)
  dF = dF[with(dF, order(child_id1, child_id2, utt_id) ),]
  dF$judge_id = 1:J
  
  
  # judges' random effects
  if(!is.null(seed)){
    set.seed(seed+3)  
  }
  par$re_j = round( rnorm(J, mean=par$m_j, sd=par$s_j), 5)
  dF$re_j = par$re_j[dF$judge_id]
  
  # pasting SI on full data
  mom1 = dT[dF$child_id1, 9:ncol(dT)]
  mom2 = dT[dF$child_id2, 9:ncol(dT)]
  
  # k=1
  for(k in 1:K){
    idx = dF$utt_id==k
    dF$SI1[idx] = mom1[idx, k]
    dF$SI2[idx] = mom2[idx, k]
  }
  
  # CJD linear predictor
  dF$m_CJD = with(dF, (SI1 - SI2) + re_j)
  
  # CJD prob
  dF$p_CJD = with(dF, inv_logit(dF$m_CJD))
  
  # observed CJD
  if(!is.null(seed)){
    set.seed(seed-3)  
  }
  dF$CJD = rbinom(N, size=1, prob=dF$p_CJD)
  # table(dF$utt_id)
  
  
  
  
  # 3. observed (sampled) data ####
  if(!is.null(seed)){
    set.seed(seed+4)  
  }
  # i=1;k=2
  for(i in 1:I){
    for(k in 1:K){
      
      # judge count
      if(i==1 & k==1){
        jcount = data.frame( id=1:J, count=0, av=T )
      } else{
        mom = data.frame( table(dO$judge_id) )
        mom$Var1 = as.integer( mom$Var1 )
        names(mom) = c('id','count')
        idx = !(jcount$id %in% mom$id)
        jcount = rbind(mom, data.frame( id=jcount$id[ idx ],
                                        count=jcount$count[ idx ]) )
        jcount$av = jcount$count < I*K*D/J 
        # only 80 comparisons per judge (make sure it is divisible)
      }
      # str(jcount)
      
      
      # identify child and utterance, and available judges
      jav = jcount$id[jcount$av==T]
      idx = with(dF, which(child_id1==i & 
                             utt_id==k & 
                             judge_id %in% jav) )

      # sample D comparisons from judge j
      idx = sample(idx, size=D, replace=F) 
      # make sure D/K is an integer
      
      
      # saving the sampled data
      if(i==1 & k==1){
        dO = dF[idx,]
      } else{
        dO = rbind(dO, dF[idx,])
      }
    
    }
  }
  dO = dO[with(dO, order(child_id1, child_id2, utt_id, judge_id) ),]
  # View(dO)
  # nrow(dO)
  # table(dO$judge_id) # balanced judges
  # table(dO$utt_id) # balanced utterances
  # table(dO$child_id1) # not balanced for children
  # table(dO$child_id2)
  # table( c(dO$child_id1, dO$child_id2) ) 
  # table(dO$utt_id, dO$child_id1)
  
  

  
  # 4. reduced data ####
  dR = dO %>%
    group_by(child_id1, utt_id) %>% #, child_id2
    summarise( c_CJD= sum(CJD) , n_CJD= n( ) )
  dR = data.frame( dR[with(dR, order(child_id1, utt_id) ),] ) #, child_id2
  # psych::describe(dR$nCJD)
  # sum(dR$nCJD)
  # nrow(dR)
  
  
  # 4. list data ####
  dL = list(
    # dimensions
    N = nrow(dO), # observations
    I = max(dO$child_id1), # children
    K = max(dO$utt_id), # utterances
    J = max(dO$judge_id), # judges
    D = nrow(dO) / ( max(dO$child_id1) * max(dO$utt_id) ), # duplicate
    R = max(dO$child_id1) * max(dO$utt_id), # total replicates
    
    # category numbers
    cHS = max(dT$HS),
    cE = max(dT$E),
    
    # child's data
    HS = dT$HS,
    Am = with(dT, A-min(A) ), # centered at minimum
    E = dT$E,
    sPTA = c( standardize( dT$PTA ) ),
    
    # full observed data
    CJD = dO$CJD, 
    cid = dO$child_id,
    uid = dO$utt_id,
    jid = dO$judge_id,
    
    # reduced data
    c_CJD = dR$c_CJD,
    n_CJD = dR$n_CJD,
    rcid = dR$child_id1,
    ruid = dR$utt_id
  )
  
  
  # 5. save data ####
  mom = list(dS=list( dT=dT, dO=dO, dR=dR, par=par), dL=dL)
  
  if( is.null(sim_save) | is.null(sim_name) ){
    return(mom)
  } else{
    save(mom, file=file.path(sim_save, sim_name) )
  } 
  
} 




# HJ power: ####
# you need a correspondence between:
#   1. selected model,
#   2. parameters of interest
#   3. contrast of interest
#
CJDpower = function(power_save=NULL, # file_save need to include getwd()
                    sim_name=NULL, # file_name need to include '.RData'
                    sim_save=NULL, # file_save need to include getwd()
                    model_name, # model for which we want to calculate power
                    model_in = file.path(getwd(), 'sim_models'), # location load models
                    model_out = file.path(getwd(), 'sim_chain'), # location to save results
                    seed=NULL, # seed
                    Nsim=100, # number of simulation for power
                    I_grid, # experimental units (children) 
                    K_grid, # replicates (utterances)
                    D_grid, # duplicates (comparisons)
                    J=80, # number of judges
                    par_int, # parameter to analyze power
                    p=c(0.36, 0.32, 0.32), # children prop. on each group
                    par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
                              m_j=0, s_j=0.5, # hyperprior generation of df (M)
                              r=NULL, # rate for s_SI
                              s_SI=0.1, # variability in children's observed SIs (vector[I] or constant)
                              s_HJ=0.1, # var. in observed HJo (constant)
                              a=0, # test only intercept model
                              aE=0, # test par with 4 levels, and 6 contrasts 
                              aHS=0, # test par with 3 levels, and 3 contrasts 
                              bP=0, # continuous (standardized) variable
                              bA=0, # continuous (integer) variable
                              bAHS=0) ){ # continuous interaction (goes together with bA)
  
  
  # test
  power_save=file.path(getwd(), 'sim_chain') # power result dir need to include getwd()
  sim_name='CJD_sim2_power.RData' # file_save need to include getwd()
  sim_save=file.path(getwd(), 'sim_data') # file_name need to include '.RData'
  model_name='CJD_NC_sim5_re' # model for which we want to calculate power
  model_in=file.path(getwd(), 'sim_models') # location load models
  model_out=file.path(getwd(), 'sim_chain') # location to save results
  seed=NULL # seed
  Nsim=10 # number of simulation for power
  par_int=c('aHS','bP','bA','m_i','s_i','s_SI','m_SI') # parameter to analyze power
  I_grid = c(32, 40, 50) # experimental units (children)
  K_grid = c(10, 15, 20) # replicates (utterances)
  D_grid = c(20, 25, 30) # duplicates (comparisons)
  J=80 # number of judges
  p=c(0.36, 0.32, 0.32) # children prop. on each group
  par=list( m_i=0, s_i=0.5, # hyperprior children's random effects
            m_j=0, s_j=0.5, # hyperprior generation of df (M)
            r=NULL, # rate for s_SI
            s_SI=0.1, # variability in children's observed SIs (vector[I] or constant)
            s_HJ=0.1, # var. in observed HJo (constant)
            a=0, # test only intercept model
            aE=0, # test par with 4 levels, and 6 contrasts
            aHS=-0.5, # test par with 3 levels, and 3 contrasts
            bP=-0.3, # continuous (standardized) variable
            bA=0.15, # continuous (integer) variable
            bAHS=0) # continuous interaction (goes together with bA)
  #         m_M=1.5, s_M=0.5
  #         a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0

  
  
  # expand grid
  par_grid = expand.grid(I_grid=I_grid, K_grid=K_grid, D_grid=D_grid)
  par_grid = par_grid[with(par_grid, order(I_grid,K_grid,D_grid)),]
  # par_grid$N = with(par_grid, I_grid*K_grid*D_grid)
  # par_grid$cJ_app = par_grid$N/J
  
  
  
  # l=1
  for(l in 1:nrow(par_grid) ){
    
    # printing stuff
    writeLines( c( paste( rep('#', 50), collapse='' ),
                   paste( rep('#', 50), collapse='' ),
                   paste( rep('\n', 1), collapse='' ),
                   paste0('model: ', model_name, 
                          ', I = ', par_grid[l,1], 
                          ', K = ', par_grid[l,2],
                          ', D = ', par_grid[l,3]), 
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
        mom = HJsim( sim_name=sim_name, 
                     sim_save=sim_save, 
                     seed=seed1, 
                     I=par_grid[l,1], 
                     K=par_grid[l,2],
                     D=par_grid[l,3],
                     J=J,
                     p=p, par=par )  
      } else{
        HJsim( sim_name=sim_name, 
               sim_save=sim_save, 
               seed=seed1, 
               I=par_grid[l,1], 
               K=par_grid[l,2],
               D=par_grid[l,3],
               J=J,
               p=p, par=par )
        load( file.path(sim_save, sim_name) )
        # mom
      }
      
      
      # 3. model running ####
      mod$sample( data=mom$dL, 
                  output_dir=model_out, 
                  output_basename = str_replace(model_name, '.stan', ''),
                  chains=4, parallel_chains=4,
                  max_treedepth=20, adapt_delta=0.95, #,init=0
                  iter_warmup=2000, iter_sampling=2000, thin=2,
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
      par_res = data.frame(I=par_grid[l,1], 
                           K=par_grid[l,2],
                           D=par_grid[l,3],
                           par_mom)
    } else{
      par_res = rbind(par_res,
                      data.frame(I=par_grid[l,1], 
                                 K=par_grid[l,2], 
                                 D=par_grid[l,3],
                                 par_mom) )
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