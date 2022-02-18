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
source( file.path( getwd(), 'sim_code', '1_0_beta_sim_extra.R') )




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
Esim = function(file_save, # file_save need to include getwd()
                file_name, # file_name need to include '.RData'
                I=32, K=10, seed=12345,
                p=c(0.38, 0.31, 0.31), # children prop. on each group
                par=list( m_c=0, s_c=0.5, # children's random effects
                          m_M=1.5, s_M=0.5, # generation of df (M)
                          a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ) ){
  
  # # test
  # I = 32 # experimental units (children)
  # K = 10 # replicates (utterances)
  # seed=12345
  # p=c(0.38, 0.31, 0.31)
  # par=list( m_c=0, s_c=0.5, # children's random effects
  #           m_M=10, s_M=NULL, # generation of df (M)
  #           a=0, aE=0, aHS=0, bP=0, bA=0, bAHS=0 )
  # #         m_M=1.5, s_M=0.5
  # #         a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0

  
  
  # 1. true data ####
  dT = data.frame(matrix(NA, nrow=I, ncol=9))
  names(dT) = c('child_id','E','PTA','A','HS','re_i','m_SI','m_H','M')
  dT$child_id = 1:I
  
  
  # generating covariates
  set.seed(seed)
  n = round( p*I )
  dT$HS = c( rep(1, n[1]), rep(2, n[2]), rep(3, n[3])) 
  dT$A = c( rep(7, n[1]), round(rnorm( sum(n[2:3]), 5, 1)) ) 
  dT$A = with(dT, ifelse(A>7, 7, A) )
  
  dT$E = c( rep(1, n[1]), # no way to know true effects
            sample(2:3, size=n[2], replace=T),
            sample(3:4, size=n[3], replace=T)) 
  
  dT$PTA = c( round(rnorm(n[1], 60, 10)), # first 12 NH 
              round(rnorm(n[2], 90, 10)), # next 10
              round(rnorm(n[3], 110, 20))) # last 10
  
  
  # children's random effects
  set.seed(seed+1)
  re_i = rnorm(I, par$m_c, par$s_c)
  dT$re_i = re_i 
  
  
  # linear predictor / SI index
  dT$m_SI = with(dT, re_i + par$a + par$aE*E + par$aHS*HS +
                   par$bA*(A - min(A)) +
                   par$bAHS*(A - min(A))*HS + 
                   par$bP * c( standardize(PTA) ) )
  dT$m_H = inv_logit(-dT$m_SI) # true entropy (SI -> Ht: negative)
  

  # variability of H
  set.seed(seed-1)
  if( is.numeric(par$m_M) & !is.numeric(par$s_M) ){ 
    dT$M = par$m_M # same df for all children (not same shape!!) 
  } else{
    dT$M = rlnorm(I, meanlog=par$m_M, sdlog= par$s_M) # dfs
    dT$M = round( dT$M )
  }
  
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
  set.seed(seed+2)
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
  save(mom, file=file.path(file_save, file_name) )
  
}  





# simulation 2: ####
# 
# details:
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
# simulation function
Esim2 = function( file_save, # file_save need to include getwd()
                  file_name, # file_name need to include '.RData'
                  children=32, words=10, judges=100, max_occ=50 ){
  
  # # data
  # children = 32
  # words = 10 # utterances
  # judges = 100
  # max_occ=50
  
  # 1. full transcriptions ####
  # (diff. numbers count as diff. words)
  dF = data.frame(matrix(NA, nrow=children*judges, ncol=words+2))
  names(dF) = c('child', 'judge', paste0('u_', 1:words))
  dF$child = rep(1:children, each=judges)
  dF$judge = rep(1:judges, children)
  # dim(dF)
  # View(dF)
  
  # c=1; w=10
  for(c in 1:children){
    for(w in 1:words){
      index = with(dF, child==c) # identify child 
      word_occ = sample(size=1, x=1:max_occ, replace=T) # simulate word occurence
      dF[index, w+2] = round( runif(judges, min=1, max=word_occ) )
    }
  }
  
  
  # 2. data event ####
  dE = data.frame(matrix(NA, nrow=children*max_occ, ncol=words+2))
  names(dE) = c('child', 'w_occ', paste0('u_', 1:words))
  dE$child = rep(1:children, each=max_occ)
  dE$w_occ = rep(1:max_occ, children)
  # dim(dE)
  # View(dE)
  
  # c=1; m=1
  for(c in 1:children){
    for(m in 1:max_occ){
      index1 = with(dF, child==c) # identify child in dF
      index2 = with(dE, child==c & w_occ==m) # identify child and event
      dE[index2, -c(1:2)] = colSums( dF[index1, -c(1:2)] == m )
    }
  }
  
  
  
  # 3. data probability ####
  dP = dE
  dP[,-c(1:2)] = dP[,-c(1:2)]/judges
  # dim(dP)
  # View(dP)
  
  
  
  # 4. data entropy ####
  dH = data.frame(matrix(NA, nrow=children, ncol=words+1))
  names(dH) = c('child', paste0('u_', 1:words))
  dH$child = 1:children
  
  # c=1; w=8
  for(c in 1:children){
    index1 = with(dP, child==c)
    index2 = with(dH, child==c)
    dH[index2, -1] = apply( dP[index1, -c(1:2)], 2, Hfun, N=judges)
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
  save(mom, file=file.path(file_save, file_name) )
  
}
