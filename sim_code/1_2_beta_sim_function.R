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
source( file.path( getwd(), 'sim_code', '0_beta_sim_extra.R') )




# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
# generating function
Hsim1 = function(I=32, K=10, seed=12345, 
                 par=list(mu_a=0, s_a=1) ){
  
  # initial parameters
  # I = 32 # children
  # K = 10 # utterances
  # seed=12345
  # par=list(mu_a=0, s_a=1)
  
  # simulation
  set.seed(seed)
  a = rnorm(I, par$mu_a, par$s_a)
  
  # storage 1
  Ht = data.frame(matrix(NA, nrow=I, ncol=3))
  names(Ht) = c('child','SI','Ht')
  Ht$child= 1:I
  Ht$SI = a # true SI index
  Ht$Ht = inv_logit(-Ht$SI) # true entropy (SI -> Ht: negative)
  
  # storage 2
  N = I*K
  H = data.frame(matrix(NA, nrow=N, ncol=3))
  names(H) = c('child','utterance','H')
  H$child = rep(1:I, each=K)
  H$utterance = rep(1:K, I)
  # str(H)
  
  # generating observed entropy
  # i=1
  for(i in 1:I){
    index = H$child==i
    set.seed(seed)
    H$H[index] = rbeta2(n=K, prob=Ht$Ht[i], theta=10)
  }
  # View(H)
  
  
  # data estimation
  dlist = list(
    N = nrow(H),
    K = max(H$utterance), # utterances
    I = nrow(Ht),
    H = with(H, ifelse(H==0, 0.0001, ifelse(H==1, 0.9999, H)) ), # trick
    cid = H$child )
  
  
  # storage
  data_storage = list(data_true=list(H=H, Ht=Ht, par=list(mu_a=par$mu_a, s_a=par$s_a, a=a)),
                      data_list=dlist)
  
  # save data
  file_name = 'Hbeta_sim1.RData'
  save(data_storage, file=file.path(getwd(), 'sim_data', file_name) )
  
}




# simulation 2: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   positive (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
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
Hsim2 = function(I=32, K=10, seed=12345, 
                 prop=c(0.38, 0.31, 0.31), # proportion of children
                 par=list( mu_a=0.5, s_a=0.2, aE=-0.1, 
                           aHS=-0.4, bP=-0.1, bA=0.15 ) ){
  
  # # initial parameters
  # I = 32 # children
  # K = 10 # utterances
  # seed=12345
  # prop=c(0.38, 0.31, 0.31)
  # par=list( mu_a=0.5, s_a=0.2, aE=-0.1, 
  #           aHS=-0.4, bP=-0.1, bA=0.15 )
  
  # storage 1
  Ht = data.frame(matrix(NA, nrow=I, ncol=6))
  names(Ht) = c('child','E','PTA','A','HS','SI')
  Ht$child = 1:I
  
  
  # generating relationships
  set.seed(seed)
  # prop=c(0.38, 0.31, 0.31)
  prop = round( prop*I )
  Ht$HS = c( rep(1, prop[1]), rep(2, prop[2]), rep(3, prop[3])) 
  Ht$A = c( rep(7, prop[1]), round(rnorm( sum(prop[2:3]), 5, 1)) ) 
  Ht$A = ifelse(Ht$A>7, 7, Ht$A)
  
  # no way to know true effects
  Ht$E = c( rep(1, prop[1]), 
            sample(2:3, size=prop[2], replace=T),
            sample(3:4, size=prop[3], replace=T)) 
  
  Ht$PTA = c( round(rnorm(prop[1], 60, 10)), # first 12 NH 
              round(rnorm(prop[2], 90, 10)), # last 20
              round(rnorm(prop[3], 110, 20)))
  
  
  # final effects
  set.seed(seed)
  a = rnorm(I, par$mu_a, par$s_a)
  aE = par$aE
  aHS = par$aHS
  bP = par$bP
  bA = par$bA
  
  Ht$SI = with(Ht, a + aE*E + aHS*HS + bA*( A - min(A) ) +
                 bP * c( standardize(PTA) ) )
  Ht$Ht = inv_logit(-Ht$SI) # true entropy (SI -> Ht: negative)
  # hist(Ht$SI, breaks=100, xlim=c(-1,1))
  # hist(Ht$Ht, breaks=100, xlim=c(0,1))
  # well distributed
  
  
  
  # storage 2
  N = I*K
  H = data.frame(matrix(NA, nrow=N, ncol=3))
  names(H) = c('child','utterance','H')
  H$child = rep(1:I, each=K)
  H$utterance = rep(1:K, I)
  # str(H)
  
  # generating observed entropy
  # i=1
  for(i in 1:I){
    index = H$child==i
    H$H[index] = rbeta2(n=K, prob=Ht$Ht[i], theta=10)
  }
  # View(H)
  
  
  # data list
  dlist = list(
    N = nrow(H),
    K = max(H$utterance), # utterances
    I = nrow(Ht),
    cHS = max(Ht$HS),
    cE = max(Ht$E),
    H = with(H, ifelse(H==0, 0.0001, ifelse(H==1, 0.9999, H)) ), # trick
    cid = H$child,
    HS = Ht$HS,
    A = with(Ht, A - min(A) ),
    E = Ht$E,
    PTA = c( standardize( Ht$PTA ) ) )
  
  
  # storage
  data_storage = list(data_true=list( H=H, Ht=Ht, par=list( par=par, a=a) ),
                      data_list=dlist)
  
  # save data
  file_name = 'Hbeta_sim2.RData'
  save(data_storage, file=file.path(getwd(), 'sim_data', file_name) )
  
}




# simulation 3: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation different M, zero/one values
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
Hsim3 = function(I=32, K=10, seed=12345,
                 prop=c(0.38, 0.31, 0.31), # proportion of children
                 par=list( mu_a=0.5, s_a=0.2, mu_the=1.5, s_the=0.5,
                           aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15 ) ){
  
  # # initial parameters
  # I = 32 # children
  # K = 10 # utterances
  # seed=12345
  # prop=c(0.38, 0.31, 0.31)
  # par=list( mu_a=0.5, s_a=0.2, mu_the=1.5, s_the=0.5,
  #           aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15 )
  
  # storage 1
  Ht = data.frame(matrix(NA, nrow=I, ncol=6))
  names(Ht) = c('child','E','PTA','A','HS','SI')
  Ht$child = 1:I
  
  
  # generating relationships
  set.seed(seed)
  # prop=c(0.38, 0.31, 0.31)
  prop = round( prop*I )
  Ht$HS = c( rep(1, prop[1]), rep(2, prop[2]), rep(3, prop[3])) 
  Ht$A = c( rep(7, prop[1]), round(rnorm( sum(prop[2:3]), 5, 1)) ) 
  Ht$A = ifelse(Ht$A>7, 7, Ht$A)
  
  # no way to know true effects
  Ht$E = c( rep(1, prop[1]), 
            sample(2:3, size=prop[2], replace=T),
            sample(3:4, size=prop[3], replace=T)) 
  
  Ht$PTA = c( round(rnorm(prop[1], 60, 10)), # first 12 NH 
              round(rnorm(prop[2], 90, 10)), # last 20
              round(rnorm(prop[3], 110, 20)))
  
  
  # final effects
  set.seed(seed)
  a = rnorm(I, par$mu_a, par$s_a)
  aE = par$aE
  aHS = par$aHS
  bP = par$bP
  bA = par$bA
  
  Ht$SI = with(Ht, a + aE*E + aHS*HS + bA*( A - min(A) ) +
                 bP * c( standardize(PTA) ) )
  Ht$Ht = inv_logit(-Ht$SI) # true entropy (SI -> Ht: negative)
  
  Ht$M = rlnorm(I, meanlog = par$mu_the, sdlog= par$s_the) # degrees of freedom
  Ht$M = round(Ht$M)
  # hist(Ht$SI, breaks=100, xlim=c(-1,1))
  # hist(Ht$Ht, breaks=100, xlim=c(0,1))
  # well distributed
  
  
  # storage 2
  N = I*K
  H = data.frame(matrix(NA, nrow=N, ncol=3))
  names(H) = c('child','utterance','H')
  H$child = rep(1:I, each=K)
  H$utterance = rep(1:K, I)
  # str(H)
  
  # generating observed entropy
  # i=1
  for(i in 1:I){
    index = H$child==i
    H$H[index] = rbeta2(n=K, prob=Ht$Ht[i], theta=Ht$M[i])
  }
  # View(H)
  
  
  # data list
  dlist = list(
    N = nrow(H),
    K = max(H$utterance), # utterances
    I = nrow(Ht),
    cHS = max(Ht$HS),
    cE = max(Ht$E),
    H = with(H, ifelse(H==0, 0.0001, ifelse(H==1, 0.9999, H)) ), # trick
    cid = H$child,
    HS = Ht$HS,
    A = with(Ht, A - min(A) ),
    E = Ht$E,
    PTA = c( standardize( Ht$PTA ) ) )
  
  
  # storage
  data_storage = list(data_true=list( H=H, Ht=Ht, par=list( par=par, a=a) ),
                      data_list=dlist)
  
  # save data
  file_name = 'Hbeta_sim3.RData'
  save(data_storage, file=file.path(getwd(), 'sim_data', file_name) )
  
}





# simulation 4: ####
# 
# details:
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
# simulation function
Hsim4 = function( children=32, words=10, judges=100, max_occ=50 ){
  
  # # data
  # children = 32
  # words = 10 # utterances
  # judges = 100
  # max_occ=50
  
  
  # full transcriptions (diff. numbers count as diff. words)
  full_data = data.frame(matrix(NA, nrow=children*judges, ncol=words+2))
  names(full_data) = c('child', 'judge', paste0('u_', 1:words))
  full_data$child = rep(1:children, each=judges)
  full_data$judge = rep(1:judges, children)
  # dim(full_data)
  # View(full_data)
  
  # c=1; w=10
  for(c in 1:children){
    for(w in 1:words){
      index = with(full_data, child==c) # identify child 
      word_occ = sample(size=1, x=1:max_occ, replace=T) # simulate word occurence
      full_data[index, w+2] = round( runif(judges, min=1, max=word_occ) )
    }
  }
  
  
  # event data
  event_data = data.frame(matrix(NA, nrow=children*max_occ, ncol=words+2))
  names(event_data) = c('child', 'w_occ', paste0('u_', 1:words))
  event_data$child = rep(1:children, each=max_occ)
  event_data$w_occ = rep(1:max_occ, children)
  # dim(event_data)
  # View(event_data)
  
  # c=1; m=1
  for(c in 1:children){
    for(m in 1:max_occ){
      index1 = with(full_data, child==c) # identify child in full_data
      index2 = with(event_data, child==c & w_occ==m) # identify child and event
      event_data[index2, -c(1:2)] = colSums( full_data[index1, -c(1:2)] == m )
    }
  }
  
  # probability data
  prob_data = event_data
  prob_data[,-c(1:2)] = prob_data[,-c(1:2)]/judges
  # dim(prob_data)
  # View(prob_data)
  
  # entropy calculation
  entropy_data = data.frame(matrix(NA, nrow=children, ncol=words+1))
  names(entropy_data) = c('child', paste0('u_', 1:words))
  entropy_data$child = 1:children
  
  # c=1; w=8
  for(c in 1:children){
    index1 = with(prob_data, child==c)
    index2 = with(entropy_data, child==c)
    entropy_data[index2, -1] = apply( prob_data[index1, -c(1:2)], 2, Hfun, N=judges)
  }
  
  
  # data list
  H = melt(entropy_data, id.vars='child')
  H = H[order(H$child),]
  
  dlist = list(
    N = nrow( H ),
    K = length( unique( H$variable ) ), # utterances
    I = max( H$child ),
    H = with(H, ifelse(value==0, 0.0001, ifelse(value==1, 0.9999, value)) ),
    cid = H$child )
  # notice trick to handle zeroes/ones
  
  
  
  # storage
  data_storage = list(data_true = list(full_data=full_data, 
                                       event_data=event_data, 
                                       prob_data=prob_data,
                                       entropy_data=entropy_data),
                      data_list = dlist )
  
  # save data
  file_name = 'Hbeta_sim4.RData'
  save(data_storage, file=file.path(getwd(), 'sim_data', file_name) )
  
}




# simulation 5: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation different M, zero/one values
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
Hsim5 = function(I=32, K=10, seed=12345,
                 prop=c(0.38, 0.31, 0.31), # proportion of children
                 par=list( mu_a=0.5, s_a=0.2, mu_the=1.5, s_the=0.5,
                           aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ) ){
  
  # # initial parameters
  # I = 32 # children
  # K = 10 # utterances
  # seed=12345
  # prop=c(0.38, 0.31, 0.31)
  # par=list( mu_a=0.5, s_a=0.2, mu_the=1.5, s_the=0.5,
  #           aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 )
  
  # storage 1
  Ht = data.frame(matrix(NA, nrow=I, ncol=6))
  names(Ht) = c('child','E','PTA','A','HS','SI')
  Ht$child = 1:I
  
  
  # generating relationships
  set.seed(seed)
  # prop=c(0.38, 0.31, 0.31)
  prop = round( prop*I )
  Ht$HS = c( rep(1, prop[1]), rep(2, prop[2]), rep(3, prop[3])) 
  Ht$A = c( rep(7, prop[1]), round(rnorm( sum(prop[2:3]), 5, 1)) ) 
  Ht$A = ifelse(Ht$A>7, 7, Ht$A)
  
  # no way to know true effects
  Ht$E = c( rep(1, prop[1]), 
            sample(2:3, size=prop[2], replace=T),
            sample(3:4, size=prop[3], replace=T)) 
  
  Ht$PTA = c( round(rnorm(prop[1], 60, 10)), # first 12 NH 
              round(rnorm(prop[2], 90, 10)), # last 20
              round(rnorm(prop[3], 110, 20)))
  
  
  # final effects
  set.seed(seed)
  a = rnorm(I, par$mu_a, par$s_a)
  aE = par$aE
  aHS = par$aHS
  bP = par$bP
  bA = par$bA
  bAHS = par$bAHS # it has to be very small to make sense abs([0, 0.015])
  
  # # example
  # data_mom = data.frame( eff = with(Ht, -0.015*( A - min(A) )*HS))
  # data_mom$HS = as.integer(Ht$HS)
  # data_mom %>%
  #   group_by(HS) %>%
  #   summarise(mean = mean(eff, na.rm=T), sd=sd(eff, na.rm=T))
  
  Ht$SI = with(Ht, a + aE*E + aHS*HS + bA*( A - min(A) ) + 
                 bAHS*( A - min(A) )*HS +  
                 bP * c( standardize(PTA) ) )
  Ht$Ht = inv_logit(-Ht$SI) # true entropy (SI -> Ht: negative)
  
  Ht$M = rlnorm(I, meanlog = par$mu_the, sdlog= par$s_the) # degrees of freedom
  Ht$M = round(Ht$M)
  # hist(Ht$SI, breaks=100, xlim=c(-1,1))
  # hist(Ht$Ht, breaks=100, xlim=c(0,1))
  # well distributed
  
  
  # storage 2
  N = I*K
  H = data.frame(matrix(NA, nrow=N, ncol=3))
  names(H) = c('child','utterance','H')
  H$child = rep(1:I, each=K)
  H$utterance = rep(1:K, I)
  # str(H)
  
  # generating observed entropy
  # i=1
  for(i in 1:I){
    index = H$child==i
    H$H[index] = rbeta2(n=K, prob=Ht$Ht[i], theta=Ht$M[i])
  }
  # View(H)
  
  
  # data list
  dlist = list(
    N = nrow(H),
    K = max(H$utterance), # utterances
    I = nrow(Ht),
    cHS = max(Ht$HS),
    cE = max(Ht$E),
    H = with(H, ifelse(H==0, 0.0001, ifelse(H==1, 0.9999, H)) ), # trick
    cid = H$child,
    HS = Ht$HS,
    A = with(Ht, A - min(A) ),
    E = Ht$E,
    PTA = c( standardize( Ht$PTA ) ) )
  
  
  # storage
  data_storage = list(data_true=list( H=H, Ht=Ht, par=list( par=par, a=a) ),
                      data_list=dlist)
  
  # save data
  file_name = 'Hbeta_sim5.RData'
  save(data_storage, file=file.path(getwd(), 'sim_data', file_name) )
  
}
