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
CJDsim = function(file_save, file_name, # file_save need to include getwd()
                  I=32, K=10, D=20, J=80, seed=12345,
                  p=c(0.38, 0.31, 0.31), # children prop. on each group
                  par=list( m_i=0, s_i=0.5, # children's random effects
                            m_j=0, s_j=0.5, # judges' random effects
                            s_SI=0.1, # variability in children's observed SIs (vector[I] or constant)
                            s_HJ=0.1, # var. in observed HJo (constant)
                            a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15, bAHS=0 ) ){
  
  # test
  I = 32 # experimental units (children)
  K = 10 # replicates (utterances)
  D = 20 # duplicates (comparisons/assessments per I and K)
  J = 80 # number of judges
  seed=12345
  p=c(0.38, 0.31, 0.31)
  par=list( m_i=0, s_i=0.5, # children's random effects
            m_j=0, s_j=0.5, # judges' random effects
            s_SI=0.1, # var. in children's observed SIs (vector[I] or constant)
            s_HJ=0.1, # var. in observed HJo (constant)
            a=0, aE=0, aHS=0, bP=0, bA=0, bAHS=0 )
  #         a=0, aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15
  
  
  # 1. true data ####
  dT = data.frame(matrix(NA, nrow=I, ncol=8+K))
  names(dT) = c('child_id','E','PTA','A','HS','re_i','m_SI','s_SI',
                paste0('u',1:K,'_SI'))
  dT$child_id = 1:I
  
  
  # generating covariates
  set.seed(seed)
  n = round( p*I )
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
  set.seed(seed+1)
  re_i = rnorm(I, mean=par$m_i,sd=par$s_i)
  dT$re_i = re_i 
  
  # linear predictor / SI index
  dT$m_SI = with(dT, re_i + par$a + par$aE*E + par$aHS*HS +
                   par$bA*(A - min(A)) +
                   par$bAHS*(A - min(A))*HS + 
                   par$bP * c( standardize(PTA) ) )
  
  # variability of SI
  dT$s_SI = par$s_SI # vector[I] or constant
  
  
  # generating utterance values
  start = min(which(str_detect(names(dT), 'u[:digit:]{1,2}[:punct:]')))-1
  set.seed(seed+2)
  for(i in 1:I){
    dT[i, 1:K + start] = rnorm(n=K, mean=dT$m_SI[i], sd=dT$s_SI[i])
  }
  
  # rounding
  dT[,6:ncol(dT)] = round( dT[,6:ncol(dT)], 5)
  
  
  
  # 3. full data ####
  
  # comparisons
  comp = expand.grid(child_id1=1:I, child_id2=1:I)
  idx = with(comp, child_id1==child_id2)
  comp = comp[!idx,]
  comp = comp[with(comp, order(child_id1, child_id2)),]
  comp$eq = with(comp, ifelse( child_id1<child_id2, 
                               paste(child_id1, child_id2),
                               paste(child_id2, child_id1) ) )
  comp = comp[!duplicated(comp$eq),]
  comp = comp[,-3]
  Dt = nrow(comp)
  
  # storage
  N = Dt*K*J
  dF = data.frame(matrix(NA, nrow=N, ncol=10))
  names(dF) = c('child_id1','child_id2','utt_id','judge_id','re_j','SI1','SI2','m_CJD','p_CJD','CJD')
  dF[,c('child_id1','child_id2')] = comp[rep(1:nrow(comp), each=K*J),]
  dF$utt_id = rep(1:K, Dt*J)
  dF = dF[with(dF, order(child_id1, child_id2, utt_id) ),]
  dF$judge_id = 1:J

  # judges' random effects
  set.seed(seed-2)
  re_j = round( rnorm(J, mean=par$m_j, sd=par$s_j), 5)
  dF$re_j = re_j[dF$judge_id]
  
  # pasting SI
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
  set.seed(seed+3)
  dF$CJD = rbinom(N, size=1, prob=dF$p_CJD)
  
  
  # selection probability
  # to do
  nam = with(dF, paste(child_id1, child_id2))
  nam_count = table(nam) 

  
  # 4. sampled data ####
  # only D comparisons per judge
  # j=1; k=3
  for(j in 1:J){
    for(k in 1:k){
      
      # identify judge
      idx = with(dF, which(judge_id==j & utt_id==k) )
      
      # sample D comparisons from judge j
      idx = sample(idx, size=D/2, replace=F)
      
      if(j==1 & k==1){
        dO = dF[idx,]
      } else{
        dO = rbind(dO, dF[idx,])
      }
      
    }
    
  }
  
  dO = dO[with(dO, order(child_id1, child_id2, utt_id, judge_id) ),]
  # table(dO$judge_id)
  # table(dO$child_id1)
  # table(dO$child_id2)
  # table(dO$utt_id)

  
  # 5. reduced data ####
  dR = dO %>%
    group_by(child_id, utt_id) %>%
    summarise(m_sSIo=mean( SIo/100 ), s_sSIo=sd( SIo/100 ) )
  
  
  
  # 6. list data ####
  dL = list(
    # dimensions
    N = nrow(dO), # observations
    I = max(dO$child_id), # children
    K = max(dO$utt_id), # utterances
    J = max(dO$judge_id), # judges
    D = max(dO$dup_id), # duplicate
    R = max(dO$child_id) * max(dO$utt_id), # total replicates
    
    # category numbers
    cHS = max(dT$HS),
    cE = max(dT$E),
    
    # child's data
    HS = dT$HS,
    Am = with(dT, A-min(A) ), # centered at minimum
    E = dT$E,
    sPTA = c( standardize( dT$PTA ) ),
    
    # full observed data
    # HJ = with(dO, ifelse(SIo==0, 0.001, ifelse(SIo==100, 0.999, SIo/100)) ), # bounded
    HJ = with(dO, c( scale(SIo) ) ), # standardized
    cid = dO$child_id,
    uid = dO$utt_id,
    jid = dO$judge_id,
    
    # reduced data
    m_HJ = dR$m_sSIo,
    s_HJ = dR$s_sSIo,
    rcid = dR$child_id,
    ruid = dR$utt_id
  )
  
  
  # 7. save data ####
  mom = list(dS=list( dT=dT, dO=dO, dR=dR, par=par), dL=dL)
  save(mom, file=file.path(file_save, file_name) )
  
}  
