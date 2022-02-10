# preliminar ####
rm(list=ls())

librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
               'reshape2','tinytex','gt','haven',
               'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
               'rethinking','rstan','coda','runjags','rjags',#'loo'
               'cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)


setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1/code')




# simulation 1 ####
# 
# details:
# Covariates: not modeled
#
# entropy function
H = function(p, N){
  -sum( ifelse(p == 0, 0,  p*log2(p) ) )/log2(N)
}
#
# simulation function
H_sim = function( children=32, words=10, judges=100, max_occ=100 ){
  
  # # data
  # children = 32
  # words = 10 # utterances
  # judges = 100
  # max_occ=10
  
  
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
    entropy_data[index2, -1] = apply( prob_data[index1, -c(1:2)], 2, H, N=judges)
  }
  
  # results report
  return( list(full_data=full_data, 
               event_data=event_data, 
               prob_data=prob_data,
               entropy_data=entropy_data) )
}


H_data = H_sim()$entropy_data
H_data_long = melt(H_data, id.vars = 'child')
H_data_long = H_data_long[order(H_data_long$child),]


# plot data
with(H_data_long, 
     plot( value, child, pch=19, xlim=c(0,1),
           xlab='entropy', ylab='child id') )
abline(h = H_data_long$child, lty=2)



# data list
dlist = list(
  N = nrow( H_data_long ),
  K = 10, # utterances
  I = length( unique( H_data_long$child ) ),
  H = H_data_long$value,
  cid = H_data_long$child
)


# measurement error model
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    vector[I] a;          // intercept
    vector[I] SI;         // true SI index (per child)
    real<lower=0> sigma;  // true Si variability
    real<lower=0> kappa;  // extra degrees
}
transformed parameters{
    vector[I] Ht;         // true entropy (per child)
    real M;               // *sample size*, should be close to 10?

    Ht = inv_logit(SI);
    M = kappa + 9.0;
}
model{
    // parameter not followed
    vector[I] mu;
    
    // priors
    a ~ normal( 0 , 0.5 );
    sigma ~ exponential( 4 );
    kappa ~ exponential( 2 );
    
    // linear predictor (vectorized)
    mu = a;
    
    // likelihood (vectorized)
    SI ~ normal( mu , sigma );
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M );
    }
    //H ~ beta_proportion( Ht[cid] , 10 ); // model to test
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "H_me.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
H_me = rstan::read_stan_csv( fit$output_files() )
# precis( H_me, depth=1 )


















# final data ####

# data load
file_dir = 'C:\Users\JRiveraEspejo\Desktop\1. Work\#Classes\PhD Antwerp\#thesis\#data'
dH = read_csv(file.path(file_dir, 'name.csv'))
# str(dH)



# data transform
var_int = c('children_id', 'hearing_status', 'hearing_age', 'etiology', 'pta')
data_mom = unique( dH[, var_int] )
data_mom = data_mom[order(data_mom$children_id),]



# data list
dlist = list(
  N = nrow( dH ),
  K = 10, # utterances
  I = length( unique( dH$children_id ) ),
  cHS = length( unique( dH$hearing_status ) ),
  cE = length( unique( dH$etiology ) ),
  H = dH$entropy,
  cid = dH$children_id,
  HS = data_mom$hearing_status,
  A = data_mom$hearing_age,
  E = data_mom$etiology,
  PTA = data_mom$pta
)



# measurement error model
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int H[N];             // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    int PTA[I];           // pta values
}
parameters{
    vector[cHS] a;        // intercepts, per HS
    vector[cHS] bA;       // slope for A, per HS
    vector[cE] aE;        // intercepts, per E
    real bP;              // slope for PTA values
    vector[I] SI;         // true SI index (per child)
    real<lower=0> sigma;  // true Si variability
}
transformed parameters{
    real A_bar;           // minimum age in sample
    vector[I] Ht;         // true entropy (per child)
    vector[I] M;          // *sample size*, should be close to 10?

    A_bar = min(A);       
    Ht = inv_logit(SI);
    M = kappa + 9;
}
model{
    // parameter not followed
    vector[I] mu;
    vector[I] kappa;
    
    // priors
    a ~ normal( 0 , 0.5 );
    bA ~ normal( 0 , 0.3 );
    aE ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    sigma ~ exponential( 1 );
    kappa ~ exponential( 2 );
    
    // linear predictor (vectorized)
    mu = a[HS] + bA[HS] * (A - A_bar) + aE[E] + bP * PTA;
    
    
    // likelihood (vectorized)
    SI ~ normal( mu , sigma );
    H ~ beta_proportion( Ht[cid] , M[cid] );
    //H ~ beta_proportion( Ht[cid] , 10 ); // model to test
}
"

# # cmdstan
# # set_cmdstan_path('~/cmdstan') # in case need it
# save_code = "H_me.stan"
# writeLines(mcmc_code, con=file.path(getwd(), 'txt', save_code) )
# mod = cmdstan_model( file.path(getwd(), 'txt', save_code) )
# fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
# H_me = rstan::read_stan_csv( fit$output_files() )
# # precis( H_me, depth=1 )
