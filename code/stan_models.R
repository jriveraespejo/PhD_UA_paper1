# preliminar
rm(list=ls())

librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
               'reshape2','tinytex','gt','haven',
               'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
               'rethinking','rstan','coda','runjags','rjags',#'loo'
               'cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)


setwd('C:\Users\JRiveraEspejo\Desktop\1. Work\#Classes\PhD Antwerp\#thesis\paper1\code')



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