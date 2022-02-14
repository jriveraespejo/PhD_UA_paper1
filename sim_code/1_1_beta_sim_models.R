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




# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    a ~ normal( mu_a , sigma_a );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# saving
model_nam = "Hbeta_C_sim1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )





## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // non-centered a
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)

    a = mu_a + sigma_a * z_a; // non-centering
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# cmdstan
model_nam = "Hbeta_NC_sim1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






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
## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept (per child)
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    a ~ normal( mu_a , sigma_a );
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# cmdstan
model_nam = "Hbeta_C_sim2.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // intercept (per child) non-centered
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    a = mu_a + sigma_a * z_a; // non-centering
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      //SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# cmdstan
model_nam = "Hbeta_NC_sim2.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






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
## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // priors
    a ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# cmdstan
model_nam = "Hbeta_C_sim3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )





## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // noncentered intercept (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    a = mu_a + z_a * sigma_a;
    M = exp( mu_the + z_M * sigma_the );
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    z_M ~ std_normal();
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# cmdstan
model_nam = "Hbeta_NC_sim3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






# simulation 4 ####
# 
# details:
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );

    
    // priors
    a ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );

    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# cmdstan
model_nam = "Hbeta_C_sim4.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )





## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // noncentered intercept (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    
    a = mu_a + z_a * sigma_a;
    M = exp( mu_the + z_M * sigma_the );
    
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );

    
    // priors
    z_a ~ std_normal();
    z_M ~ std_normal();


    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# cmdstan
model_nam = "Hbeta_NC_sim4.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )
