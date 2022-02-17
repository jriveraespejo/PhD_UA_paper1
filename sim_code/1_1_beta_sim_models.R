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
    real a;               // fixed intercept
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a_i;        // random intercepts (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a + a_i;         // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    a ~ normal( 1 , 0.5 );
    a_i ~ normal( mu_a , sigma_a );
    
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
    real a;               // fixed intercept
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // non-centered random interpcepts
}
transformed parameters{
    vector[I] a_i;        // random intercepts (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)

    a_i = mu_a + sigma_a * z_a; // non-centering
    SI = a + a_i;         // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    a ~ normal(0, 0.5);
    z_a ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# saving
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
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a_i;        // random intercepts (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
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
    a ~ normal( 0 , 0.5 );
    a_i ~ normal( mu_a , sigma_a );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# saving
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
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // random intercepts (per child) non-centered
}
transformed parameters{
    vector[I] a_i;        // random intercept (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    a_i = mu_a + sigma_a * z_a; // non-centering
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      //SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
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
    a ~ normal(0 , 0.5);
    z_a ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# saving
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
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a_i;        // random intercepts (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
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
    a ~ normal( 0 , 0.5 );
    a_i ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# saving
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
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // random intercept (per child) noncentered
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] a_i;        // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    a_i = mu_a + sigma_a * z_a;
    M = exp( mu_the + sigma_the * z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
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
    a ~ normal( 0 , 0.5 );
    z_a ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# saving
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
    real a;               // fixed intercept
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a_i;        // random intercept
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a + a_i;         // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.5 );
    a_i ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# saving
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
    real a;               // fixed intercepts
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // random intercepts (per child) noncentered
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] a_i;        // random intercepts (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    
    a_i = mu_a + sigma_a * z_a;
    M = exp( mu_the + sigma_the * z_M );
    
    SI = a + a_i;         // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.5 );
    z_a ~ std_normal();
    z_M ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# saving
model_nam = "Hbeta_NC_sim4.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




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
## centered no cor ####
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
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a_i;        // random intercepts (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // simple hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    a_i ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# saving
model_nam = "Hbeta_C_sim5_nocor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )



## non-centered no cor ####
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
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // random intercept (per child) noncentered
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] a_i;        // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // random effects and df's
    a_i = mu_a + sigma_a * z_a;
    M = exp( mu_the + sigma_the * z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
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
    a ~ normal( 0 , 0.5 );
    z_a ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"


# saving
model_nam = "Hbeta_NC_sim5_nocor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




## centered cor ####
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
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real mu_aHS;          // hyperparameter for aHS
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real mu_bAHS;         // hyperparameter for bAHS
    vector<lower=0>[2] sigma_abHS; // variability for aHS and bAHS
    corr_matrix[2] Rho;   // correlation matrix for aHS and bAHS
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a_i;        // random intercepts (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // parameter not to follow
    vector[2] mu_abHS;    // hyperprior mean for aHS and bAHS
    matrix[2,2] S_abHS;   // hyperprior SD for aHS and bAHS
    vector[2] RE[cHS];    // declare storage for fixed effects
    // first are columns, and then rows
    
    
    // simple hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // hyperprior for correlated fixed effects
    mu_aHS ~ normal( 0 , 0.5 ); 
    mu_bAHS  ~ normal( 0 , 0.5 );
    sigma_abHS ~ exponential( 1 );
    Rho ~ lkj_corr( 2 );  

    mu_abHS = [mu_aHS , mu_bAHS]';
    S_abHS = quad_form_diag(Rho, sigma_abHS);
    for ( c in 1:cHS ){ 
      RE[c] = [aHS[c], bAHS[c]]';      // storage first fixed effects
    }
    RE ~ multi_normal( mu_abHS , S_abHS ); 

    
    // priors
    a ~ normal( 0 , 0.5 );
    a_i ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );
    //aE ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# saving
model_nam = "Hbeta_C_sim5_cor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )





## non-centered cor ####
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
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[2, cHS] z_abHS;// matrix of (2x4)
    real mu_aHS;          // hyperparameter for aHS
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    real mu_bAHS;         // hyperparameter for bAHS
    vector<lower=0>[2] sigma_abHS; // variability for aHS and bAHS
    cholesky_factor_corr[2] L_Rho; // cholesky factor for correlation matrix for aHS and bAHS
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // random intercept (per child) noncentered
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[2] mu_abHS;    // hyperprior mean for aHS and bAHS
    matrix[2, cHS] RE;    // correlated fixed effects (2x4)
    vector[cHS] aHS;      // fixed intercept (per HS)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    vector[I] a_i;        // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    matrix[2, 2] Rho;     // correlation matrix

    // random effects and df's
    a_i = mu_a + sigma_a * z_a;
    M = exp( mu_the + sigma_the * z_M );
    
    // correlated fixed effects
    mu_abHS = [mu_aHS , mu_bAHS]';
    RE = (diag_pre_multiply(sigma_abHS, L_Rho) * z_abHS)';
    for(c in 1:cHS){
      RE[c] = to_row_vector(mu_abHS) + RE[c];
    }
    aHS = RE[,1];
    bAHS = RE[,2];
    
    // correlation matrix
    Rho = multiply_lower_tri_self_transpose(L_Rho);
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + (bA + bAHS[HS[i]])*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    
    // simple hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // hyperprior for correlated fixed effects
    mu_aHS ~ normal( 0 , 0.5 ); 
    mu_bAHS  ~ normal( 0 , 0.5 );
    sigma_abHS ~ exponential( 1 );
    L_Rho ~ lkj_corr_cholesky( 2 ); 

    
    // priors
    a ~ normal( 0 , 0.5 );
    z_a ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    to_vector( z_abHS ) ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# saving
model_nam = "Hbeta_NC_sim5_cor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )
