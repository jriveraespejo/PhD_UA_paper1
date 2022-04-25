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
source( file.path( getwd(), 'sim_code', '1_2_E_sim_fun.R') )



# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
## NC1 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // non-centered random interpcepts
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST

    re_i = m_i + s_i*z_re;// non-centered RE
    b_i = m_b + s_b*z_b;  // non-centered blocks
    SI = a + re_i;        // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , 10 );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n] , 10 );
    }
}
"

# saving
model_nam = "E_NC1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )






# simulation 2: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   positive (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## NC2a ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    real bP;              // fixed slope standardized PTA
    vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercepts (per child) non-centered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    //real<lower=0> m_M;    // df beta
}
transformed parameters{
    vector[I] re_i;       // random intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    re_i = m_i + s_i*z_re;// non-centered 
    b_i = m_b + s_b*z_b;  // non-centered blocks
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    //m_M ~ lognormal( 1.5 , 0.5 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , 10 );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n] , 10 );
    }
}
"

# saving
model_nam = "E_NC2a.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )





## NC2b ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    real bP;              // fixed slope standardized PTA
    vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercepts (per child) non-centered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real<lower=0> m_M;    // df beta
}
transformed parameters{
    vector[I] re_i;       // random intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    re_i = m_i + s_i*z_re;// non-centered 
    b_i = m_b + s_b*z_b;  // non-centered blocks
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ lognormal( 1.5 , 0.5 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , m_M );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n] , m_M );
    }
}
"

# saving
model_nam = "E_NC2b.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )








# simulation 3: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation different M, zero/one values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   dSI/dAm > 0 (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## NC3 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    real bP;              // fixed slope standardized PTA
    vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] M;          // df (per child)
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects and dfs
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();
    z_M ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , M[cid[n]] );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n] , M[cid[n]] );
    }
}
"

# saving
model_nam = "E_NC3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )







# simulation 5: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation different M, zero/one values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   dSI/dAm > 0 (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# Am * HS -> SI: 
#   dSI/dAm[HS=NH] = dSI/dAm[HI/CI] = dSI/dAm[HS=HI/HA] = 0 
#   (no different evolution)
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## NC5a1 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    //matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    //real<lower=0> m_M;    // mean of df
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aHS[HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aHS[HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    //m_M ~ lognormal( 1.5 , 0.5 );

    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    //to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , 10 );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n] , 10 );
    }
}
"


# saving
model_nam = "E_NC5a1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )





## NC5a2 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    //vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[cE, cHS] aEHS; // fixed interaction E*HS
    real bA;              // fixed slope (A - A_min) (if no different effect)
    //vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    //real<lower=0> m_M;    // mean of df
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bA*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aEHS[E[i], HS[i]] +
      //        bA*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    //m_M ~ lognormal( 1.5 , 0.5 );

    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    //aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    to_vector(aEHS) ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , 10 );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n] , 10 );
    }
}
"


# saving
model_nam = "E_NC5a2.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )






## NC5a3 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    //vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    //real<lower=0> m_M;    // mean of df
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aEHS[E[i], HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    //m_M ~ lognormal( 1.5 , 0.5 );

    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    //aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , 10 );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n], 10 );
    }
}
"


# saving
model_nam = "E_NC5a3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )






## NC5b1 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    //matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real<lower=0> m_M;    // mean of df
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aHS[HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aHS[HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ lognormal( 1.5 , 0.5 );

    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    //to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , m_M );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n], m_M );
    }
}
"


# saving
model_nam = "E_NC5b1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )





## NC5b2 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    //vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[cE, cHS] aEHS; // fixed interaction E*HS
    real bA;              // fixed slope (A - A_min) (if no different effect)
    //vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real<lower=0> m_M;    // mean of df
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bA*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aEHS[E[i], HS[i]] +
      //        bA*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ lognormal( 1.5 , 0.5 );

    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    //aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    to_vector(aEHS) ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , m_M );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n], m_M );
    }
}
"


# saving
model_nam = "E_NC5b2.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )






## NC5b3 ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    //vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real<lower=0> m_M;    // mean of df
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aEHS[E[i], HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ lognormal( 1.5 , 0.5 );

    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    //aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , m_M );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n], m_M );
    }
}
"


# saving
model_nam = "E_NC5b3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )







# simulation 6: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation different M, zero/one values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# sPTA -> HS:
#   positive
#   sPTA=L -> HS=NH, sPTA=M1|M2 -> HS=HI/HA, sPTA=M2|H -> HS=HI/CI
#   sPTA range, L=low, M1<M2=mid, H=high
# Am -> SI: 
#   dSI/dAm > 0 (more Am, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# Am * HS -> SI: 
#   dSI/dAm[HS=NH] = dSI/dAm[HI/CI] = dSI/dAm[HS=HI/HA] = 0 
#   (no different evolution)
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# sPTA -> SI:
#   negative (more sPTA, less SI)
#
#   ideally is non-linear
#   SI[sPTA=L] > SI[sPTA=H] > SI[sPTA=M1|M2]
#   sPTA range, L=low, M1<M2=mid, H=high
#
## NC6a ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    //matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] M;          // df (per child)
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects and df's
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aHS[HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aHS[HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    //to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();
    z_M ~ std_normal();
  
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , M[cid[n]] );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n], M[cid[n]] );
    }
}
"


# saving
model_nam = "E_NC6a.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )





## NC6c ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    //vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[cE, cHS] aEHS; // fixed interaction E*HS
    real bA;              // fixed slope (A - A_min) (if no different effect)
    //vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] M;          // df (per child)
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects and df's
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bA*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aEHS[E[i], HS[i]] +
      //        bA*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    //aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    to_vector(aEHS) ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();
    z_M ~ std_normal();
  
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , M[cid[n]] );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n], M[cid[n]] );
    }
}
"


# saving
model_nam = "E_NC6b.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )






## NC6c ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int B;                // number of blocks
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int bid[N];           // block id
    int HS[I];            // hearing status 
    real Am[I];           // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    //vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_b;             // mean of block
    real<lower=0> s_b;    // variability of block
    vector[B] z_b;        // non-centered blocks
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[B] b_i;        // block effects
    vector[I] M;          // df (per child)
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    vector[N] mu;         // OF NO INTEREST
    
    // random effects and df's
    re_i = m_i + s_i*z_re;
    b_i = m_b + s_b*z_b;  // non-centered blocks
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aEHS[E[i], HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
    
    for(n in 1:N){
      mu[n] = inv_logit( b_i[bid[n]] - SI[cid[n]] );
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_b ~ normal( 0 , 0.2 );
    s_b ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    //aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();
    z_b ~ std_normal();
    z_M ~ std_normal();
  
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( mu[n] , M[cid[n]] );
    }
}
generated quantities{
    vector[N] log_lik;
    
    // log-likelihood
    for(n in 1:N){
      log_lik[n] = beta_proportion_lpdf( H[n] | mu[n], M[cid[n]] );
    }
}
"


# saving
model_nam = "E_NC6c.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'real_models', model_nam) )
