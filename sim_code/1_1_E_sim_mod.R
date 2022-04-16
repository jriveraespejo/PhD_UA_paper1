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
source( file.path( getwd(), 'sim_code', '1_2_E_sim_fun.R') )



# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
## prior ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    real<lower=0> s_SI;   // variability of SI
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    s_SI ~ exponential( 2 );
}
generated quantities {
    vector[I] m_SI;               // mean SI index (per child)
    vector[I] SI;                 // SI index
    vector[I] Ht;                 // average entropy (per child)
    vector[N] H;                  // replicated entropies
    
    m_SI = a + re_i;              // linear predictor
    SI = to_vector( normal_rng(m_SI, s_SI) );  // SI index
    Ht = inv_logit(-SI);          // average entropy (SI -> Ht: negative)
    
    // likelihood
    for(n in 1:N){
      H[n] = beta_proportion_rng( Ht[cid[n]], 10 );
    }
    
}
"

# saving
model_nam = "Hbeta_C_sim1_prior.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    vector[I] SI;         // SI index
    real<lower=0> s_SI;   // variability of SI
}
transformed parameters{
    vector[I] Ht;         // true entropy (per child)
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    vector[I] m_SI;       // mean SI index (per child)

    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    s_SI ~ exponential( 2 );
    
    // likelihood
    m_SI = a + re_i;          // linear predictor
    SI ~ normal(m_SI, s_SI);  // SI index
    
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
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // non-centered random interpcepts
    vector[I] z_SI;       // non-centered SI index
    real<lower=0> s_SI;   // variability of SI
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index
    vector[I] Ht;         // true entropy (per child)

    re_i = m_i + s_i*z_re;// non-centered RE
    m_SI = a + re_i;      // linear predictor
    SI = m_SI + s_SI*z_SI;// non-centered SI
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{

    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();
    s_SI ~ exponential( 2 );
    
    // likelihood
    z_SI ~ std_normal();      // non-centered SI index

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
## prior ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    real<lower=0> s_SI;   // variability of SI
    real<lower=0> m_M;    // dfs beta
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    m_M ~ lognormal( 1.5 , 0.5 );
    s_SI ~ exponential( 2 );
}
generated quantities {
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    vector[N] H;          // replicated entropies

    // linear predictor
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI = to_vector( normal_rng(m_SI, s_SI) );  // SI
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);
    
    // likelihood
    for(n in 1:N){
      H[n] = beta_proportion_rng( Ht[cid[n]], m_M );
    }
    
}
"

# saving
model_nam = "Hbeta_C_sim2_prior.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    vector[I] SI;         // SI index
    real<lower=0> s_SI;   // variability of SI
    real<lower=0> m_M;    // dfs beta
}
transformed parameters{
    vector[I] Ht;         // true entropy (per child)
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    vector[I] m_SI;       // mean SI index (per child)
    
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    m_M ~ lognormal( 1.5 , 0.5 );
    s_SI ~ exponential( 2 );
    
    // likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI ~ normal(m_SI, s_SI);  // SI index
    
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , m_M );
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
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercepts (per child) non-centered
    vector[I] z_SI;       // SI index
    real<lower=0> s_SI;   // variability of SI
    real<lower=0> m_M;    // df beta
}
transformed parameters{
    vector[I] re_i;       // random intercept (per child)
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    re_i = m_i + s_i*z_re;// non-centered 
    
    // linear predictor
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI = m_SI + s_SI*z_SI;// non-centered SI

    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    m_M ~ lognormal( 1.5 , 0.5 );
    s_SI ~ exponential( 2 );
    
    // likelihood
    z_SI ~ std_normal();      // non-centered SI index
    
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , m_M );
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
## prior ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    s_SI ~ exponential( 2 );
}
generated quantities {
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index
    vector[I] Ht;         // true entropy (per child)
    vector[N] H;          // replicated entropies

    // linear predictor
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI = to_vector( normal_rng(m_SI, s_SI) );  // SI
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);
    
    // likelihood
    for(n in 1:N){
      H[n] = beta_proportion_rng( Ht[cid[n]], M[cid[n]] );
    }
    
}
"

# saving
model_nam = "Hbeta_C_sim3_prior.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )





## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    vector[I] SI;         // SI index
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] Ht;         // true entropy (per child)
    Ht = inv_logit(-SI); // average entropy (SI -> Ht: negative)  
}
model{
    vector[I] m_SI;       // mean SI index (per child)

    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    s_SI ~ exponential( 2 );
    
    // likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI ~ normal(m_SI, s_SI);  // SI index
    
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
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    vector[I] z_SI;       // SI index
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // random effects and dfs
    re_i = m_i + s_i*z_re;
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI = m_SI + s_SI*z_SI;// non-centered SI
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    s_SI ~ exponential( 2 );
    
    // likelihood
    z_SI ~ std_normal();      // non-centered SI index
    
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
## prior ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercept
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
    s_SI ~ exponential( 2 );
}
generated quantities {
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index
    vector[I] Ht;         // true entropy (per child)
    vector[N] H;          // replicated entropies
    
    m_SI = a + re_i;      // linear predictor
    SI = to_vector( normal_rng(m_SI, s_SI) );  // SI index
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)

    // likelihood
    for(n in 1:N){
      H[n] = beta_proportion_rng( Ht[cid[n]], M[cid[n]] );
    }

}
"

# saving
model_nam = "Hbeta_C_sim4_prior.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercept
    vector[I] SI;         // SI index
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] Ht;         // true entropy (per child)
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    vector[I] m_SI;       // mean SI index (per child)

    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
    s_SI ~ exponential( 2 );

    // likelihood
    m_SI = a + re_i;          // linear predictor
    SI ~ normal(m_SI, s_SI);  // SI index
    
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
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercepts
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercepts (per child) noncentered
    vector[I] z_SI;       // SI index
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[I] M;          // df (per child)
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    
    // random effects and dfs
    re_i = m_i + s_i*z_re;
    M = exp( m_M + s_M*z_M );
    
    m_SI = a + re_i;      // linear predictor
    SI = m_SI + s_SI*z_SI;// non-centered SI
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();
    z_M ~ std_normal();
    s_SI ~ exponential( 2 );
    
    // likelihood
    z_SI ~ std_normal();      // non-centered SI index
    
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
## prior ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
model{
    // simple hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    s_SI ~ exponential( 2 );
}
generated quantities {
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index
    vector[I] Ht;         // true entropy (per child)
    vector[N] H;          // replicated entropies

    // linear predictor
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // when no interaction
    }
    SI = to_vector( normal_rng(m_SI, s_SI) );  // SI index
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);
    
    // likelihood
    for(n in 1:N){
      H[n] = beta_proportion_rng( Ht[cid[n]], M[cid[n]] );
    }
}
"

# saving
model_nam = "Hbeta_C_sim5_nocor_prior.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




## centered no cor ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    vector[I] SI;         // SI index
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] Ht;         // true entropy (per child)
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)  
}
model{
    vector[I] m_SI;       // mean SI index (per child)

    // simple hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    s_SI ~ exponential( 2 );
    
    // likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // when no interaction
    }
    SI ~ normal(m_SI, s_SI);  // SI index

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
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    vector[I] z_SI;       // SI index
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    
    // random effects and df's
    re_i = m_i + s_i*z_re;
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // when no interaction
    }
    SI = m_SI + s_SI*z_SI;// non-centered SI
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    s_SI ~ exponential( 2 );
    
    // likelihood
    z_SI ~ std_normal();      // non-centered SI index
    
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"


# saving
model_nam = "Hbeta_NC_sim5_nocor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )
