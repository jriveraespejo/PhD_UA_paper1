# preliminar ####
rm(list=ls())

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
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a + re_i;        // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    
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
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)

    re_i = m_i + s_i*z_re;// non-centering
    SI = a + re_i;        // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal(0, 0.5);
    z_re ~ std_normal();
    
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
    int Am[I];             // hearing age
    int E[I];             // etiology
    real sPTA[I];          // (standardized) pta values
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
    real<lower=0> m_M;    // dfs beta
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    m_M ~ lognormal( 1.5 , 0.5 );
    
    // likelihood
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
    int Am[I];             // hearing age
    int E[I];             // etiology
    real sPTA[I];          // (standardized) pta values
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
    real<lower=0> m_M;    // df beta
}
transformed parameters{
    vector[I] re_i;       // random intercept (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    re_i = m_i + s_i*z_re;// non-centering
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal(0 , 0.5);
    z_re ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    m_M ~ lognormal( 1.5 , 0.5 );
    
    // likelihood
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
    int Am[I];             // hearing age
    int E[I];             // etiology
    real sPTA[I];          // (standardized) pta values
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
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
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
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // random effects and dfs
    re_i = m_i + s_i*z_re;
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
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    z_re ~ std_normal();
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
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a + re_i;        // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );

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
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    
    // random effects and dfs
    re_i = m_i + s_i*z_re;
    M = exp( m_M + s_M*z_M );
    
    SI = a + re_i;        // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.5 );
    z_re ~ std_normal();
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
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // when no interaction
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // simple hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
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
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // random effects and df's
    re_i = m_i + s_i*z_re;
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // when no interaction
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    z_re ~ std_normal();
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




# ## centered cor ####
# mcmc_code = "
# data{
#     int N;                // experimental runs
#     int I;                // experimental units (children)
#     int K;                // replicates (utterances)
#     int cHS;              // categories in Hearing Status (HS)
#     int cE;               // categories in Etiology (E)
#     real H[N];            // replicated entropies
#     int cid[N];           // child's id
#     int HS[I];            // hearing status 
#     int Am[I];            // hearing age
#     int E[I];             // etiology
#     real sPTA[I];         // (standardized) pta values
# }
# parameters{
#     real a;               // fixed intercepts
#     //vector[cE] aE;        // fixed intercept (per E)
#     vector[cHS] aHS;      // fixed intercept (per HS)
#     real mu_aHS;          // hyperparameter for aHS
#     real bP;              // fixed slope standardized PTA
#     //real bA;              // fixed slope (A - A_min)
#     vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
#     real mu_bAHS;         // hyperparameter for bAHS
#     vector<lower=0>[2] sigma_abHS; // variability for aHS and bAHS
#     corr_matrix[2] Rho;   // correlation matrix for aHS and bAHS
#     real m_i;             // mean of population
#     real<lower=0> s_i;    // variability of population
#     vector[I] re_i;       // random intercepts (per child)
#     real m_M;             // mean of df
#     real<lower=0> s_M;    // variability of df
#     real<lower=0> M[I];   // df (per child)
# }
# transformed parameters{
#     vector[I] SI;         // true SI index (per child)
#     vector[I] Ht;         // true entropy (per child)
#     
#     // linear predictor
#     for(i in 1:I){
#       SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
#       // no multicollinearity between E and HS
#       
#       //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
#       // multicollinearity between E and HS
#       
#       //SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
#       // when no interaction
#     }
#     
#     // average entropy (SI -> Ht: negative)
#     Ht = inv_logit(-SI);  
# }
# model{
#     // parameter not to follow
#     vector[2] mu_abHS;    // hyperprior mean for aHS and bAHS
#     matrix[2,2] S_abHS;   // hyperprior SD for aHS and bAHS
#     vector[2] RE[cHS];    // declare storage for fixed effects
#     // first are columns, and then rows
#     
#     
#     // simple hyperpriors
#     m_i ~ normal( 0 , 0.5 );
#     s_i ~ exponential( 1 );
#     m_M ~ normal( 0 , 0.5 );
#     s_M ~ exponential( 1 );
#     
#     // hyperprior for correlated fixed effects
#     mu_aHS ~ normal( 0 , 0.5 ); 
#     mu_bAHS  ~ normal( 0 , 0.5 );
#     sigma_abHS ~ exponential( 1 );
#     Rho ~ lkj_corr( 2 );  
# 
#     mu_abHS = [mu_aHS , mu_bAHS]';
#     S_abHS = quad_form_diag(Rho, sigma_abHS);
#     for ( c in 1:cHS ){ 
#       RE[c] = [aHS[c], bAHS[c]]';  // storage first fixed effects
#     }
#     RE ~ multi_normal( mu_abHS , S_abHS ); 
# 
#     
#     // priors
#     a ~ normal( 0 , 0.5 );
#     re_i ~ normal( m_i , s_i );
#     M ~ lognormal( m_M , s_M );
#     //aE ~ normal( 0 , 0.5 );
#     bP ~ normal( 0 , 0.3 );
#     //bA ~ normal( 0 , 0.3 );
#     
#     // likelihood
#     for(n in 1:N){
#       H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
#     }
# }
# "
# 
# # saving
# model_nam = "Hbeta_C_sim5_cor.stan"
# writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )
# 
# 
# 
# 
# 
# ## non-centered cor ####
# mcmc_code = "
# data{
#     int N;                // experimental runs
#     int I;                // experimental units (children)
#     int K;                // replicates (utterances)
#     int cHS;              // categories in Hearing Status (HS)
#     int cE;               // categories in Etiology (E)
#     real H[N];            // replicated entropies
#     int cid[N];           // child's id
#     int HS[I];            // hearing status 
#     int Am[I];            // hearing age
#     int E[I];             // etiology
#     real sPTA[I];         // (standardized) pta values
# }
# parameters{
#     real a;               // fixed intercepts
#     //vector[cE] aE;        // fixed intercept (per E)
#     matrix[2, cHS] z_abHS;// matrix of (2x4)
#     real mu_aHS;          // hyperparameter for aHS
#     real bP;              // fixed slope standardized PTA
#     //real bA;              // fixed slope (A - A_min)
#     real mu_bAHS;         // hyperparameter for bAHS
#     vector<lower=0>[2] sigma_abHS; // variability for aHS and bAHS
#     cholesky_factor_corr[2] L_Rho; // cholesky factor for correlation matrix for aHS and bAHS
#     real m_i;             // mean of population
#     real<lower=0> s_i;    // variability of population
#     vector[I] z_re;        // random intercept (per child) noncentered
#     real m_M;             // mean of df
#     real<lower=0> s_M;    // variability of df
#     vector[I] z_M;        // noncentered df (per child)
# }
# transformed parameters{
#     vector[2] mu_abHS;    // hyperprior mean for aHS and bAHS
#     matrix[2, cHS] RE;    // correlated fixed effects (2x4)
#     vector[cHS] aHS;      // fixed intercept (per HS)
#     vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
#     vector[I] re_i;       // intercept (per child)
#     vector[I] M;          // df (per child)
#     vector[I] SI;         // true SI index (per child)
#     vector[I] Ht;         // true entropy (per child)
#     matrix[2, 2] Rho;     // correlation matrix
# 
#     // random effects and df's
#     re_i = m_i + s_i*z_re;
#     M = exp( m_M + s_M*z_M );
#     
#     // correlated fixed effects
#     mu_abHS = [mu_aHS , mu_bAHS]';
#     RE = (diag_pre_multiply(sigma_abHS, L_Rho) * z_abHS)';
#     for(c in 1:cHS){
#       RE[c] = to_row_vector(mu_abHS) + RE[c];
#     }
#     aHS = RE[,1];
#     bAHS = RE[,2];
#     
#     // correlation matrix
#     Rho = multiply_lower_tri_self_transpose(L_Rho);
#     
#     // linear predictor
#     for(i in 1:I){
#       SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
#       // no multicollinearity between E and HS
#       
#       //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
#       // multicollinearity between E and HS
#       
#       //SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
#       // when no interaction
#     }
#     
#     // average entropy (SI -> Ht: negative)
#     Ht = inv_logit(-SI);  
# }
# model{
#     
#     // simple hyperpriors
#     m_i ~ normal( 0 , 0.5 );
#     s_i ~ exponential( 1 );
#     m_M ~ normal( 0 , 0.5 );
#     s_M ~ exponential( 1 );
#     
#     // hyperprior for correlated fixed effects
#     mu_aHS ~ normal( 0 , 0.5 ); 
#     mu_bAHS  ~ normal( 0 , 0.5 );
#     sigma_abHS ~ exponential( 1 );
#     L_Rho ~ lkj_corr_cholesky( 2 ); 
# 
#     
#     // priors
#     a ~ normal( 0 , 0.5 );
#     z_re ~ std_normal();
#     z_M ~ std_normal();
#     //aE ~ normal( 0 , 0.5 );
#     bP ~ normal( 0 , 0.3 );
#     //bA ~ normal( 0 , 0.3 );
#     to_vector( z_abHS ) ~ std_normal();
#     
#     // likelihood
#     for(n in 1:N){
#       H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
#     }
# }
# "
# 
# # saving
# model_nam = "Hbeta_NC_sim5_cor.stan"
# writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )
