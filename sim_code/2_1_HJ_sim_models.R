# preliminar ####
rm(list=ls())

setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1')


# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_HJ low and equal for all children 
# Covariates: None
#
## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];          // replicated (bounded) absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] re_i;       // random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] re_j;       // random intercepts (per judge)
    real<lower=0> s_HJ;  // variability of measurement
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    SI = re_i + a;        
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"

# saving
model_nam = "HJ_C_sim1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )



## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] z_i;        // (standardized) random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] z_j;        // (standardized) random intercepts (per judge)
    real<lower=0> s_HJ;  // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] SI;         // true SI index (per child)
    
    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    
    SI = re_i + a;        
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"

# saving
model_nam = "HJ_NC_sim1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






# simulation 2: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_HJ low and equal for all children 
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
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] re_i;       // random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] re_j;       // random intercepts (per judge)
    real<lower=0> s_HJ;  // variability of measurement
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)

    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"

# saving
model_nam = "HJ_C_sim2.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] z_i;        // (standardized) random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] z_j;        // (standardized) random intercepts (per judge)
    real<lower=0> s_HJ;  // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] SI;         // true SI index (per child)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"

# saving
model_nam = "HJ_NC_sim2.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






# simulation 3: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_HJ different for all children
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
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] re_i;       // random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] re_j;       // random intercepts (per judge)
    real<lower=0> s_HJ[I];// variability of measurement (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"

# saving
model_nam = "HJ_C_sim3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )



## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] z_i;        // (standardized) random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] z_j;        // (standardized) random intercepts (per judge)
    real<lower=0> s_HJ[I];// variability of measurement (per child)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] SI;         // true SI index (per child)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"

# saving
model_nam = "HJ_NC_sim3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )





# simulation 4: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_HJ different for all children
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
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] re_i;       // random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] re_j;       // random intercepts (per judge)
    real<lower=0> s_HJ[I];// variability of measurement (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
      
      //SI[i] = re_i[i] + a+ aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
    }
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"

# saving
model_nam = "HJ_C_sim4_nocor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )



## non-centered no cor ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] z_i;        // (standardized) random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] z_j;        // (standardized) random intercepts (per judge)
    real<lower=0> s_HJ[I];// variability of measurement (per child)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] SI;         // true SI index (per child)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
      
      //SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
    }
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}
"


# saving
model_nam = "HJ_NC_sim4_nocor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






# simulation 5: ####
#
# details:
# Model: 2 types
# Outcome: complex generation, s_HJ different for all children, reduced data
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
## centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int R;                // total number of replicates
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real m_HJ[R];         // reduced replicated (not)bounded absolute holistic judgements
    real<lower=0> s_HJ[R];// reduced replicated (not)bounded absolute holistic judgements
    int rcid[R];          // child's id
    int ruid[R];          // utterance's id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    //vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] re_i;       // random intercepts (per child)
    vector[I] SI;         // SI index (per child)
    real<lower=0> s_SI;   // sd of SI
}
transformed parameters{
    vector[I] m_SI;       // true SI index (per child)

    // linear predictor
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a;
      // simple model
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    s_SI ~ exponential( 1 );

    // likelihood
    SI ~ normal( m_SI, s_SI);
    for(r in 1:R){
      m_HJ[r] ~ normal( inv_logit( SI[rcid[r]] ) , s_HJ[r]);
    }
}
"

# saving
model_nam = "HJ_C_sim5.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )



## non-centered ####
mcmc_code = "
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int R;                // total number of replicates
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real m_HJ[R];         // reduced replicated (not)bounded absolute holistic judgements
    real<lower=0> s_HJ[R];// reduced replicated (not)bounded absolute holistic judgements
    int rcid[R];          // child's id
    int ruid[R];          // utterance's id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    //vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] z_i;        // random intercepts (per child)
    vector[I] SI;         // SI index (per child)
    real<lower=0> s_SI;   // sd of SI
}
transformed parameters{
    vector[I] m_SI;       // true SI index (per child)
    vector[I] re_i;       // random intercepts (per child)

    // random effects
    re_i = m_i + s_i*z_i;

    // linear predictor
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a;
      // simple model
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
    }
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    z_i ~ std_normal( );
    s_SI ~ exponential(1);

    // likelihood
    SI ~ normal( m_SI, s_SI);
    for(r in 1:R){
      m_HJ[r] ~ normal( inv_logit( SI[rcid[r]] ) , s_HJ[r]);
    }
}
"


# saving
model_nam = "HJ_NC_sim5.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )

