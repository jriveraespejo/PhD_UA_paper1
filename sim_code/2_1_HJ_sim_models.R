# preliminar ####
rm(list=ls())

setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1')


# simulation 1: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_SI low and equal for all children 
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
    real HJ[N];           // replicated (bounded) absolute holistic judgements
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
    real<lower=0> s_SI;   // var. SI index 
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor
    
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_SI ~ exponential( 4 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a;
    }
    SI ~ normal(m_SI, s_SI);
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow

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
    real<lower=0> s_SI;   // var. SI index
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );

    
    // priors
    s_SI ~ exponential( 4 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a;
    }
    SI ~ normal(m_SI, s_SI);
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
}
"

# saving
model_nam = "HJ_NC_sim1.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






# simulation 2: ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_SI low and equal for all children 
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
    real<lower=0> s_SI;   // var. SI index
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_SI ~ exponential( 4 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
    
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI ~ normal(m_SI, s_SI);
    
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
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
    real<lower=0> s_SI;   // var. SI index
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );

    
    // priors
    s_SI ~ exponential( 4 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
    
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }  
    SI ~ normal(m_SI, s_SI);
    
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
}
"

# saving
model_nam = "HJ_NC_sim2.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )




# simulation 2: RE ####
# 
# details:
# Model: 2 types
# Outcome: easy generation, s_SI low and equal for all children 
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
    real m_k;             // mean of utterance random effects
    real<lower=0> s_SI;   // sd of utterance random effects (measuremenet error also)
    vector[K] re_k;       // random intercepts (per utterance)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] m_SI;       // SI linear predictor
    
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
    
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }  
}
model{
    // parameter to not follow
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    m_k ~ normal( 0 , 0.5 );
    s_SI ~ exponential( 4 );
    
    // priors
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    re_k ~ normal( m_k , s_SI );
    
    
    // HJ likelihood 
    m_HJ = m_SI[cid] + re_j[jid] + re_k[uid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
}
"

# saving
model_nam = "HJ_C_sim2_re.stan"
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
    real m_k;             // mean of utterance random effects
    real<lower=0> s_SI;   // sd of utterance random effects (measuremenet error also)
    vector[K] z_k;        // (standardized) random intercepts (per utterance)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] m_SI;       // SI linear predictor
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[K] re_k;       // random intercepts (per utterance)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    re_k = m_k + s_SI*z_k;
    
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
    
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }  

}
model{
    // parameter to not follow
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    m_k ~ normal( 0 , 0.5 );
    s_SI ~ exponential( 4 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );
    z_k ~ std_normal( );
    
    
    // priors
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    
    // HJ likelihood 
    m_HJ = m_SI[cid] + re_j[jid] + re_k[uid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
}
"

# saving
model_nam = "HJ_NC_sim2_re.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )







# simulation 3: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_SI different for all children
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
    real<lower=0> r;      // rate for s_SI (to learn from data)
    real<lower=0> s_SI[I];// var. SI index (per child)
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    r ~ exponential( 0.5 );
    s_SI ~ exponential( r );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
    
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }  
    SI ~ normal(m_SI, s_SI);
    
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
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
    real<lower=0> r;      // rate for s_HJ (to learn from data)
    real<lower=0> z_SI[I];// (standardized) var. SI index (per child)
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] s_SI;       // var. SI index (per child)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    s_SI = r*to_vector(z_SI);
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );


    // priors
    r ~ exponential( 0.5 );
    z_SI ~ exponential( 1 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
    
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }  
    SI ~ normal(m_SI, s_SI);
    
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
}
"

# saving
model_nam = "HJ_NC_sim3.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )










# simulation 4: ####
# 
# details:
# Model: 2 types
# Outcome: complex generation, s_SI different for all children
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
    real<lower=0> r;      // rate for s_HJ (to learn from data)
    real<lower=0> s_SI[I];// var. SI index (per child)
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    r ~ exponential( 0.5 );
    s_SI ~ exponential( r );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    
    // SI likelihood
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a+ aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }  
    SI ~ normal(m_SI, s_SI);
    
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
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
    real<lower=0> r;      // rate for s_HJ (to learn from data)
    real<lower=0> z_SI[I];// (standardized) var. SI index (per child)
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] s_SI;       // var. SI index (per child)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    s_SI = r*to_vector(z_SI);
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );

    
    // priors
    r ~ exponential( 0.5 );
    z_SI ~ exponential( 1 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    
    
    // SI likelihood
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a+ aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }  
    SI ~ normal(m_SI, s_SI);
    
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
}
"


# saving
model_nam = "HJ_NC_sim4_nocor.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






# simulation 5: ####
#
# details:
# Model: 2 types
# Outcome: complex generation, s_SI different for all children, reduced data
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
    real m_HJ[R];         // mean replicated unbounded HJ (reduced data)
    real<lower=0> s_HJ[R];// var. measurement + judges (reduced data)
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
    real<lower=0> s_SI;   // sd of SI
    vector[I] SI;         // SI index (per child)
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[R] mu;


    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );

    // priors
    s_SI ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );


    // SI likelihood
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a;
      // simple model

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }
    SI ~ normal( m_SI, s_SI);
    
    
    // reduced HJ likelihood
    mu = inv_logit(SI[rcid]);
    m_HJ ~ normal( mu , s_HJ);
    // transform MEAN from [-oo,+oo] to [0,1] range
    
    //logit( m_HJ ) ~ normal( SI[rcid] , s_HJ);
    // transform DATA from [0,1] to [-oo,+oo] range
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
    real m_HJ[R];         // mean replicated unbounded HJ (reduced data)
    real<lower=0> s_HJ[R];// var. measurement + judges (reduced data)
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
    real<lower=0> s_SI;   // sd of SI
    vector[I] SI;         // SI index (per child)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)

    // random effects
    re_i = m_i + s_i*z_i;

}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[R] mu;
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    z_i ~ std_normal( );


    // priors
    s_SI ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );


    // SI likelihood
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a;
      // simple model

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }
    SI ~ normal( m_SI, s_SI);
    
    
    // reduced HJ likelihood
    mu = inv_logit(SI[rcid]);
    m_HJ ~ normal( mu , s_HJ);
    // transform MEAN from [-oo,+oo] to [0,1] range
    
    //logit( m_HJ ) ~ normal( SI[rcid] , s_HJ);
    // transform DATA from [0,1] to [-oo,+oo] range
}
"


# saving
model_nam = "HJ_NC_sim5.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )






# simulation 5: RE ####
#
# details:
# Model: 2 types
# Outcome: complex generation, s_SI different for all children, reduced data
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
    real m_HJ[R];         // mean replicated unbounded HJ (reduced data)
    real<lower=0> s_HJ[R];// var. measurement + judges (reduced data)
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
    real m_k;             // mean of utterance random effects
    real<lower=0> s_SI;   // sd of SI
    vector[K] re_k;       // random intercepts (per utterance)
}
transformed parameters{
    vector[I] m_SI;       // SI linear predictor
    
    // SI linear predictor
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a;
      // simple model

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }
}
model{
    // to not follow
    vector[R] mu;
    
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_k ~ normal( 0 , 0.5 );
    s_SI ~ exponential( 4 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_k ~ normal( m_k , s_SI );
    
    // reduced HJ likelihood
    mu = inv_logit( m_SI[rcid] + re_k[ruid] );
    m_HJ ~ normal( mu , s_HJ);
    // transform MEAN from [-oo,+oo] to [0,1] range
    
    //logit( m_HJ ) ~ normal( SI[rcid] , s_HJ);
    // transform DATA from [0,1] to [-oo,+oo] range
}
"

# saving
model_nam = "HJ_C_sim5_re.stan"
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
    real m_HJ[R];         // mean replicated unbounded HJ (reduced data)
    real<lower=0> s_HJ[R];// var. measurement + judges (reduced data)
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
    real m_k;             // mean of utterance random effects
    real<lower=0> s_SI;   // sd of SI
    vector[K] z_k;        // random intercepts (per utterance)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[K] re_k;       // random intercepts (per utterance)
    vector[I] m_SI;       // SI linear predictor

    // random effects
    re_i = m_i + s_i*z_i;
    re_k = m_k + s_SI*z_k;
    
    // SI linear predictor
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a;
      // simple model

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }

}
model{
    // to not follow
    vector[R] mu;
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_k ~ normal( 0 , 0.5 );
    s_SI ~ exponential( 4 );
    z_i ~ std_normal( );
    z_k ~ std_normal( );


    // priors
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );


    // reduced HJ likelihood
    mu = inv_logit( m_SI[rcid] + re_k[ruid] );
    m_HJ ~ normal( mu , s_HJ);
    // transform MEAN from [-oo,+oo] to [0,1] range
    
    //logit( m_HJ ) ~ normal( SI[rcid] , s_HJ);
    // transform DATA from [0,1] to [-oo,+oo] range
}
"


# saving
model_nam = "HJ_NC_sim5_re.stan"
writeLines(mcmc_code, con=file.path(getwd(), 'sim_models', model_nam) )

