
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
    vector[cHS] bP;       // fixed slope standardized PTA (different per HS)
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
      SI[i] = re_i[i] + a + bP[HS[i]]*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bA*Am[i];
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

