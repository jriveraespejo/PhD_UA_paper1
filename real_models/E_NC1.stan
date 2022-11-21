
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

