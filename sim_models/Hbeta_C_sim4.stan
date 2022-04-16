
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

