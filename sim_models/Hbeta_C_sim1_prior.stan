
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

