
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercept
    real m_c;             // mean of population
    real<lower=0> s_c;    // variability of population
    vector[I] z_re;       // non-centered random interpcepts
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)

    re_i = m_c + s_c*z_re;// non-centering
    SI = a + re_i;        // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    m_c ~ normal( 0 , 0.5 );
    s_c ~ exponential( 1 );
    
    // priors
    a ~ normal(0, 0.5);
    z_re ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}

