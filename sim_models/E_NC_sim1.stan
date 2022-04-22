
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
    vector[I] SI;         // SI index
    vector[I] Ht;         // true entropy (per child)

    re_i = m_i + s_i*z_re;// non-centered RE
    SI = a + re_i;        // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{

    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}

