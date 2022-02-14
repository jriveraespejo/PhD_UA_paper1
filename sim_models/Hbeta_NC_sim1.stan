
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // non-centered a
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)

    a = mu_a + sigma_a * z_a; // non-centering
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}

