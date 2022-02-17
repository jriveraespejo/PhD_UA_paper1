
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real a;               // fixed intercepts
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // random intercepts (per child) noncentered
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] a_i;        // random intercepts (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    
    a_i = mu_a + sigma_a * z_a;
    M = exp( mu_the + sigma_the * z_M );
    
    SI = a + a_i;         // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );

    // priors
    a ~ normal( 0 , 0.5 );
    z_a ~ std_normal();
    z_M ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}

