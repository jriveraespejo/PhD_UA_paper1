
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // noncentered intercept (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    a = mu_a + z_a * sigma_a;
    M = exp( mu_the + z_M * sigma_the );
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    z_M ~ std_normal();
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}

