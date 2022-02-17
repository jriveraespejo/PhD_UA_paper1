
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
    real a;               // fixed intercepts
    vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real mu_aHS;          // hyperparameter for aHS
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real mu_bAHS;         // hyperparameter for bAHS
    vector<lower=0>[2] sigma_abHS; // variability for aHS and bAHS
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    corr_matrix[2] Rho;   // correlation matrix for aHS and bAHS
    vector[I] a_i;        // random intercepts (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + (bA + bAHS[HS[i]])*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + (bA + bAHS[HS[i]])*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // parameter not to follow
    vector[2] mu_abHS;    // hyperprior mean for aHS and bAHS
    matrix[2,2] S_abHS;   // hyperprior SD for aHS and bAHS
    vector[2] RE[cHS];    // declare storage for fixed effects
    // first are columns, and then rows
    
    
    // simple hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // hyperprior for correlated fixed effects
    mu_aHS ~ normal( 0 , 0.5 ); 
    mu_bAHS  ~ normal( 0 , 0.5 );
    sigma_abHS ~ exponential( 1 );
    Rho ~ lkj_corr( 2 );  

    mu_abHS = [mu_aHS , mu_bAHS]';
    S_abHS = quad_form_diag(Rho, sigma_abHS);
    for ( c in 1:cHS ){ 
      RE[c] = [aHS[c], bAHS[c]]';      // storage first fixed effects
    }
    RE ~ multi_normal( mu_abHS , S_abHS ); 

    
    // priors
    a ~ normal( 0 , 0.5 );
    a_i ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );
    aE ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}

