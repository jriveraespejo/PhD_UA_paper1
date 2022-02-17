
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
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[2, cHS] z_abHS;// matrix of (2x4)
    real mu_aHS;          // hyperparameter for aHS
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    real mu_bAHS;         // hyperparameter for bAHS
    vector<lower=0>[2] sigma_abHS; // variability for aHS and bAHS
    cholesky_factor_corr[2] L_Rho; // cholesky factor for correlation matrix for aHS and bAHS
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // random intercept (per child) noncentered
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[2] mu_abHS;    // hyperprior mean for aHS and bAHS
    matrix[2, cHS] RE;    // correlated fixed effects (2x4)
    vector[cHS] aHS;      // fixed intercept (per HS)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    vector[I] a_i;        // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    matrix[2, 2] Rho;     // correlation matrix

    // random effects and df's
    a_i = mu_a + sigma_a * z_a;
    M = exp( mu_the + sigma_the * z_M );
    
    // correlated fixed effects
    mu_abHS = [mu_aHS , mu_bAHS]';
    RE = (diag_pre_multiply(sigma_abHS, L_Rho) * z_abHS)';
    for(c in 1:cHS){
      RE[c] = to_row_vector(mu_abHS) + RE[c];
    }
    aHS = RE[,1];
    bAHS = RE[,2];
    
    // correlation matrix
    Rho = multiply_lower_tri_self_transpose(L_Rho);
    
    // linear predictor
    for(i in 1:I){
      SI[i] = a + a_i[i] + aHS[HS[i]] + bAHS[HS[i]]*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] = a + a_i[i] + aE[E[i]] + aHS[HS[i]] + (bA + bAHS[HS[i]])*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    
    // simple hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // hyperprior for correlated fixed effects
    mu_aHS ~ normal( 0 , 0.5 ); 
    mu_bAHS  ~ normal( 0 , 0.5 );
    sigma_abHS ~ exponential( 1 );
    L_Rho ~ lkj_corr_cholesky( 2 ); 

    
    // priors
    a ~ normal( 0 , 0.5 );
    z_a ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    to_vector( z_abHS ) ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}

