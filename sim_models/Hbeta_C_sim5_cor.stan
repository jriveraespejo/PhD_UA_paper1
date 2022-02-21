
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int Am[I];            // hearing age
    int E[I];             // etiology
    real sPTA[I];         // (standardized) pta values
}
parameters{
    real a;               // fixed intercepts
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real mu_aHS;          // hyperparameter for aHS
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real mu_bAHS;         // hyperparameter for bAHS
    vector<lower=0>[2] sigma_abHS; // variability for aHS and bAHS
    corr_matrix[2] Rho;   // correlation matrix for aHS and bAHS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // when no interaction
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
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // hyperprior for correlated fixed effects
    mu_aHS ~ normal( 0 , 0.5 ); 
    mu_bAHS  ~ normal( 0 , 0.5 );
    sigma_abHS ~ exponential( 1 );
    Rho ~ lkj_corr( 2 );  

    mu_abHS = [mu_aHS , mu_bAHS]';
    S_abHS = quad_form_diag(Rho, sigma_abHS);
    for ( c in 1:cHS ){ 
      RE[c] = [aHS[c], bAHS[c]]';  // storage first fixed effects
    }
    RE ~ multi_normal( mu_abHS , S_abHS ); 

    
    // priors
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
    //aE ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}

