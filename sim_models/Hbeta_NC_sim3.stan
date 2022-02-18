
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
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
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_c;             // mean of population
    real<lower=0> s_c;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // random effects and dfs
    re_i = m_c + s_c*z_re;
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_c ~ normal( 0 , 0.5 );
    s_c ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    z_re ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
