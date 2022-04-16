
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
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    vector[I] z_SI;       // SI index
    real<lower=0> s_SI;   // variability of SI
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    vector[I] z_M;        // noncentered df (per child)
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] m_SI;       // mean SI index (per child)
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // random effects and dfs
    re_i = m_i + s_i*z_re;
    M = exp( m_M + s_M*z_M );
    
    // linear predictor
    for(i in 1:I){
      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }
    SI = m_SI + s_SI*z_SI;// non-centered SI
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    z_re ~ std_normal();
    z_M ~ std_normal();
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    s_SI ~ exponential( 2 );
    
    // likelihood
    z_SI ~ std_normal();      // non-centered SI index
    
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}

