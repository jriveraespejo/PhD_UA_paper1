
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
    real bP;              // fixed slope standardized PTA
    vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    //matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] z_re;       // random intercept (per child) noncentered
    real<lower=0> m_M;    // mean of df
}
transformed parameters{
    vector[I] re_i;       // intercept (per child)
    vector[I] SI;         // SI index (per child)    
    vector[I] Ht;         // true entropy (per child)
    
    // random effects
    re_i = m_i + s_i*z_re;

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aHS[HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aHS[HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ lognormal( 1.5 , 0.5 );

    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    //to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    z_re ~ std_normal();

    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , m_M );
    }
}

