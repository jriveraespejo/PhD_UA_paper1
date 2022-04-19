
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
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
    vector[I] re_i;       // random intercepts (per child)
    real<lower=0> m_M;    // dfs beta
}
model{
    // hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    re_i ~ normal( m_i , s_i );
    //aE ~ normal( 0 , 0.3 );
    aHS ~ normal( 0 , 0.3 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    m_M ~ lognormal( 1.5 , 0.5 );
}
generated quantities {
    vector[I] SI;         // SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    vector[N] H;          // replicated entropies

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);
    
    // likelihood
    for(n in 1:N){
      H[n] = beta_proportion_rng( Ht[cid[n]], m_M );
    }
    
}

