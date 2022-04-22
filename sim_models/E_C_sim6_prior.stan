
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
    real a;               // fixed intercepts
    real bP;              // fixed slope standardized PTA
    //vector[cHS] aHS;      // fixed intercept (per HS)
    //vector[cE] aE;        // fixed intercept (per E)
    matrix[cE, cHS] aEHS; // fixed interaction E*HS
    //real bA;              // fixed slope (A - A_min) (if no different effect)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of population
    real<lower=0> s_i;    // variability of population
    vector[I] re_i;       // random intercepts (per child)
    real m_M;             // mean of df
    real<lower=0> s_M;    // variability of df
    real<lower=0> M[I];   // df (per child)
}
model{
    // simple hyperpriors
    m_i ~ normal( 0 , 0.2 );
    s_i ~ exponential( 1 );
    m_M ~ normal( 0 , 0.5 );
    s_M ~ exponential( 1 );
    
    // priors
    a ~ normal( 0 , 0.2 );
    bP ~ normal( 0 , 0.3 );
    //aHS ~ normal( 0 , 0.3 );
    //aE ~ normal( 0 , 0.3 );
    to_vector(aEHS) ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    M ~ lognormal( m_M , s_M );
}
generated quantities {
    vector[I] SI;         // SI index
    vector[I] Ht;         // true entropy (per child)
    vector[N] H;          // replicated entropies

    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + bP*sPTA[i] + 
              aEHS[E[i], HS[i]] +
              bAHS[HS[i]]*Am[i];
      // no multicollinearity between E and HS
      
      //SI[i] = re_i[i] + a + aE[E[i]] + bP*sPTA[i] + 
      //        aEHS[E[i], HS[i]] +
      //        bAHS[HS[i]]*Am[i];
      // multicollinearity between E and HS
    }

    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);
    
    // likelihood
    for(n in 1:N){
      H[n] = beta_proportion_rng( Ht[cid[n]], M[cid[n]] );
    }
}

