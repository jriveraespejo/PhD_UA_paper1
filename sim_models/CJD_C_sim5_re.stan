
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int R;                // total number of replicates
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    int<lower=0> c_CJD[R];// successes CJD (reduced data)
    int<lower=0> n_CJD[R];// total comparisons CJD (reduced data)
    int rcid[R];          // child's id
    int ruid[R];          // utterance's id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    real bA;              // fixed slope (A - A_min)
    //vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] re_i;       // random intercepts (per child)
    real m_k;             // mean of utterance random effects
    real<lower=0> s_SI;   // sd of SI
    vector[K] re_k;       // random intercepts (per utterance)
}
transformed parameters{
    vector[I] m_SI;       // SI linear predictor
    
    // SI linear predictor
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a;
      // simple model

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
      
      //m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }
}
model{
    // to not follow
    vector[R] m_CJD;
    
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_k ~ normal( 0 , 0.5 );
    s_SI ~ exponential( 4 );
    
    // priors
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );
    re_i ~ normal( m_i , s_i );
    re_k ~ normal( m_k , s_SI );
    
    // reduced CJD likelihood
    m_CJD = inv_logit( m_SI[rcid] + re_k[ruid] );
    c_CJD ~ binomial( n_CJD , m_CJD);
}

