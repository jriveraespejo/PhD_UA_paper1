
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
    real m_HJ[R];         // mean replicated unbounded HJ (reduced data)
    real<lower=0> s_HJ[R];// var. measurement + judges (reduced data)
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
    vector[I] z_i;        // random intercepts (per child)
    real<lower=0> s_SI;   // sd of SI
    vector[I] SI;         // SI index (per child)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)

    // random effects
    re_i = m_i + s_i*z_i;

}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[R] mu;
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    z_i ~ std_normal( );


    // priors
    s_SI ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    //bAHS ~ normal( 0 , 0.3 );


    // SI likelihood
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
    SI ~ normal( m_SI, s_SI);
    
    
    // reduced HJ likelihood
    mu = inv_logit(SI[rcid]);
    m_HJ ~ normal( mu , s_HJ);
    // transform MEAN from [-oo,+oo] to [0,1] range
    
    //logit( m_HJ ) ~ normal( SI[rcid] , s_HJ);
    // transform DATA from [0,1] to [-oo,+oo] range
}

