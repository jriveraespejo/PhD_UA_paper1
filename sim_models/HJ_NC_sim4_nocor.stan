
data{
    int N;                // experimental runs
    int I;                // experimental units (children)
    int K;                // replicates (utterances)
    int D;                // duplicates (comparisons)
    int J;                // judges
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    int HS[I];            // hearing status 
    int Am[I];            // hearing age (centered at minimum)
    int E[I];             // etiology
    real sPTA[I];         // (standardized) PTA values
    real HJ[N];           // replicated (not)bounded absolute holistic judgements
    int cid[N];           // child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    //vector[cE] aE;        // fixed intercept (per E)
    vector[cHS] aHS;      // fixed intercept (per HS)
    real bP;              // fixed slope standardized PTA
    //real bA;              // fixed slope (A - A_min)
    vector[cHS] bAHS;     // fixed interaction (A - A_min)*HS
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] z_i;        // (standardized) random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] z_j;        // (standardized) random intercepts (per judge)
    real<lower=0> r;      // rate for s_HJ (to learn from data)
    real<lower=0> z_SI[I];// (standardized) var. SI index (per child)
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] s_SI;       // var. SI index (per child)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    s_SI = r*to_vector(z_SI);
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_HJ;       // HJ linear predictor

    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );

    
    // priors
    r ~ exponential( 0.5 );
    z_SI ~ exponential( 1 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
    
    
    // SI likelihood
    for(i in 1:I){
      //m_SI[i] = re_i[i] + a+ aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, no interaction

      m_SI[i] = re_i[i] + a + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction

      //m_SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
    }  
    SI ~ normal(m_SI, s_SI);
    
    
    // HJ likelihood 
    m_HJ = SI[cid] + re_j[jid];
    
    HJ ~ normal(m_HJ , s_HJ); 
    // assuming a [-oo,+oo] measure (standardized)
    // no issues with the max_tree_length, and good speed
    
    //logit(HJ) ~ normal( m_HJ , s_HJ );
    // assuming a [0,1] measure
    // it has issues with the max_tree_length, and samples way to slow
      
    //HJ ~ normal( inv_logit(m_HJ) , s_HJ );
    // assuming a [0,1] measure
    // no issues with the max_tree_length, and samples way to slow
}
