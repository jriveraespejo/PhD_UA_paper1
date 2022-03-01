
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
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] z_i;        // (standardized) random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] z_j;        // (standardized) random intercepts (per judge)
    real<lower=0> s_SI;   // var. SI index
    vector[I] SI;         // true SI index (per child)
    real<lower=0> s_HJ;   // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
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
    s_SI ~ exponential( 4 );
    s_HJ ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a;
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

