
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
    real<lower=0> s_HJ;  // variability of measurement
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    vector[I] SI;         // true SI index (per child)
    
    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    
    SI = re_i + a;        
}
model{
    // parameter to not follow
    real mu;              // linear predictor
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_HJ ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    z_i ~ std_normal( );
    z_j ~ std_normal( );
    
    // likelihood
    for(n in 1:N){
      mu = SI[cid[n]] + re_j[jid[n]];
      
      //logit(HJ[n]) ~ normal( mu , s_HJ );
      // assuming a [0,1] measure
      // it has issues with the max_tree_length, and samples way to slow
      
      //HJ[n] ~ normal( inv_logit(mu) , s_HJ );
      // assuming a [0,1] measure
      // no issues with the max_tree_length, and samples way to slow
      
      HJ[n] ~ normal( mu , s_HJ );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}

