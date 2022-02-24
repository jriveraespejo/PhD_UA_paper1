
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
    real<lower=0> l;      // rate for s_HJ (to learn from data)
    real<lower=0> z_HJ[I];// (standardized) variability of measurement (per child)
}
transformed parameters{
    vector[I] re_i;       // random intercepts (per child)
    vector[J] re_j;       // random intercepts (per judge)
    real<lower=0> s_HJ[I];// variability of measurement (per child)
    vector[I] SI;         // true SI index (per child)

    re_i = m_i + s_i*z_i;
    re_j = m_j + s_j*z_j;
    s_HJ = l*z_HJ;
    
    // linear predictor
    for(i in 1:I){
      SI[i] = re_i[i] + a + aHS[HS[i]] + + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, interaction
      
      //SI[i] = re_i[i] + a + aE[E[i]] + aHS[HS[i]] + bAHS[HS[i]]*Am[i] + bP*sPTA[i];
      // multicollinearity between E and HS, interaction
      
      //SI[i] = re_i[i] + a + aHS[HS[i]] + bA*Am[i] + bP*sPTA[i];
      // no multicollinearity between E and HS, no interaction
    }
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
    z_HJ ~ exponential( 1 );
    l ~ exponential( 2 );
    a ~ normal( 0 , 0.5 );
    //aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    //bA ~ normal( 0 , 0.3 );
    bAHS ~ normal( 0 , 0.3 );
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
      
      HJ[n] ~ normal( mu , s_HJ[cid[n]] );
      // assuming a [-oo,+oo] measure (standardized)
      // no issues with the max_tree_length, and good speed
    }
}

