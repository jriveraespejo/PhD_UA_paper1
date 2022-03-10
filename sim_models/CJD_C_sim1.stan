
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
    int CJD[N];           // replicated dichotomous comparative judgment
    int cid1[N];          // first child's id
    int cid2[N];          // second child's id
    int uid[N];           // utterance's id
    int jid[N];           // judges' id
}
parameters{
    real a;               // fixed intercept
    real m_i;             // mean of child's random effects
    real<lower=0> s_i;    // sd of child's random effects
    vector[I] re_i;       // random intercepts (per child)
    real m_j;             // mean of judges' random effects
    real<lower=0> s_j;    // sd of judges's random effects
    vector[J] re_j;       // random intercepts (per judge)
    real<lower=0> s_SI;   // var. SI index 
    vector[I] SI;         // true SI index (per child)
}
model{
    // parameter to not follow
    vector[I] m_SI;       // SI linear predictor
    vector[N] m_CJD;      // CJD linear predictor
    
    
    // hyperpriors
    m_i ~ normal( 0 , 0.5 );
    s_i ~ exponential( 1 );
    m_j ~ normal( 0 , 0.5 );
    s_j ~ exponential( 1 );
    
    // priors
    s_SI ~ exponential( 4 );
    a ~ normal( 0 , 0.5 );
    re_i ~ normal( m_i , s_i );
    re_j ~ normal( m_j , s_j );
    
    
    // SI likelihood
    for(i in 1:I){
      m_SI[i] = re_i[i] + a;
    }
    SI ~ normal(m_SI, s_SI);
    
    
    // CJD likelihood 
    m_CJD = ( SI[cid1] - SI[cid2] ) + re_j[jid];
    CJD ~ bernoulli( inv_logit(m_CJD) ); 
}

