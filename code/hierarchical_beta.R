# preliminar ####
rm(list=ls())

librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
               'reshape2','tinytex','gt','haven',
               'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
               'rethinking','rstan','coda','runjags','rjags',#'loo'
               'cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)


setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1/code')



# simulation 1: ####
# 
# details:
# Structure: centered
# Outcome: easy generation M=10, no zero values
# Covariates: None
#
# generating function
Hsim1 = function(I=32, K=10, seed=12345, 
                 par=list(mu_a=0, s_a=1) ){
  
  # initial parameters
  # I = 32 # children
  # K = 10 # utterances
  set.seed(seed)
  a = rnorm(I, par$mu_a, par$s_a)
  
  # storage 1
  Ht = data.frame(matrix(NA, nrow=I, ncol=3))
  names(Ht) = c('child','SI','Ht')
  Ht$child= 1:I
  Ht$SI = a # true SI index
  Ht$Ht = inv_logit(-Ht$SI) # true entropy (SI -> Ht: negative)
  
  # storage 2
  N = I*K
  H = data.frame(matrix(NA, nrow=N, ncol=3))
  names(H) = c('child','utterance','H')
  H$child = rep(1:I, each=K)
  H$utterance = rep(1:K, I)
  # str(H)
  
  # generating observed entropy
  # i=1
  for(i in 1:I){
    index = H$child==i
    set.seed(seed)
    H$H[index] = rbeta2(n=K, prob=Ht$Ht[i], theta=10)
  }
  # View(H)
  
  # return value
  return(list(H=H, Ht=Ht, par=list(mu_a=par$mu_a, s_a=par$s_a, a=a)))
  
}


# generate data
data_mom = Hsim1()
Ht = data_mom$Ht
H = data_mom$H


# plot data
plot( H$H, H$child, col='gray', pch=19, xlim=c(0,1), 
      xlab='entropy', ylab='child ID')
points( Ht$Ht, Ht$child, col='blue', pch=19)
abline(h = H$child, lty=2, col='gray')
abline(v = c(0, 1), lty=1, col='gray')


# data list
dlist = list(
  N = nrow(H),
  K = max(H$utterance), # utterances
  I = nrow(Ht),
  H = H$H,
  cid = H$child )


# measurement error model
# centered
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    a ~ normal( mu_a , sigma_a );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "Hbeta.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
res = rstan::read_stan_csv( fit$output_files() )


# final comparison
compare = precis( res, depth=2, pars=c('mu_a','sigma_a','a','SI','Ht') )
compare$true = c(with(data_mom$par, c(mu_a, s_a, a) ), Ht$SI, Ht$Ht )
compare$withinCI = with(compare, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare
sum(compare$withinCI)/nrow(compare)
# good samples for mu_a and sigma_a, 
# a's are  also good
# 100% true parameters inside CI








# simulation 2: ####
# 
# details:
# Structure: non-centered
# Outcome: same as simulation 1
# Covariates: same as simulation 2
#
# generate data
data_mom = Hsim1()
Ht = data_mom$Ht
H = data_mom$H


# plot data
plot( H$H, H$child, col='gray', pch=19, xlim=c(0,1), 
      xlab='entropy', ylab='child ID')
points( Ht$Ht, Ht$child, col='blue', pch=19)
abline(h = H$child, lty=2, col='gray')
abline(v = c(0, 1), lty=1, col='gray')


# data list
dlist = list(
  N = nrow(H),
  K = max(H$utterance), # utterances
  I = nrow(Ht),
  H = H$H,
  cid = H$child )


# measurement error model
# non-centered
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // non-centered a
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)

    a = mu_a + sigma_a * z_a; // non-centering
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "Hbeta.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
res = rstan::read_stan_csv( fit$output_files() )


# final comparison
compare = precis( res, depth=2, pars=c('mu_a','sigma_a','a','SI','Ht') )
compare$true = c(with(data_mom$par, c(mu_a, s_a, a) ), Ht$SI, Ht$Ht )
compare$withinCI = with(compare, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare
sum(compare$withinCI)/nrow(compare)
# worst samples for mu_a and sigma_a 
# a's are equally good as previous simulation
# 100% true parameters inside CI







# simulation 3: ####
# 
# details:
# Structure: centered
# Outcome: easy generation M=10, no zero values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   positive (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# function
Hsim2 = function(I=32, K=10, seed=12345, 
                 prop=c(0.38, 0.31, 0.31), # proportion of children
                 par=list( mu_a=0.5, s_a=0.2, aE=-0.1, 
                           aHS=-0.4, bP=-0.1, bA=0.15 ) ){
  
  # # initial parameters
  # I = 32 # children
  # K = 10 # utterances
  
  # storage 1
  Ht = data.frame(matrix(NA, nrow=I, ncol=6))
  names(Ht) = c('child','E','PTA','A','HS','SI')
  Ht$child = 1:I
  
  
  # generating relationships
  set.seed(seed)
  # prop=c(0.38, 0.31, 0.31)
  prop = round( prop*I )
  Ht$HS = c( rep(1, prop[1]), rep(2, prop[2]), rep(3, prop[3])) 
  Ht$A = c( rep(7, prop[1]), round(rnorm( sum(prop[2:3]), 5, 1)) ) 
  Ht$A = ifelse(Ht$A>7, 7, Ht$A)
  
  # no way to know true effects
  Ht$E = c( rep(1, prop[1]), 
            sample(2:3, size=prop[2], replace=T),
            sample(3:4, size=prop[3], replace=T)) 
  
  Ht$PTA = c( round(rnorm(prop[1], 60, 10)), # first 12 NH 
              round(rnorm(prop[2], 90, 10)), # last 20
              round(rnorm(prop[3], 110, 20)))
  
  
  # final effects
  set.seed(seed)
  a = rnorm(I, par$mu_a, par$s_a)
  aE = par$aE
  aHS = par$aHS
  bP = par$bP
  bA = par$bA
  
  Ht$SI = with(Ht, a + aE*E + aHS*HS + bA*( A - min(A) ) +
                 bP * c( standardize(PTA) ) )
  Ht$Ht = inv_logit(-Ht$SI) # true entropy (SI -> Ht: negative)
  # hist(Ht$SI, breaks=100, xlim=c(-1,1))
  # hist(Ht$Ht, breaks=100, xlim=c(0,1))
  # well distributed
  
  
  
  # storage 2
  N = I*K
  H = data.frame(matrix(NA, nrow=N, ncol=3))
  names(H) = c('child','utterance','H')
  H$child = rep(1:I, each=K)
  H$utterance = rep(1:K, I)
  # str(H)
  
  # generating observed entropy
  # i=1
  for(i in 1:I){
    index = H$child==i
    H$H[index] = rbeta2(n=K, prob=Ht$Ht[i], theta=10)
  }
  # View(H)
  
  # return object
  return(list( H=H, Ht=Ht, par=list( par=par, a=a) ) )
  
}


# generate data
data_mom = Hsim2() # default 32
# data_mom = Hsim2(I = 100, prop=c(0.34, 0.33, 0.33) )
# data_mom = Hsim2(K = 30 )
Ht = data_mom$Ht
H = data_mom$H



# plot data
plot( H$H, H$child, col='gray', pch=19, xlim=c(0,1), 
      xlab='entropy', ylab='child ID')
abline(h = H$child, lty=2, col='gray')
abline(v = c(0, 1), lty=1, col='gray')
points( Ht$Ht, 1:nrow(Ht), col='blue', pch=19)



# # approximate effects (E + PTA -> HS)
# require(nnet)
# 
# # data mom
# data_test = Ht[,c('E','PTA','HS')]
# data_test$HS = factor(data_test$HS)
# data_test$E = factor(data_test$E)
# data_test$PTA = standardize(data_test$PTA)
# 
# # probability, reference HS=NH[1]
# model_test = multinom(HS ~ E + PTA, data=data_test)
# res_test = summary(model_test)
# z <- res_test$coefficients/res_test$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# 
# round(inv_logit(coef(model_test)), 5)
# p
# # intercept -> non-significant
# # E=2 -> less prob of HS=3
# # E=3 -> non significant (Ho: b=1)
# # E=4 -> less prob of HS=2
# # PTA -> non-significant (if you use E)
# 
# 
# # probability, reference HS=NH[1]
# model_test = multinom(HS ~ PTA, data=data_test)
# res_test = summary(model_test)
# z <- res_test$coefficients/res_test$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# 
# round(inv_logit(coef(model_test)), 5)
# p
# # intercept -> non-significant
# # PTA -> significant (if you do not use E)



# data list
dlist = list(
  N = nrow(H),
  K = max(H$utterance), # utterances
  I = nrow(Ht),
  cHS = max(Ht$HS),
  cE = max(Ht$E),
  H = H$H,
  cid = H$child,
  HS = Ht$HS,
  A = with(Ht, A - min(A) ),
  E = Ht$E,
  PTA = c( standardize( Ht$PTA ) ) )



# measurement error model
# centered
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept (per child)
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    a ~ normal( mu_a , sigma_a );
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "Hbeta.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
res = rstan::read_stan_csv( fit$output_files() )
# divergent transitions (between 1-8 of 4000)



# parameter comparison
# compare1 = precis( res, depth=2, pars=c('aE','aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )
compare1 = precis( res, depth=2, pars=c('aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )

true_pars = with(data_mom$par,
                 c( #par$aE * c(1:dlist$cE), 
                    par$aHS * c(1:dlist$cHS),
                    par$bP, par$bA, par$mu_a, par$s_a,
                    a, Ht$SI, Ht$Ht) )

compare1$true = true_pars
compare1$withinCI = with(compare1, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare1
sum(compare1$withinCI)/nrow(compare1)
# poor samples for all parameters, when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model
# good samples for SI, Ht (no matter E, HS, or both in model)
# 54.4% true parameters inside CI (depends on reference)




# contrast comparison
post = extract.samples(res)
# names(post)

contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
nam = c()

# for(p in 1:ncol(post$aE)){
#   for(q in 1:ncol(post$aE)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aE[,p] - post$aE[,q] )
#       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
#     }
#   }
# }

for(p in 1:ncol(post$aHS)){
  for(q in 1:ncol(post$aHS)){
    if(q>p){
      contrast_par = cbind(contrast_par,
                           post$aHS[,p] - post$aHS[,q] )
      nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
    }
  }
}

contrast_par = contrast_par[,-1]
contrast_par = data.frame(contrast_par)
names(contrast_par) = nam

compare2 = precis(contrast_par, depth=2, hist=F)
compare2$true = with( data_mom$par, 
                      c( #-par$aE * c(1:3, 1:2, 1),
                         -par$aHS * c(1:2, 1) ) )
compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare2
# when use E and HS in model, contrasts come all wrong, 
#   because of multicollinearity
#   it does not matter if you increase I or K
# when use only HS in model, contrasts come good, 
#   even with I=32
#   because we break multicollinearity



for_contrast = data.frame( 
  cbind(#post$aE,
    post$aHS) )
names(for_contrast) = c( 
  #paste0('aE[',1:4,']'), 
  paste0('aHS[',1:3,']') )

psych::pairs.panels( for_contrast )
psych::pairs.panels( contrast_par )
# notice the narrow ridge, when use E and HS 
#   indication of multicollinearity (makes sense)
# when use only HS, we also see narrow ridge
#   which is also indication of multicollinearity (how?)





# simulation 4: ####
# 
# details:
# Structure: non-centered
# Outcome: same as simulation 3
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   positive (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# generate data
data_mom = Hsim2() # default 32
# data_mom = Hsim2(I = 100, prop=c(0.34, 0.33, 0.33) )
# data_mom = Hsim2(K = 30 )
Ht = data_mom$Ht
H = data_mom$H


# plot data
plot( H$H, H$child, col='gray', pch=19, xlim=c(0,1), 
      xlab='entropy', ylab='child ID')
abline(h = H$child, lty=2, col='gray')
abline(v = c(0, 1), lty=1, col='gray')
points( Ht$Ht, 1:nrow(Ht), col='blue', pch=19)


# data list
dlist = list(
  N = nrow(H),
  K = max(H$utterance), # utterances
  I = nrow(Ht),
  cHS = max(Ht$HS),
  cE = max(Ht$E),
  H = H$H,
  cid = H$child,
  HS = Ht$HS,
  A = with(Ht, A - min(A) ),
  E = Ht$E,
  PTA = c( standardize( Ht$PTA ) ) )



# measurement error model
# non-centered
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // intercept (per child) non-centered
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    a = mu_a + sigma_a * z_a; // non-centering
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      //SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , 10 );
    }
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "Hbeta.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
res = rstan::read_stan_csv( fit$output_files() )
# no divergent transitions 


# parameter comparison
# compare1 = precis( res, depth=2, pars=c('aE','aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )
compare1 = precis( res, depth=2, pars=c('aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )

true_pars = with(data_mom$par,
                 c( #par$aE * c(1:dlist$cE), 
                    par$aHS * c(1:dlist$cHS),
                    par$bP, par$bA, par$mu_a, par$s_a,
                    a, Ht$SI, Ht$Ht) )

compare1$true = true_pars
compare1$withinCI = with(compare1, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare1
sum(compare1$withinCI)/nrow(compare1)
# great samples for all parameters 
# great samples for SI, Ht
# 54.3% true parameters inside CI (depends on reference)





# contrast comparison
post = extract.samples(res)
# names(post)

contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
nam = c()

# for(p in 1:ncol(post$aE)){
#   for(q in 1:ncol(post$aE)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aE[,p] - post$aE[,q] )
#       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
#     }
#   }
# }

for(p in 1:ncol(post$aHS)){
  for(q in 1:ncol(post$aHS)){
    if(q>p){
      contrast_par = cbind(contrast_par,
                           post$aHS[,p] - post$aHS[,q] )
      nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
    }
  }
}

contrast_par = contrast_par[,-1]
contrast_par = data.frame(contrast_par)
names(contrast_par) = nam

compare2 = precis(contrast_par, depth=2, hist=F)
compare2$true = with( data_mom$par, 
                      c( #-par$aE * c(1:3, 1:2, 1),
                        -par$aHS * c(1:2, 1) ) )
compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare2
# when use E and HS, contrasts come all wrong, 
#   because of multicollinearity
#   it does not matter if you increase I or K
# when use only HS, contrasts come good, even with I=32
#   because we break multicollinearity



for_contrast = data.frame( 
  cbind(#post$aE,
    post$aHS) )
names(for_contrast) = c( 
  #paste0('aE[',1:4,']'), 
  paste0('aHS[',1:3,']') )

psych::pairs.panels( for_contrast )
psych::pairs.panels( contrast_par )
# notice the narrow ridge, when use E and HS 
#   indication of multicollinearity (makes sense)
# when use only HS, we also see narrow ridge
#   which is also indication of multicollinearity (how?)








# simulation 5: ####
# 
# details:
# Structure: centered
# Outcome: complex generation different M, zero/one values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   positive (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# function
Hsim3 = function(I=32, K=10, seed=12345,
                 prop=c(0.38, 0.31, 0.31), # proportion of children
                 par=list( mu_a=0.5, s_a=0.2, mu_the=1.5, s_the=0.5,
                           aE=-0.1, aHS=-0.4, bP=-0.1, bA=0.15 ) ){
  
  # # initial parameters
  # I = 32 # children
  # K = 10 # utterances
  
  # storage 1
  Ht = data.frame(matrix(NA, nrow=I, ncol=6))
  names(Ht) = c('child','E','PTA','A','HS','SI')
  Ht$child = 1:I
  
  
  # generating relationships
  set.seed(seed)
  # prop=c(0.38, 0.31, 0.31)
  prop = round( prop*I )
  Ht$HS = c( rep(1, prop[1]), rep(2, prop[2]), rep(3, prop[3])) 
  Ht$A = c( rep(7, prop[1]), round(rnorm( sum(prop[2:3]), 5, 1)) ) 
  Ht$A = ifelse(Ht$A>7, 7, Ht$A)
  
  # no way to know true effects
  Ht$E = c( rep(1, prop[1]), 
            sample(2:3, size=prop[2], replace=T),
            sample(3:4, size=prop[3], replace=T)) 
  
  Ht$PTA = c( round(rnorm(prop[1], 60, 10)), # first 12 NH 
              round(rnorm(prop[2], 90, 10)), # last 20
              round(rnorm(prop[3], 110, 20)))
  
  
  # final effects
  set.seed(seed)
  a = rnorm(I, par$mu_a, par$s_a)
  aE = par$aE
  aHS = par$aHS
  bP = par$bP
  bA = par$bA
  
  Ht$SI = with(Ht, a + aE*E + aHS*HS + bA*( A - min(A) ) +
                 bP * c( standardize(PTA) ) )
  Ht$Ht = inv_logit(-Ht$SI) # true entropy (SI -> Ht: negative)
  
  Ht$M = rlnorm(I, meanlog = par$mu_the, sdlog= par$s_the) # degrees of freedom
  Ht$M = round(Ht$M)
  # hist(Ht$SI, breaks=100, xlim=c(-1,1))
  # hist(Ht$Ht, breaks=100, xlim=c(0,1))
  # well distributed
  
  
  # storage 2
  N = I*K
  H = data.frame(matrix(NA, nrow=N, ncol=3))
  names(H) = c('child','utterance','H')
  H$child = rep(1:I, each=K)
  H$utterance = rep(1:K, I)
  # str(H)
  
  # generating observed entropy
  # i=1
  for(i in 1:I){
    index = H$child==i
    H$H[index] = rbeta2(n=K, prob=Ht$Ht[i], theta=Ht$M[i])
  }
  # View(H)
  
  # return object
  return(list( H=H, Ht=Ht, par=list( par=par, a=a) ) )
  
}


# generate data
data_mom = Hsim3() # default 32
# data_mom = Hsim3(I = 100, prop=c(0.34, 0.33, 0.33) )
# data_mom = Hsim3(K = 30 )
Ht = data_mom$Ht
H = data_mom$H


# plot data
plot( H$H, H$child, col='gray', pch=19, xlim=c(0,1), 
      xlab='entropy', ylab='child ID')
abline(h = H$child, lty=2, col='gray')
abline(v = c(0, 1), lty=1, col='gray')
points( Ht$Ht, 1:nrow(Ht), col='blue', pch=19)
# notice now we have zeros/ones



# data list
dlist = list(
  N = nrow(H),
  K = max(H$utterance), # utterances
  I = nrow(Ht),
  cHS = max(Ht$HS),
  cE = max(Ht$E),
  H = with(H, ifelse(H==0, 0.001, ifelse(H==1, 0.999, H)) ), # trick
  cid = H$child,
  HS = Ht$HS,
  A = with(Ht, A - min(A) ),
  E = Ht$E,
  PTA = c( standardize( Ht$PTA ) ) )



# measurement error model
# centered
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // priors
    a ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "Hbeta.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
res = rstan::read_stan_csv( fit$output_files() )
# divergent transitions (between 100-300 of 4000)



# parameter comparison
# compare1 = precis( res, depth=2, pars=c('aE','aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )
compare1 = precis( res, depth=2, pars=c('aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a','M','SI','Ht') )

true_pars = with(data_mom$par,
                 c( #par$aE * c(1:dlist$cE), 
                   par$aHS * c(1:dlist$cHS),
                   par$bP, par$bA, 
                   par$mu_a, par$s_a, par$mu_the, par$s_the, 
                   a, Ht$M, Ht$SI, Ht$Ht) )

compare1$true = true_pars
compare1$withinCI = with(compare1, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare1
sum(compare1$withinCI)/nrow(compare1)
# poor samples for all parameters, when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model
# good samples for M, SI, Ht (no matter E, HS, or both in model)
# 65% true parameters inside CI (depends on reference)




# contrast comparison
post = extract.samples(res)
# names(post)

contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
nam = c()

# for(p in 1:ncol(post$aE)){
#   for(q in 1:ncol(post$aE)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aE[,p] - post$aE[,q] )
#       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
#     }
#   }
# }

for(p in 1:ncol(post$aHS)){
  for(q in 1:ncol(post$aHS)){
    if(q>p){
      contrast_par = cbind(contrast_par,
                           post$aHS[,p] - post$aHS[,q] )
      nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
    }
  }
}

contrast_par = contrast_par[,-1]
contrast_par = data.frame(contrast_par)
names(contrast_par) = nam

compare2 = precis(contrast_par, depth=2, hist=F)
compare2$true = with( data_mom$par, 
                      c( #-par$aE * c(1:3, 1:2, 1),
                        -par$aHS * c(1:2, 1) ) )
compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare2
# when use E and HS in model, contrasts come all wrong, 
#   because of multicollinearity
#   it does not matter if you increase I or K
# when use only HS in model, contrasts come good, 
#   even with I=32
#   because we break multicollinearity



for_contrast = data.frame( 
  cbind(#post$aE,
    post$aHS) )
names(for_contrast) = c( 
  #paste0('aE[',1:4,']'), 
  paste0('aHS[',1:3,']') )

psych::pairs.panels( for_contrast )
psych::pairs.panels( contrast_par )
# notice the narrow ridge, when use E and HS 
#   indication of multicollinearity (makes sense)
# when use only HS, we also see narrow ridge
#   which is also indication of multicollinearity (how?)








# simulation 6: ####
# 
# details:
# Structure: centered
# Outcome: complex generation different M, zero/one values
# Covariates: 
# E -> HS:
#   HS[E=N]=NH, HS[E=L|M]=HI/HA, HS[E=M|H]=HI/CI
#   some E=M -> HS=HI/HA, and some E=M -> HS=HI/CI (to break multicol)  
# PTA -> HS:
#   positive
#   PTA=L -> HS=NH, PTA=M1|M2 -> HS=HI/HA, PTA=M2|H -> HS=HI/CI
#   PTA range, L=low, M1<M2=mid, H=high
# A -> SI: 
#   positive (more A, more SI)
# HS -> SI: 
#   SI[HS=NH] > SI[HS=HI/CI] > SI[HS=HI/HA]
# E -> SI:
#   negative (higher E, less SI)
#   SI[E=N] > SI[E=L] > SI[E=M] > SI[E=H] 
#   E severity: N=none, L=low, M=mid, H=high 
# PTA -> SI:
#   negative (more PTA, less SI)
#
#   ideally is non-linear
#   SI[PTA=L] > SI[PTA=H] > SI[PTA=M1|M2]
#   PTA range, L=low, M1<M2=mid, H=high
#
# generate data
data_mom = Hsim3() # default 32
# data_mom = Hsim3(I = 100, prop=c(0.34, 0.33, 0.33) )
# data_mom = Hsim3(K = 30 )
Ht = data_mom$Ht
H = data_mom$H


# plot data
plot( H$H, H$child, col='gray', pch=19, xlim=c(0,1), 
      xlab='entropy', ylab='child ID')
abline(h = H$child, lty=2, col='gray')
abline(v = c(0, 1), lty=1, col='gray')
points( Ht$Ht, 1:nrow(Ht), col='blue', pch=19)
# notice now we have zeros/ones



# data list
dlist = list(
  N = nrow(H),
  K = max(H$utterance), # utterances
  I = nrow(Ht),
  cHS = max(Ht$HS),
  cE = max(Ht$E),
  H = with(H, ifelse(H==0, 0.001, ifelse(H==1, 0.999, H)) ), # trick
  cid = H$child,
  HS = Ht$HS,
  A = with(Ht, A - min(A) ),
  E = Ht$E,
  PTA = c( standardize( Ht$PTA ) ) )



# measurement error model
# non-centered
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    int cHS;              // categories in Hearing Status (HS)
    int cE;               // categories in Etiology (E)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
    int HS[I];            // hearing status 
    int A[I];             // hearing age
    int E[I];             // etiology
    real PTA[I];          // (standardized) pta values
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] z_a;        // noncentered intercept (per child)
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    vector[I] z_M;        // noncentered df (per child)
    vector[cE] aE;        // intercept (per E)
    vector[cHS] aHS;      // intercept (per HS)
    real bP;              // slope standardized PTA
    real bA;              // slope (A - A_min)
}
transformed parameters{
    vector[I] a;          // intercept (per child)
    vector[I] M;          // df (per child)
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    a = mu_a + z_a * sigma_a;
    M = exp( mu_the + z_M * sigma_the );
    
    // linear predictor
    for(i in 1:I){
      SI[i] =  a[i] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // SI[i] =  a[i] + aE[E[i]] + aHS[HS[i]] + bA*A[i] + bP*PTA[i];
      // multicollinearity between E and HS
    }
    
    // average entropy (SI -> Ht: negative)
    Ht = inv_logit(-SI);  
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );
    
    // priors
    z_a ~ std_normal();
    z_M ~ std_normal();
    aE ~ normal( 0 , 0.5 );
    aHS ~ normal( 0 , 0.5 );
    bP ~ normal( 0 , 0.3 );
    bA ~ normal( 0 , 0.3 );
    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "Hbeta.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
res = rstan::read_stan_csv( fit$output_files() )
# divergent transitions (between 100-300 of 4000)



# parameter comparison
# compare1 = precis( res, depth=2, pars=c('aE','aHS','bP','bA','mu_a','sigma_a','a','SI','Ht') )
compare1 = precis( res, depth=2, pars=c('aHS','bP','bA','mu_a','sigma_a','mu_the','sigma_the','a','M','SI','Ht') )

true_pars = with(data_mom$par,
                 c( #par$aE * c(1:dlist$cE), 
                   par$aHS * c(1:dlist$cHS),
                   par$bP, par$bA, 
                   par$mu_a, par$s_a, par$mu_the, par$s_the, 
                   a, Ht$M, Ht$SI, Ht$Ht) )

compare1$true = true_pars
compare1$withinCI = with(compare1, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare1
sum(compare1$withinCI)/nrow(compare1)
# great samples for all parameters, when only HS is in the model
#   worst samples for all parameters, when E and HS are in the model
# great samples for M, SI, Ht (no matter E, HS, or both in model)
# 65% true parameters inside CI (depends on reference)




# contrast comparison
post = extract.samples(res)
# names(post)

contrast_par = matrix(NA, nrow=dim(post$mu_a), ncol=1)
nam = c()

# for(p in 1:ncol(post$aE)){
#   for(q in 1:ncol(post$aE)){
#     if(q>p){
#       contrast_par = cbind(contrast_par,
#                            post$aE[,p] - post$aE[,q] )
#       nam = c(nam, paste0('aE[',p,'] - aE[', q, ']'))
#     }
#   }
# }

for(p in 1:ncol(post$aHS)){
  for(q in 1:ncol(post$aHS)){
    if(q>p){
      contrast_par = cbind(contrast_par,
                           post$aHS[,p] - post$aHS[,q] )
      nam = c(nam, paste0('aHS[',p,'] - aHS[', q, ']'))
    }
  }
}

contrast_par = contrast_par[,-1]
contrast_par = data.frame(contrast_par)
names(contrast_par) = nam

compare2 = precis(contrast_par, depth=2, hist=F)
compare2$true = with( data_mom$par, 
                      c( #-par$aE * c(1:3, 1:2, 1),
                        -par$aHS * c(1:2, 1) ) )
compare2$withinCI = with(compare2, as.integer(true>=`5.5%` & true<=`94.5%`) )
compare2
# when use E and HS in model, contrasts come all wrong, 
#   because of multicollinearity
#   it does not matter if you increase I or K
# when use only HS in model, contrasts come good, 
#   even with I=32
#   because we break multicollinearity



for_contrast = data.frame( 
  cbind(#post$aE,
    post$aHS) )
names(for_contrast) = c( 
  #paste0('aE[',1:4,']'), 
  paste0('aHS[',1:3,']') )

psych::pairs.panels( for_contrast )
psych::pairs.panels( contrast_par )
# notice the narrow ridge, when use E and HS 
#   indication of multicollinearity (makes sense)
# when use only HS, we also see narrow ridge
#   which is also indication of multicollinearity (how?)











# simulation 7 ####
# 
# details:
# Outcome = no known process behind (no known M)
# Covariates: not modeled
#
# entropy function
Hfun = function(p, N){
  -sum( ifelse(p == 0, 0,  p*log2(p) ) )/log2(N)
}
#
# simulation function
H_sim4 = function( children=32, words=10, judges=100, max_occ=100 ){
  
  # # data
  # children = 32
  # words = 10 # utterances
  # judges = 100
  # max_occ=10
  
  
  # full transcriptions (diff. numbers count as diff. words)
  full_data = data.frame(matrix(NA, nrow=children*judges, ncol=words+2))
  names(full_data) = c('child', 'judge', paste0('u_', 1:words))
  full_data$child = rep(1:children, each=judges)
  full_data$judge = rep(1:judges, children)
  # dim(full_data)
  # View(full_data)
  
  # c=1; w=10
  for(c in 1:children){
    for(w in 1:words){
      index = with(full_data, child==c) # identify child 
      word_occ = sample(size=1, x=1:max_occ, replace=T) # simulate word occurence
      full_data[index, w+2] = round( runif(judges, min=1, max=word_occ) )
    }
  }
  
  
  # event data
  event_data = data.frame(matrix(NA, nrow=children*max_occ, ncol=words+2))
  names(event_data) = c('child', 'w_occ', paste0('u_', 1:words))
  event_data$child = rep(1:children, each=max_occ)
  event_data$w_occ = rep(1:max_occ, children)
  # dim(event_data)
  # View(event_data)
  
  # c=1; m=1
  for(c in 1:children){
    for(m in 1:max_occ){
      index1 = with(full_data, child==c) # identify child in full_data
      index2 = with(event_data, child==c & w_occ==m) # identify child and event
      event_data[index2, -c(1:2)] = colSums( full_data[index1, -c(1:2)] == m )
    }
  }
  
  # probability data
  prob_data = event_data
  prob_data[,-c(1:2)] = prob_data[,-c(1:2)]/judges
  # dim(prob_data)
  # View(prob_data)
  
  # entropy calculation
  entropy_data = data.frame(matrix(NA, nrow=children, ncol=words+1))
  names(entropy_data) = c('child', paste0('u_', 1:words))
  entropy_data$child = 1:children

  # c=1; w=8
  for(c in 1:children){
    index1 = with(prob_data, child==c)
    index2 = with(entropy_data, child==c)
    entropy_data[index2, -1] = apply( prob_data[index1, -c(1:2)], 2, Hfun, N=judges)
  }
  
  # results report
  return( list(full_data=full_data, 
               event_data=event_data, 
               prob_data=prob_data,
               entropy_data=entropy_data) )
}


# generate data
data_mom = H_sim4()$entropy_data
H = melt(data_mom, id.vars = 'child')
H = H[order(H$child),]


# plot data
plot( H$value, H$child, pch=19, xlim=c(0,1),
      xlab='entropy', ylab='child id') 
abline(h = H$child, lty=2)



# data list
dlist = list(
  N = nrow( H ),
  K = length( unique( H$variable ) ), # utterances
  I = max( H$child ),
  H = with(H, ifelse(value==0, 0.001, ifelse(value==1, 0.999, value)) ),
  cid = H$child )
# notice trick to handle zeroes/ones



# measurement error model
# centered
mcmc_code = "
data{
    int N;                // experimental runs
    int K;                // replicates (utterances)
    int I;                // experimental units (children)
    real H[N];            // replicated entropies
    int cid[N];           // child's id
}
parameters{
    real mu_a;            // mean of population
    real<lower=0> sigma_a;// variability of population
    vector[I] a;          // intercept
    real mu_the;          // mean of df
    real<lower=0> sigma_the;// variability of df
    real<lower=0> M[I];   // df (per child)
}
transformed parameters{
    vector[I] SI;         // true SI index (per child)
    vector[I] Ht;         // true entropy (per child)
    
    SI = a;               // linear predictor
    Ht = inv_logit(-SI);  // average entropy (SI -> Ht: negative)
}
model{
    // hyperpriors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 1 );
    mu_the ~ normal( 0 , 0.5 );
    sigma_the ~ exponential( 1 );

    
    // priors
    a ~ normal( mu_a , sigma_a );
    M ~ lognormal( mu_the , sigma_the );

    
    // likelihood
    for(n in 1:N){
      H[n] ~ beta_proportion( Ht[cid[n]] , M[cid[n]] );
    }
}
"

# cmdstan
set_cmdstan_path('C:/Users/JRiveraEspejo/Documents/.cmdstanr/cmdstan-2.28.1') # in case need it
save_code = "Hbeta.stan"
writeLines(mcmc_code, con=file.path(getwd(), save_code) )
mod = cmdstan_model( file.path(getwd(), save_code) )
fit = mod$sample( data=dlist, chains=4, parallel_chains=4 ) #,init=0, adapt_delta=0.95
res = rstan::read_stan_csv( fit$output_files() )


# final comparison
compare_r = precis( res, depth=2, pars=c('mu_a','sigma_a','mu_the','sigma_the','a','M','SI','Ht') )
compare_r
# good samples for mu_a and sigma_a, 
# a's are  also good
# 100% true parameters inside CI
# no need non-centered because no covariates





# distribution Ht
post = extract.samples(res)

set.seed(12345)
index = sample(x=1:4000, size=100, replace=F)
Ht_mom = post$Ht[index,]
M_mom = post$M[index,]

Ht_mean = colMeans(post$Ht)
M_mean = colMeans(post$M)

index2 = row.names(compare_r) %in% paste0('Ht[',1:32,']')


par(mfrow=c(6,6))

# i=1
for(i in 1:ncol(Ht_mom) ){ # children
  
  # first sample
  curve( dbeta2(x, Ht_mom[1,i], M_mom[1,i]), from=0, to=1, ylim=c(0, 6),
         xlab='Entropy', ylab='Density', col='gray')
  
  # rest of samples
  for(s in 2:nrow(Ht_mom) ){ 
    curve( dbeta2(x, Ht_mom[s,i], M_mom[s,i]), from=0, to=1,
           xlab='Entropy', ylab='Density', col='gray', add=T)
  }
  
  # mean distribution
  curve( dbeta2(x, Ht_mean[i], M_mean[i]), from=0, to=1,
         xlab='Entropy', ylab='Density', col='black', lwd=2.5, add=T)
  points( H$value[H$child==i], rep(0, 10), pch=19, col='blue')
  points( compare_r$mean[index2][i], 0, pch=19, col='red')
  
}

par(mfrow=c(1,1))









# final data ####

# data load
file_dir = 'C:\Users\JRiveraEspejo\Desktop\1. Work\#Classes\PhD Antwerp\#thesis\#data'
dH = read_csv(file.path(file_dir, 'name.csv'))
# str(dH)



# data transform
var_int = c('children_id', 'hearing_status', 'hearing_age', 'etiology', 'pta')
data_mom = unique( dH[, var_int] )
data_mom = data_mom[order(data_mom$children_id),]




