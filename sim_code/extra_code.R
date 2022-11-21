
# measurement error model:
# prior elicitation 
require(rethinking)

n = 1000
alpha = rnorm(n, 0, 0.5)
beta = rnorm(n, 0, 0.3)
gamma = rnorm(n, 0, 0.5)
mu = alpha + beta + gamma

par(mfrow=c(2,1))
hist(mu, breaks = 100)
hist(inv_logit(mu), breaks = 100)
par(mfrow=c(1,1))
psych::describe(inv_logit(mu))


logit(0.99999)



# prior for kappa
hist( rexp(n, 2) + 9, breaks = 100) 
psych::describe(rexp(n, 2) + 9)



# beta distribution plot
mu = seq(0.05, 0.95, by=0.05)

par(mfrow=c(5,4))
for(i in 1:length(mu)){
  curve( dbeta2(x, mu[i], 10) , from=0 , to=1)
}
par(mfrow=c(1,1))


