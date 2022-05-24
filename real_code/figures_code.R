# preliminar ####
rm(list=ls())

librerias <- c('stringr','dplyr','ggplot2','ggpubr','knitr','tidyverse',
               'reshape2','tinytex','gt','haven',
               'dagitty','ellipse','mvtnorm','MASS','splines','gtools',
               'rethinking','rstan','coda','runjags','rjags',#'loo'
               'cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)


setwd('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/paper1')



# loading sources
source( file.path( getwd(), 'sim_code', '0_sim_extra.R') )
# source( file.path( getwd(), 'real_code', '1_2_E_real_run.R') )



M = c(2,5,10,20)
cp = sapply( 1:length(M), col.alpha, alpha=0.7)

# pdf('BetaProp_dist1.pdf')
for(i in 1:length(M)){
  
  
  if(i==1){
    curve( dbeta2(x, prob=0.5, theta=M[i]), 0, 1, lwd=2, col=cp[i], 
           xlab="", ylab="", ylim=c(0,5))
    abline(v=0.5, col='gray', lty=2)
    
    legend('topleft', col=cp, lwd=2, bty='n',
             legend=c(expression(M[ik]==2),expression(M[ik]==5),
                      expression(M[ik]==10),expression(M[ik]==20)))
  } else{
    curve( dbeta2(x, prob=0.5, theta=M[i]), 0, 1, lwd=2, col=cp[i], xlab="", ylab="", add=T)
  }
}
# dev.off()




p = c(0.2,0.5,0.8)
M = c(2,5,10,20)
cp = sapply( 1:length(M), col.alpha, alpha=0.7)

# pdf('BetaProp_dist2.pdf',width=10, height=5)
par(mfrow=c(1,3))
for(j in 1:length(p)){
  for(i in 1:length(M)){
    
    
    if(i==1){
      curve( dbeta2(x, prob=p[j], theta=M[i]), 0, 1, lwd=2, col=cp[i], 
             xlab="", ylab="", ylim=c(0,5))
      abline(v=p[j], col='gray', lty=2)
      
      if(j==2){
        legend('topleft', col=cp, lwd=2, bty='n',
               legend=c(expression(M[ik]==2),expression(M[ik]==5),
                        expression(M[ik]==10),expression(M[ik]==20)))
      }
    } else{
      curve( dbeta2(x, prob=p[j], theta=M[i]), 0, 1, lwd=2, col=cp[i], xlab="", ylab="", add=T)
    }
  }
}
par(mfrow=c(1,1))
# dev.off()



icid=mom$dL$cid
mom_plot = with(mom, data.frame(cid=icid, 
                                HS=dL$HS[icid], 
                                E=dL$E[icid],
                                Am=dL$Am[icid], 
                                A=dS$dT$A[icid],
                                PTA=dS$dT$PTA[icid],
                                sPTA=dL$sPTA[icid],
                                H=dS$dO$H) )

pdf('data_example.pdf')
par(mfrow=c(2,2))
plot(mom_plot[,c('HS','H')], pch=19, col=col.alpha('black', 0.05))
coef_mom =coefficients( lm(data=mom_plot[, c('H','HS')]) )
abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )

plot(mom_plot[,c('E','H')], pch=19, col=col.alpha('black', 0.05))
coef_mom =coefficients( lm(data=mom_plot[, c('H','E')]) )
abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )

plot(mom_plot[,c('A','H')], pch=19, col=col.alpha('black', 0.05))
coef_mom =coefficients( lm(data=mom_plot[, c('H','A')]) )
abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )

plot(mom_plot[,c('PTA','H')], pch=19, col=col.alpha('black', 0.05))
coef_mom =coefficients( lm(data=mom_plot[, c('H','PTA')]) )
abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
par(mfrow=c(1,1))
dev.off()






## NC2b ####
model_nam = "E_NC2b"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC2b = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC2b, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','m_M','SI') )



# plot of variability
post = extract.samples(E_NC2b)
# str(post)

n=100 # samples from posterior 
set.seed(12345)
idx_sample = sample(1:length(post$a), size=n)


mean_val = c(0.2,0.5,0.8)

# pdf('variability_plot.pdf')

par(mfrow=c(3,1))

for(p in 1:length(mean_val) ){
  
  set.seed(12345)
  if(p==1){
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=mean(post$s_b) ) ),
          col=rethink_palette[p], lwd=2, xlab='Entropy', xlim=c(0,1))
    mtext( paste0('s_b = ', round( mean(post$s_b), 2) ), 3, adj=0, cex=1.5)
    legend('topleft', legend=c('H=0.2','H=0.5','H=0.8'), bty='n',
           lty=rep(1,3), lwd=rep(2,3), cex=1.5,
           col=c(rethink_palette[1],rethink_palette[2],rethink_palette[3]) )
  } else{
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=mean(post$s_b) ) ),
          col=rethink_palette[p], lwd=2, add=T)
  }
  
  for(s in 1:length(idx_sample)){
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=post$s_b[idx_sample[s]] ) ),
          col=col.alpha(rethink_palette[p], 0.05), lwd=1, add=T)
  }
}


for(p in 1:length(mean_val) ){
  
  set.seed(12345)
  if(p==1){
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=mean(post$s_i) ) ),
          col=rethink_palette[p], lwd=2, xlab='Entropy', xlim=c(0,1))
    mtext( paste0('s_i = ', round( mean(post$s_i), 2) ), 3, adj=0, cex=1.5)
  } else{
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=mean(post$s_i) ) ),
          col=rethink_palette[p], lwd=2, add=T)
  }
  
  for(s in 1:length(idx_sample)){
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=post$s_i[idx_sample[s]] ) ),
          col=col.alpha(rethink_palette[p], 0.05), lwd=1, add=T)
  }
}


for(p in 1:length(mean_val) ){
  
  set.seed(12345)
  if(p==1){
    dens( rbeta2(n=1e5, prob=mean_val[p], theta=mean(post$m_M) ),
          col=rethink_palette[p], lwd=2, xlab='Entropy', xlim=c(0,1)) 
    mtext( paste0('m_M = ', round( mean(post$m_M), 2) ), 3, adj=0, cex=1.5)
  } else{
    dens( rbeta2(n=1e5, prob=mean_val[p], theta=mean(post$m_M) ),
          col=rethink_palette[p], lwd=2, add=T)
  }
  
  for(s in 1:length(idx_sample)){
    dens( rbeta2(n=1e5, prob=mean_val[p], theta=post$m_M[idx_sample[s]] ),
          col=col.alpha(rethink_palette[p], 0.05), lwd=1, add=T)
  }
}

par(mfrow=c(1,1))

# dev.off()
