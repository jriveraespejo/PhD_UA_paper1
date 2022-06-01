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





# varibility in beta distribution ####

M = c(2,5,10,20)
cp = sapply( 1:length(M), col.alpha, alpha=0.7)

# pdf('BetaProp_dist1.pdf')
for(i in 1:length(M)){
  
  
  if(i==1){
    curve( dbeta2(x, prob=0.5, theta=M[i]), 0, 1, lwd=2, col=cp[i], 
           xlab="", ylab="", ylim=c(0,5))
    abline(v=0.5, col='gray', lty=2)
    
    legend('topleft', col=cp, lwd=2, bty='n',
             legend=c(expression(M[i]==2),expression(M[i]==5),
                      expression(M[i]==10),expression(M[i]==20)))
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
               legend=c(expression(M[i]==2),expression(M[i]==5),
                        expression(M[i]==10),expression(M[i]==20)))
      }
    } else{
      curve( dbeta2(x, prob=p[j], theta=M[i]), 0, 1, lwd=2, col=cp[i], xlab="", ylab="", add=T)
    }
  }
}
par(mfrow=c(1,1))
# dev.off()





# data plot ####
icid=mom$dL$cid
mom_plot = with(mom, data.frame(cid=icid, 
                                HS=dL$HS[icid], 
                                E=dL$E[icid],
                                Am=dL$Am[icid], 
                                A=dS$dT$A[icid],
                                PTA=dS$dT$PTA[icid],
                                sPTA=dL$sPTA[icid],
                                H=dS$dO$H) )

# pdf('data_example.pdf')
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
# dev.off()
