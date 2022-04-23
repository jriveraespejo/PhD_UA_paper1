M = c(2,5,10,20,40)

pdf('BetaProp_dist.pdf')
for(i in 1:length(M)){
  if(i==1){
    curve( dbeta2(x, prob=0.5, theta=M[i]), 0, 1, lwd=2, col=i, 
           xlab="", ylab="", ylim=c(0,5))
    legend('topleft', legend=c('M=2','M=5','M=10','M=20','M=40'),
           col=1:length(M), lwd=2, bty='n'  )
  } else{
    curve( dbeta2(x, prob=0.5, theta=M[i]), 0, 1, lwd=2, col=i, xlab="", ylab="", add=T)
  }
}
dev.off()





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