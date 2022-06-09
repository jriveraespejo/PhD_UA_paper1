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





# data ####
data_dir = file.path('C:/Users/JRiveraEspejo/Desktop/1. Work/#Classes/PhD Antwerp/#thesis/#data')
data_nam = "E_data.RData"
model_data = file.path(data_dir, data_nam )
load( model_data )
# dlist


# plotting data
data_plots1(d=dlist, xdata='Am', ydata='H', alpha=0.05, os=F)
data_plots1(d=dlist, xdata='sPTA', ydata='H', alpha=0.05, os=F)
data_plots1(d=dlist, xdata='HS', ydata='H', alpha=0.05, os=F)
data_plots1(d=dlist, xdata='E', ydata='H', alpha=0.05, os=F)
# no relationship






# model load ####

## NC1 ####
model_nam = "E_NC1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC1, true_par = NULL, p=0.95,
#                     est_par=c('a','m_i','s_i','m_b','s_b','SI') )
# str(E_NC1) # no data reporting


## NC2a ####
model_nam = "E_NC2a"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC2a = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC2a, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','SI') )


## NC2b ####
model_nam = "E_NC2b"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC2b = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC2b, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','m_M','SI') )


## NC3 ####
model_nam = "E_NC3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC3, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','m_M','s_M','SI') )


## NC5a1 ####
model_nam = "E_NC5a1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC5a1, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aHS','bAHS','m_i','s_i','m_b','s_b','SI') )


## NC5a2 ####
model_nam = "E_NC5a2"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a2 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC5a2, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aEHS','bA','m_i','s_i','m_b','s_b','SI') )


## NC5a3 ####
model_nam = "E_NC5a3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5a3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC5a3, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','SI') )


## NC5b1 ####
model_nam = "E_NC5b1"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b1 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC5b1, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aHS','bAHS','m_i','s_i','m_b','s_b','m_M','SI') )


## NC5b2 ####
model_nam = "E_NC5b2"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b2 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC5b2, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aEHS','bA','m_i','s_i','m_b','s_b','m_M','SI') )


## NC5b3 ####
model_nam = "E_NC5b3"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC5b3 = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC5b3, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','m_M','SI') )


## NC6a ####
model_nam = "E_NC6a"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6a = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC6a, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aHS','bAHS','m_i','s_i','m_b','s_b','m_M','s_M','SI') )


## NC6b ####
model_nam = "E_NC6b"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6b = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC6b, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aEHS','bA','m_i','s_i','m_b','s_b','m_M','s_M','SI') )


## NC6c ####
model_nam = "E_NC6c"
model_out = file.path(getwd(), 'real_chain')
model_fit = file_id(model_out, model_nam) 
E_NC6c = rstan::read_stan_csv( file.path( model_out, model_fit ) )
# parameter_recovery( stan_object= E_NC6c, true_par = NULL, p=0.95,
#                     est_par=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','m_M','s_M','SI') )






# model analysis ####

## comparison ####
set.seed(12345)
comp_WAIC = rethinking::compare( E_NC1, E_NC2a, E_NC2b, E_NC3, 
                                 E_NC5a1, E_NC5a2, E_NC5a3,
                                 E_NC5b1, E_NC5b2, E_NC5b3,
                                 E_NC6a, E_NC6b, E_NC6c, 
                                 func=WAIC )

comp_PSIS = rethinking::compare( E_NC1, E_NC2a, E_NC2b, E_NC3, 
                                 E_NC5a1, E_NC5a2, E_NC5a3,
                                 E_NC5b1, E_NC5b2, E_NC5b3,
                                 E_NC6a, E_NC6b, E_NC6c,
                                 func=PSIS )

comp_WAIC
comp_PSIS
# Results:
#
# interaction models are indistinguishable from non interaction models
# less evidence in robust models (diff. M for diff. i)
# no evidence in favor of M=10
# Notice E_NC5b3 model has less outliers (see below)



## selection ####
parameter_recovery( stan_object= E_NC2b, true_par = NULL, p=0.95,
                    est_par=c('a','bP','aHS','bA','m_i','s_i','m_b','s_b','m_M') )

parameter_recovery( stan_object= E_NC5b3, true_par = NULL, p=0.95,
                    est_par=c('a','bP','aEHS','bAHS','m_i','s_i','m_b','s_b','m_M') )




## contrasts ####

# E_NC2b
post = extract.samples( E_NC2b )
cont_post = data.frame( post$aHS[,2] - post$aHS[,1] )
colnames(cont_post) = 'aHS[2] - aHS[1]'
  
res_stan = precis( as_tibble(cont_post), depth=4, hist=F, prob=0.95 )
names(res_stan)[3:4] = c('CI_lower','CI_upper')

# get the HPDI
hpdi_res = HPDinterval( as.mcmc(cont_post), prob=0.95)
attr(hpdi_res, 'dimnames')[[2]] = c('HPDI_lower','HPDI_upper') 

res_stan = cbind(res_stan, hpdi_res) # join info
res_stan = round( res_stan, 3 ) # round
res_stan


sum(cont_post < 0)/nrow(cont_post)
sum(post$bP[,2] < 0)/nrow(post$bP)




# E_NC5b3
post = extract.samples( E_NC5b3 )

# levels
cont_post = post$aEHS[,2,2] - post$aEHS[,1,1]
cont_post = cbind(cont_post, 
                  post$aEHS[,3,2] - post$aEHS[,1,1])
cont_post = cbind(cont_post, 
                  post$aEHS[,4,2] - post$aEHS[,1,1])
cont_post = cbind(cont_post, 
                  post$aEHS[,3,2] - post$aEHS[,2,2])
cont_post = cbind(cont_post, 
                  post$aEHS[,4,2] - post$aEHS[,2,2])
cont_post = cbind(cont_post, 
                  post$aEHS[,4,2] - post$aEHS[,3,2])

# slopes
cont_post = cbind(cont_post,
                  post$bAHS[,2] - post$bAHS[,1])

# adding names
attr(cont_post, "dimnames")[[2]] = c('aEHS[2,2] - aEHS[1,1]',
                                     'aEHS[3,2] - aEHS[1,1]',
                                     'aEHS[4,2] - aEHS[1,1]',
                                     'aEHS[3,2] - aEHS[2,2]',
                                     'aEHS[4,2] - aEHS[2,2]',
                                     'aEHS[4,2] - aEHS[3,2]',
                                     'bAHS[2] - bAHS[1]')

# storage
res_stan = precis( as_tibble(cont_post), depth=4, hist=F, prob=0.95 )
names(res_stan)[3:4] = c('CI_lower','CI_upper')


# get the HPDI
hpdi_res = HPDinterval( as.mcmc(cont_post), prob=0.95)
attr(hpdi_res, 'dimnames')[[2]] = c('HPDI_lower','HPDI_upper') 

res_stan = cbind(res_stan, hpdi_res) # join info
res_stan = round( res_stan, 3 ) # round
res_stan
# Results
#
# Variability around parameters does not allow to check if the contrast
# are significantly different from zero


colSums(cont_post < 0)/nrow(cont_post)
colSums(cont_post > 0)/nrow(cont_post)
sum(post$bP[,2] < 0)/nrow(post$bP)



## chain quality ####
# pdf("chains_real1.pdf")
tri_plot(stan_object=E_NC5b3, pars=c('m_i','s_i','m_b','s_b','m_M'))
# dev.off()

# pdf("chains_real2.pdf")
tri_plot(stan_object=E_NC5b3, pars=c('a','bP[2]','bAHS[1]','bAHS[2]') )
# dev.off()

tri_plot(stan_object=E_NC5b3, pars=paste0('aEHS[',1:5,',1]') )
tri_plot(stan_object=E_NC5b3, pars=paste0('aEHS[',1:5,',2]') )
tri_plot(stan_object=E_NC5b3, pars=paste0('re_i[', 1:5,']') )
tri_plot(stan_object=E_NC5b3, pars=paste0('SI[', 1:5,']') )
tri_plot(stan_object=E_NC5b3, pars=paste0('Ht[', 1:5,']') )










## variability plot ####

# plot of variability
post = extract.samples(E_NC5b3)
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
    mtext( text = parse(text=paste0('"Blocks "', '(U[b])')), 3, adj=0, cex=1.5)
    # legend('topleft', legend=c('H=0.2','H=0.5','H=0.8'), bty='n',
    #        lty=rep(1,3), lwd=rep(2,3), cex=1.5,
    #        col=c(rethink_palette[1],rethink_palette[2],rethink_palette[3]) )
  } else{
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=mean(post$s_b) ) ),
          col=rethink_palette[p], lwd=2, add=T)
  }
  
  for(s in 1:length(idx_sample)){
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=post$s_b[idx_sample[s]] ) ),
          col=col.alpha(rethink_palette[p], 0.05), lwd=1, add=T)
  }
  abline(v=mean_val[p], col=rethink_palette[p], lty=2)
}


for(p in 1:length(mean_val) ){
  
  set.seed(12345)
  if(p==1){
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=mean(post$s_i) ) ),
          col=rethink_palette[p], lwd=2, xlab='Entropy', xlim=c(0,1))
    # mtext( paste0('s_i = ', round( mean(post$s_i), 2) ), 3, adj=0, cex=1.5)
    mtext( text = parse(text=paste0('"Children "', '(U[i])')), 3, adj=0, cex=1.5)
  } else{
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=mean(post$s_i) ) ),
          col=rethink_palette[p], lwd=2, add=T)
  }
  
  for(s in 1:length(idx_sample)){
    dens( inv_logit( rnorm(n=1e5, mean=logit(mean_val[p]), sd=post$s_i[idx_sample[s]] ) ),
          col=col.alpha(rethink_palette[p], 0.05), lwd=1, add=T)
  }
  abline(v=mean_val[p], col=rethink_palette[p], lty=2)
}


for(p in 1:length(mean_val) ){
  
  set.seed(12345)
  if(p==1){
    dens( rbeta2(n=1e5, prob=mean_val[p], theta=mean(post$m_M) ),
          col=rethink_palette[p], lwd=2, xlab='Entropy', xlim=c(0,1)) 
    # mtext( paste0('m_M = ', round( mean(post$m_M), 2) ), 3, adj=0, cex=1.5)
    mtext( text = parse(text=paste0('"Replicates "', '(U[ik])')), 3, adj=0, cex=1.5)
  } else{
    dens( rbeta2(n=1e5, prob=mean_val[p], theta=mean(post$m_M) ),
          col=rethink_palette[p], lwd=2, add=T)
  }
  
  for(s in 1:length(idx_sample)){
    dens( rbeta2(n=1e5, prob=mean_val[p], theta=post$m_M[idx_sample[s]] ),
          col=col.alpha(rethink_palette[p], 0.05), lwd=1, add=T)
  }
  abline(v=mean_val[p], col=rethink_palette[p], lty=2)
}

par(mfrow=c(1,1))

# dev.off()







## Ht and SI plots ####
data_true = with(dlist, data.frame(H=H, child=cid, HS=HS[cid]))
par_E_NC5b3 = parameter_recovery( stan_object= E_NC5b3, 
                                  true_par = NULL, 
                                  p=0.95,
                                  est_par=c('SI','Ht') )


idx = str_detect( rownames(par_E_NC5b3), '^Ht')
sc = par_E_NC5b3[idx,]
sc$N = as.integer( str_extract( row.names(sc), '[:digit:]{1,2}') )
sc$HS = dlist$HS
idx_order = with(sc, order(HS, mean) )
sc = sc[idx_order,]

# pdf("posterior_predictive_real2_1.pdf", width=7, height=3.5)
plot( 1:nrow(sc), sc[,'mean'], pch=19, col=rethink_palette[sc$HS], 
      ylim=c(0,1), xaxt='n', xlab="children", ylab=" 'true' entropy (Ht)")
abline(v=16.5, col='gray', lwd=0.5, lty=2)
axis(side=1, at=1:nrow(sc), labels=sc$N, las=2 )
for(i in 1:nrow(sc)){
  lines( x=rep(i, 2), 
         y=sc[i, c('HPDI_lower','HPDI_upper')],
         col=rethink_palette[sc$HS[i]] )
}

lines(x= c(0,16.5), y= rep( mean( sc$mean[sc$HS==1] ), 2),
      col=col.alpha(rethink_palette[1], 0.5), lty=2, lwd=1.5)
polygon( x=c(0,16.5,16.5,0), 
         y=c(rep(quantile( sc$mean[sc$HS==1], 0.025 ), 2),
             rep(quantile( sc$mean[sc$HS==1], 0.975 ), 2)),
         col=col.alpha(rethink_palette[1], 0.15), lty=0, lwd=0.1)

lines(x= c(16.5,33), y= rep( mean( sc$mean[sc$HS==2] ), 2),
      col=col.alpha(rethink_palette[2], 0.5), lty=2, lwd=1.5)
polygon( x=c(16.5,33,33,16.5), 
         y=c(rep(quantile( sc$mean[sc$HS==2], 0.025 ), 2),
             rep(quantile( sc$mean[sc$HS==2], 0.975 ), 2)),
         col=col.alpha(rethink_palette[2], 0.15), lty=0, lwd=0.1)

legend('topleft',legend=c('NH','HI/CI'), col=rethink_palette[1:2], bty='n', pch=19, lty=1)
# dev.off()




idx = str_detect( rownames(par_E_NC5b3), '^SI')
sc = par_E_NC5b3[idx,]
sc$N = as.integer( str_extract( row.names(sc), '[:digit:]{1,2}') )
sc$HS = dlist$HS
sc = sc[ idx_order,]


# pdf("posterior_predictive_real2_2.pdf", width=7, height=3.5)
plot( 1:nrow(sc), sc[,'mean'], pch=19, col=rethink_palette[sc$HS], 
      ylim=c(-2,3.5), xaxt='n', xlab="children", ylab=" speech intelligibility (SI)")
abline(v=16.5, col='gray', lty=2)
axis(side=1, at=1:nrow(sc), labels=sc$N, las=2 )
for(i in 1:nrow(sc)){
  lines( x=rep(i, 2), 
         y=sc[i, c('HPDI_lower','HPDI_upper')],
         col=rethink_palette[sc$HS[i]] )
}
lines(x= c(0,16.5), y= rep( mean( sc$mean[sc$HS==1] ), 2),
      col=col.alpha(rethink_palette[1], 0.5), lty=2, lwd=1.5)
polygon( x=c(0,16.5,16.5,0), 
         y=c(rep(quantile( sc$mean[sc$HS==1], 0.025 ), 2),
             rep(quantile( sc$mean[sc$HS==1], 0.975 ), 2)),
         col=col.alpha(rethink_palette[1], 0.15), lty=0, lwd=0.1)

lines(x= c(16.5,33), y= rep( mean( sc$mean[sc$HS==2] ), 2),
      col=col.alpha(rethink_palette[2], 0.5), lty=2, lwd=1.5)
polygon( x=c(16.5,33,33,16.5), 
         y=c(rep(quantile( sc$mean[sc$HS==2], 0.025 ), 2),
             rep(quantile( sc$mean[sc$HS==2], 0.975 ), 2)),
         col=col.alpha(rethink_palette[2], 0.15), lty=0, lwd=0.1)

par(mfrow=c(1,1))
# dev.off()







## distributional plots ####
data_true = with(dlist, data.frame(H=H, child=cid, HS=HS[cid]))
par_E_NC5b3 = parameter_recovery( stan_object= E_NC5b3, 
                                  true_par = NULL, 
                                  p=0.95,
                                  est_par=c('SI','Ht') )


# pdf("posterior_predictive_real1.pdf")
distH_plot1( stan_object=E_NC5b3, 
             true_data=data_true, 
             par_object=par_E_NC5b3,
             csize=6, 
             rsize=100,
             rplot=c(3,2),
             M=6,
             seed=10)
# dev.off()
# well enough capture of the data










## outlier check ####
WAIC_E = WAIC(E_NC5b3, pointwise=TRUE)
PSIS_E = PSIS(E_NC5b3, pointwise=TRUE)


# pdf("outliers.pdf")
plot( PSIS_E$k, WAIC_E$penalty, col=rangi2, lwd=2, xlim=c(0,0.8),
      xlab="PSIS Pareto k", ylab="WAIC penalty"  )
abline(v=0.5, lty=2)
abline(v=0.7, lty=2, lwd=2)
identify( x=PSIS_E$k , y=WAIC_E$penalty, labels=paste0(dlist$cid, ',', dlist$uid) )
# dev.off()
# 4 observations are outlying



PSIS_E[PSIS_E$k>=0.5,]
obs_out = as.integer( rownames(PSIS_E[PSIS_E$k>=0.5,]) )
child_out = dlist$cid[obs_out] # zero values (fixed with trick)
utt_out = dlist$uid[obs_out] # zero values (fixed with trick)

child_out
dlist$H[obs_out]; psych::describe( dlist$H[!(dlist$H==0.0001)] )
dlist$HS[child_out] 
dlist$E[child_out] 
# dlist$A[child_out]; psych::describe( dlist$A[-child_out] ) 
# dlist$PTA[child_out]; psych::describe( dlist$PTA[-child_out] ) 

idx = dlist$cid %in% child_out #& dlist$uid %in% utt_out
data.frame(cid=dlist$cid[idx], uid=dlist$uid[idx], H=dlist$H[idx])
# it is because they have H=0 (perfect SI)

