# function:
#     entropy (H)
# description:  
#     To calculate the entropy for all word occurrences.
# arguments:
#     p = vector of normalized frequencies across word occurrences
#     N = number of listeners to calculate normalized frequencies 
#
Hfun = function(p, N){
  -sum( ifelse(p == 0, 0,  p*log2(p) ) )/log2(N)
}


# function:
#     file_id
# description:  
#     it detects all stanfit object of interest in a folder
#     (it only applies for a study simulation with multiple conditions)
# arguments:
#     chains_path = location of csv files corresponding to stanfit objects   
#     model_int = character VECTOR with the names of the models of interest
#
file_id = function(chains_path, model_int){
  
  # # test
  # chains_path = model_out
  # model_int = model_nam
  
  # packages
  require(stringr)
  
  # list all files
  chains_list = list.files( chains_path )
  
  # identify files of interest
  idx1 = str_detect(chains_list, '.csv')
  idx2 = str_detect(chains_list, paste0(model_int, '-[:digit:]', '.csv') )
  chains_list = chains_list[idx1 & idx2]
  
  return(chains_list)
}


# function:
#     data_detect_par
# description:  
#     it detects the names of the parameters in an a simulated data
#     with specific structure.
#     It outputs a integer location vector for the parameters. 
# arguments:
#     d = simulated data
#     par_int = character vector with the names of parameters of interest
#
data_detect_par = function(d, par_int){
  
  # # test
  # d=mom
  # par_int=par_est

  
  # storage parameters
  par_true = c()
  
  
  # true parameters
  par_ext = which( !(par_int %in% c('SI','Ht')) )
  # m=3
  for(m in par_ext){
    idx_par = which( names(d$dS$par) %in% par_int[m] )
    par_mom = d$dS$par[[idx_par]]
    if( is.matrix(par_mom) ){
      par_true = c(par_true, c( t(par_mom) ) )
    } else {
      par_true = c(par_true, par_mom)
    }
    
  }
  
  par_ext = which( par_int %in% c('SI','Ht') )
  if( length(par_ext)!=0 ){
    for( m in par_ext ){
      if(par_int[m] %in% c('SI') ){
        idx_par = which( names(d$dS$dT) %in% 'SI')
      } else if(par_int[m]=='Ht'){
        idx_par = which( names(d$dS$dT) %in% 'Ht')
      }
      par_true = c(par_true, d$dS$dT[,idx_par] )
    }
  }
  
  names(par_true)=NULL
  
  return(par_true)
}


# function:
#     number_detect_par
# description:
#     it detects the names of the parameters in an object generated with
#     the function precis() or parameter_recovery(). 
#     It outputs the location of the parameters (in numbers).
# arguments:
#     precis_object = object containing a 'precis' object
#     est_par = character vector with the names of parameters of interest
#
number_detect_par = function(precis_object, est_par){
  
  # # test
  # precis_object=res_stan
  # est_par=par_est
  
  # packages
  require(stringr)

  # j=1
  for(j in 1:length(est_par)){
    
    # identify parameters of interest
    if(est_par[j] %in% c('a','bA','r') ){
      idx_mom = row.names(precis_object) == est_par[j]
    } else{
      idx_mom = str_detect( row.names(precis_object), paste0('^',est_par[j]) )
    }
    
    # generate output
    if(j==1){
      idx = which(idx_mom)
    } else{
      idx = c( idx, which(idx_mom) )
    }
  }

  return(idx)
}



# function:
#     index_detect_par
# description:  
#     it detects the names of the parameters in an object generated with
#     the function precis() or parameter_recovery().
#     It outputs a boolean vector of the parameters. 
# arguments:
#     precis_object = object containing a 'precis' object'
#     est_par = character vector with the names of parameters of interest
#
index_detect_par = function(precis_object, est_par){
  
  # # test
  # precis_object=res_stan
  # est_par=par_est
  
  # packages
  require(stringr)
  
  # j=1
  for(j in 1:length(est_par)){
    
    # identify parameters of interest
    if(est_par[j] %in% c('a','bA','r') ){
      idx_mom = row.names(precis_object) == est_par[j]
    } else{
      idx_mom = str_detect( row.names(precis_object), paste0('^',est_par[j]) )
    }
    
    # generate output
    if(j==1){
      idx = idx_mom
    } else{
      idx = idx | idx_mom
    }
  }
  
  return(idx)
}





# function:
#     rmse_pars
# description:  
#     calculates the RMSE fro a set of parameters 
# arguments:
#     stats_object = object containing a stanfit object (it can be a list also)
#     est_par = character vector with the names of parameters of interest
#     true_par = vector of values for true parameters
#
rmse_pars = function(stan_object, est_par, true_par){
  
  # # test
  # stan_object = cont_post
  # est_par=c('aHS','bAHS')
  # true_par=true_diff
  # prec = 3
  # seed = 1
  
  # packages
  require(rethinking)
  
  # calculate rmse for samples vs true parameters
  if( typeof(stan_object)=='S4' ){
    post = extract.samples( stan_object )
    idx = names(post) %in% est_par
    post = post[idx]
    
    J = length(est_par) # to run par
  } else if( typeof(stan_object)=='double' ){
    J = 1
  }

  # str(post)
  
  rmse_sim = rep(NA, length(true_par))
  
  # j=1
  for(j in 1:J ){
    
    # extract simulations of parameters
    if( typeof(stan_object)=='S4' ){
      sim_par = post[[est_par[j]]]
    } else if( typeof(stan_object)=='double' ){
      sim_par = stan_object
    }
    
    dimen = dim( sim_par )
    
    if( is.na(dimen[2]) ){
      start = which(is.na(rmse_sim))[1]
      end = start
      rmse_sim[start:end] = sqrt( mean( (sim_par - true_par[start])^2 ) )
      
    } else{
      
      if( !is.na(dimen[3]) ){
        
        for(i in 1:dimen[3]){
          if(i ==1 ){
            sim_par_mom = sim_par[,,i]
          } else{
            sim_par_mom = cbind(sim_par_mom, sim_par[,,i])  
          }
        }
        sim_par = sim_par_mom
        dimen = dim( sim_par )
      }
      
      start = which(is.na(rmse_sim))[1]
      end = (start + dimen[2] - 1)
      true_rep = matrix( rep( true_par[start:end], dimen[1] ),
                         ncol=dimen[2], nrow=dimen[1], byrow=T)
      rmse_sim[start:end] = sqrt( colMeans( (sim_par - true_rep)^2 ) )
    }
  }
  
  # return object
  return(rmse_sim)
  
}





# function:
#     HPDI
# description:  
#     It display the highest posterior density interval (HPDI)
#     for all parameters
# arguments:
#     stan_object = object containing a stanfit object
#     prob = probability of the density
#
HPDI = function(stan_object, p=0.95) {
  
  # # test
  # stan_object=res_C
  # p=0.95
  
  # packages
  require(rstan)
  require(runjags)
  
  # converting 
  samples = As.mcmc.list(stan_object) # to coda mcmc
  samples = combine.mcmc(samples) # combine mcmc
  # str(samples)
  
  # calculating HPDI
  hpdi_res = coda::HPDinterval(samples, prob=p)
  # str(hpdi_res)
  
  return(hpdi_res)
}





# function:
#     set_rope
# description:  
#     it creates a set of ROPE values based on 
#     a set of cutt-offs and reduction/increse values 
# arguments:
#     stan_object = object containing a stanfit object (it can be a list also)
#     est_par = character vector with the names of parameters of interest
#     true_par = vector of values for true parameters
#
set_rope = function(true_par, 
                    cuts=c(0,0.1,0.2,0.5,0.8,1.2,2), # based on Cohen's d
                    rvalues=c(0.2,0.2,0.15,0.3,0.3,0.4,0.8)){ # based on Cohen's d
  
  # # test
  # true_par=par_true
  # cuts=c(0,0.1,0.2,0.5,0.8) # based on Cohen's d
  # rvalues=c(0.2,0.05,0.1,0.15,0.2)
  
  # storage
  par_ROPE = data.frame(ROPE_lower=true_par, 
                        ROPE_upper=true_par,
                        ROPE_prec=0)
  
  # ROPE
  rope_val = abs(true_par)
  
  # cuts==0
  idx = rope_val==cuts[1]
  par_ROPE$ROPE_lower[idx] = true_par[idx] - rvalues[1]
  par_ROPE$ROPE_upper[idx] = true_par[idx] + rvalues[1]
  
  
  # other cuts
  for( j in 2:length(cuts)){
    idx = rope_val > cuts[j-1] & rope_val <= cuts[j]
    par_ROPE$ROPE_lower[idx] = true_par[idx] - rvalues[j]
    par_ROPE$ROPE_upper[idx] = true_par[idx] + rvalues[j]
  }
  
  # final cut
  idx = rope_val > cuts[length(cuts)]
  par_ROPE$ROPE_lower[idx] = true_par[idx] - 1
  par_ROPE$ROPE_upper[idx] = true_par[idx] + 1
  
  # precision
  par_ROPE$ROPE_prec = with(par_ROPE, ROPE_upper-ROPE_lower)
  
  # return object
  return(par_ROPE)
  
}





# function:
#     parameter_recovery
# description:  
#     It display the parameter recovery compared to a set of true parameters.
#     Among the calculations are: mean, sd, compatibility interval, effective
#     sample size Rhat4 for estimated parameters, and comparison against the
#     true parameters like: parameters with same sign (sam_sign), if the 
#     parameter is contained in the compatibility interval (in_CI), and the
#     rmse calculated with a representative sample from the posterior (RMSE).
# arguments:
#     stan_object = object containing a stanfit object (it can be a list also)
#     est_par = character vector with the names of parameters of interest
#     true_par = vector of values for true parameters
#
parameter_recovery = function(stan_object, est_par, true_par,
                              prec=3, p=0.90){
  
  # # test
  # stan_object = res
  # est_par = par_int
  # true_par = par_true
  # p=0.90
  # prec=3
  # seed=1
  
  
  # packages
  require(rethinking)
  
  
  # get the point estimates
  res_stan = precis(stan_object, depth=5, prob=p) #, pars=est_par
  names(res_stan)[3:4] = c('CI_lower','CI_upper')
  
  
  # get the HPDI
  hpdi_res = HPDI(stan_object, p=p)
  rem = which( row.names(hpdi_res) == 'lp__')
  hpdi_res = hpdi_res[-rem,] # remove
  
  rem = which( str_detect( row.names(hpdi_res), 'log_lik') )
  if( length(rem)!=0 ){
    hpdi_res = hpdi_res[-rem,] # remove
  }
  
  attr(hpdi_res, 'dimnames')[[2]] = c('HPDI_lower','HPDI_upper') 
  
  res_stan = cbind(res_stan, hpdi_res) # join info
  
  
  # reordering
  res_stan = res_stan[,c(1:4,7:8,5:6)]
  idx = number_detect_par(res_stan, est_par)
  res_stan = res_stan[idx,]
  
  if( is.null(true_par) ){
    res_stan = round( res_stan, prec ) # round
  }
  
  if( !is.null(true_par) ){
    # introduce true parameters
    res_stan$true = true_par
    
    
    # rmse
    res_stan$RMSE = round( rmse_pars(stan_object, est_par, true_par), prec)  
    
    
    # introduce ROPE
    ROPE = set_rope(true_par)
    res_stan = cbind(res_stan, ROPE)
    
    res_stan = round( res_stan, prec ) # round
    
    
    # same sign?
    res_stan$sign = with(res_stan, as.integer(sign(true) == sign(mean)) )
    
    
    # ROPE goals
    res_stan$reject_null = as.integer( 
      with(res_stan, -0.2>HPDI_upper | 0.2<HPDI_lower ) )
    res_stan$accept_val = as.integer( 
      with(res_stan, ROPE_lower<HPDI_lower | ROPE_upper>HPDI_upper ) )
    res_stan$precision = as.integer( 
      with(res_stan, (HPDI_upper-HPDI_lower) < ROPE_prec ) )
  }
  
  # return object
  return(res_stan)
}


# function:
#     true_contrast
# description:  
#     Extracts the contrast from the data.
# arguments:
#     stan_object = object containing a stanfit object (it can be a list also)
#     est_diff = character vector with the names of parameters of interest
#     true_diff = vector of values for true differences
#
true_contrast = function(d, par_int){
  
  # # test
  # d = mom
  # par_int = par_cont
  
  
  # storage
  cont_true = c()
  cont_name = c()
  

  # parameter contrasts
  par_1 = which( !( par_int %in% c('SI','Ht') ) )
  if( length(par_1)!=0 ){
    
    # m=1
    for(m in par_1){

      # identify parameters
      idx_par = which( names(d$dS$par) %in% par_int[m] )
      
      check = is.matrix(d$dS$par[[idx_par]])
      if(check){
        par_true = d$dS$par[[idx_par]]
        
        # extract par
        # i=1; j=3
        for(i in 1:ncol(par_true)){
          for(j in 1:ncol(par_true)){
            
            if( j>i ){
              if( i == 1 & j==2 & m==1){
                cont_true = par_true[,j] - par_true[,i] 
              } else{
                cont_true = c(cont_true, 
                              par_true[,j] - par_true[,i] ) 
              }
            }
          }
        }
        
      } else{
        par_true = d$dS$par[[idx_par]]

        # extract par
        # i=1; j=2
        for(i in 1:length(par_true)){
          for(j in 1:length(par_true)){
            
            if( j>i ){
              if( i == 1 & j==2 & m==1){
                cont_true = par_true[j] - par_true[i] 
              } else{
                cont_true = c(cont_true, 
                              par_true[j] - par_true[i] ) 
              }
            }
          }
        }
        
      }
      
    }
    
    attr(cont_true, "dimnames") = NULL
  }
  
  
  
  par_2 = which( par_int %in% c('SI','Ht') )
  if( length(par_2)!=0 ){
    
    # m=2
    for(m in par_2){
      
      # identify parameters
      idx_par = which( names(d$dS$dT) %in% par_int[m] )
      par_true = unlist( d$dS$dT[,idx_par] )

      # extract par
      # i=2; j=3
      for(i in 1:length(par_true)){
        for(j in 1:length(par_true)){
          
          if( j>i ){
            cont_true = c(cont_true, 
                          par_true[j] - par_true[i] ) 
          }
        }
      }
    }
    
    attr(cont_true, "dimnames") = NULL
  }
  # View(cont_true)

  # return object
  return( c(cont_true) )
}





# function:
#     contrast_recovery
# description:  
#     It display the parameter contrasts recovery compared to a set of true 
#     differences.
# arguments:
#     stan_object = object containing a stanfit object (it can be a list also)
#     est_diff = character vector with the names of parameters of interest
#     true_diff = vector of values for true differences
#     p = probability for HPDI
#
contrast_recovery = function(stan_object, est_diff, true_diff, 
                             p=0.90, prec=3, seed=1){
  
  # # test
  # stan_object=res 
  # est_diff = par_cont
  # true_diff = diff_true
  # p=0.90
  # prec=3
  # seed=1

  
  # packages
  require(rethinking)
  require(tibble)
  require(coda)
  
  
  # extract samples
  post = extract.samples( stan_object )
  idx = names(post) %in% est_diff
  post = post[idx]
  # names(post)
  
  
  # calculations
  # k=2
  for(k in 1:length(est_diff)){
    
    post_mom = post[[k]]
    
    # matrix contrast
    check = !is.na(dim(post_mom)[3])
    if( check ){
      
      # i=1;j=2
      for(i in 1:dim(post_mom)[3] ){
        for(j in 1:dim(post_mom)[3] ){
          
          if( j>i ){
            if( i == 1 & j==2 & k==1 ){
              
              cont_post = post_mom[,,j] - post_mom[,,i]
              cont_name = paste0( est_diff[k], 
                                  '[', 1:dim(post_mom)[2],
                                  ',', j,']', 
                                  '-',
                                  est_diff[k], 
                                  '[', 1:dim(post_mom)[2],
                                  ',',i,']' )
            } else{
              cont_post = cbind(cont_post, 
                                post_mom[,,j] - post_mom[,,i] ) 
              cont_name = c(cont_name, 
                            paste0( est_diff[k], 
                                    '[', 1:dim(post_mom)[2],
                                    ',', j,']', 
                                    '-',
                                    est_diff[k], 
                                    '[', 1:dim(post_mom)[2],
                                    ',',i,']' ) )
            }
          }
        }
      }
      
      attr(cont_post, "dimnames")[[2]] = cont_name
      
    } else {
      
      # selecting parameter
      par_name = paste0( est_diff[k], '[', 1:ncol(post_mom), ']' )
      
      # i=1;j=2
      for(i in 1:ncol(post_mom)){
        for(j in 1:ncol(post_mom)){
          
          if( j>i ){
            if( i == 1 & j==2 & k==1 ){
              cont_post = post_mom[,j] - post_mom[,i] 
              cont_name = paste( c(par_name[j], par_name[i]), collapse='-' )
            } else{
              cont_post = cbind(cont_post, 
                                post_mom[,j] - post_mom[,i] ) 
              cont_name = c(cont_name, 
                            paste( c(par_name[j], par_name[i]), collapse='-' ) )
            }
          }
        }
      }
      
      attr(cont_post, "dimnames")[[2]] = cont_name
      
    }
  }
  
  # storage
  res_stan = precis( as_tibble(cont_post), 
                     depth=4, hist=F, prob=p )
  names(res_stan)[3:4] = c('CI_lower','CI_upper')
  
  # get the HPDI
  hpdi_res = HPDinterval( as.mcmc(cont_post), prob=p)
  attr(hpdi_res, 'dimnames')[[2]] = c('HPDI_lower','HPDI_upper') 
  
  res_stan = cbind(res_stan, hpdi_res) # join info
  
  
  if( is.null(true_diff) ){
    res_stan = round( res_stan, prec ) # round
  }
  
  
  if( !is.null(true_diff) ){
    
    # extra lines
    res_stan$n_eff = NA
    res_stan$Rhat4 = NA
    
    
    # introduce true parameters
    res_stan$true = true_diff
    
    
    # rmse
    res_stan$RMSE = round( rmse_pars(cont_post, est_diff, true_diff), prec)  
    
    
    # introduce ROPE
    ROPE = set_rope(true_diff)
    res_stan = cbind(res_stan, ROPE)
    
    res_stan = round( res_stan, prec ) # round
    
    
    # same sign?
    res_stan$sign = with(res_stan, as.integer(sign(true) == sign(mean)) )
    
    
    # ROPE goals
    res_stan$reject_null = as.integer( 
      with(res_stan, -0.2>HPDI_upper | 0.2<HPDI_lower ) )
    res_stan$accept_val = as.integer( 
      with(res_stan, ROPE_lower<HPDI_lower | ROPE_upper>HPDI_upper ) )
    res_stan$precision = as.integer( 
      with(res_stan, (HPDI_upper-HPDI_lower) < ROPE_prec ) )
  }
  
  # return object
  return(res_stan)
}




# function:
#     data_plots
# description:  
#     It plots all relevant data plots 
# arguments:
#     d = data from simulation
#     xdata = string identifying data to plot on x axis
#             options: A, Am, PTA, sPTA, HS, E
#     ydata = string identifying data to plot on y axis
#             options: H, HJ, SI
#     alpha = color parameter
#     os = plot the original scale (default os=F)
#           available only for: HJ, CJD, CJO
#     reduce = reduced data (default reduce=F)
#           available only for: HJ, CJD, CJO
#
data_plots = function(d, xdata, ydata, alpha=0.15, os=F, reduce=F){
  
  # # test
  # d=mom
  # xdata='HS'
  # ydata='H'
  # alpha=0.15
  # os=F
  # reduce=T
  
  # working data
  if( ydata=='CJD' ){
    icid = d$dL$cid1
  } else{
    icid = d$dL$cid
  }
  mom_plot = with(d, data.frame(cid=icid, 
                                HS=dL$HS[icid], 
                                E=dL$E[icid],
                                Am=dL$Am[icid], 
                                A=dS$dT$A[icid],
                                PTA=dS$dT$PTA[icid],
                                sPTA=dL$sPTA[icid] ) )
  if( ydata=='H'){
    mom_plot[,ydata] = d$dL[ydata]
  } else if( ydata=='HJ' & !reduce ){
    mom_plot[,ydata] = d$dL[ydata]
    mom_plot$HJo = d$dS$dO$HJo
  } else if( ydata=='HJ' & reduce ){
    mom_plot$m_HJ = d$dL$m_HJ
    mom_plot$s_HJ = d$dL$s_HJ
  } else if( ydata=='CJD' ){
    mom_plot[,ydata] = d$dL[ydata]
    mom_plot = mom_plot %>%
      group_by( cid ) %>%
      summarise( across( everything() , ~mean(.x, na.rm=T))) %>%
      data.frame()
  }
  
  if( ydata=='HJ' & os){
    ydata='HJo'
  } else if ( ydata=='HJ' & reduce ){
    ydata='m_HJ'
  } 
  
  # making plots
  if( xdata=='Am' | xdata=='A' | xdata=='PTA' | xdata=='sPTA' ){
    
    par(mfrow=c(2,2))
    
    plot(mom_plot[,c(xdata,ydata)], pch=19, col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==1,c(xdata,ydata)], pch=19, main='HS==1', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==1, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==2,c(xdata,ydata)], pch=19, main='HS==2', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==2, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==3,c(xdata,ydata)], pch=19, main='HS==3', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==3, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    par(mfrow=c(1,1))  
    
  } else if( xdata=='HS' ){
    
    par(mfrow=c(2,2))
    
    cxdata = unique(mom_plot[,c(xdata)])
    plot(mom_plot[,c(xdata,ydata)], pch=19, xaxt="n", col=col.alpha('black', alpha))
    axis(side=1, at=cxdata, labels = T)
    coef_mom =coefficients( lm(data=mom_plot[, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    # plot(mom_plot[mom_plot$E==1,c(xdata,ydata)], pch=19, main='E==1', col=col.alpha('black', alpha))
    # coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==1, c(ydata, xdata)]) )
    # abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    # abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$E==2,c(xdata,ydata)], pch=19, main='E==2', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$E==2, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$E==3,c(xdata,ydata)], pch=19, main='E==3', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$E==3, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$E==4,c(xdata,ydata)], pch=19, main='E==4', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$E==4, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    par(mfrow=c(1,1))
    
  } else if( xdata=='E' ){
    
    par(mfrow=c(2,2))
    
    cxdata = unique(mom_plot[,c(xdata)])
    plot(mom_plot[,c(xdata,ydata)], pch=19, xaxt="n", col=col.alpha('black', alpha))
    axis(side=1, at=cxdata, labels = T)
    coef_mom =coefficients( lm(data=mom_plot[, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==1,c(xdata,ydata)], pch=19, main='HS==1', col=col.alpha('black', alpha))
    # coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==1, c(ydata, xdata)]) )
    # abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==2,c(xdata,ydata)], pch=19, main='HS==2', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==2, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==3,c(xdata,ydata)], pch=19, main='HS==3', col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==3, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    par(mfrow=c(1,1))
    
  }
  
}



# function:
#     data_plots
# description:  
#     It plots all relevant data plots (for the real dataset) 
# arguments:
#     d = data from simulation
#     xdata = string identifying data to plot on x axis
#             options: A, Am, PTA, sPTA, HS, E
#     ydata = string identifying data to plot on y axis
#             options: H, HJ, SI
#     alpha = color parameter
#     os = plot the original scale (default os=F)
#           available only for: HJ, CJD, CJO
#     reduce = reduced data (default reduce=F)
#           available only for: HJ, CJD, CJO
#
data_plots1 = function(d, xdata, ydata, alpha=0.15, os=F, reduce=F){
  
  # # test
  # d=dlist
  # xdata='sPTA'
  # ydata='H'
  # alpha=0.15
  # os=F
  # reduce=T
  
  # working data
  if( ydata=='CJD' ){
    icid = d$cid1
  } else{
    icid = d$cid
  }
  mom_plot = with(d, data.frame(cid=icid, 
                                HS=HS[icid], 
                                E=E[icid],
                                Am=Am[icid], 
                                sPTA=sPTA[icid] ) )
  if( ydata=='H'){
    mom_plot[,ydata] = d[ydata]
  } else if( ydata=='HJ' & !reduce ){
    mom_plot[,ydata] = d[ydata]
    mom_plot$HJo = d$HJo
  } else if( ydata=='HJ' & reduce ){
    mom_plot$m_HJ = dm_HJ
    mom_plot$s_HJ = ds_HJ
  } else if( ydata=='CJD' ){
    mom_plot[,ydata] = d[ydata]
    mom_plot = mom_plot %>%
      group_by( cid ) %>%
      summarise( across( everything() , ~mean(.x, na.rm=T))) %>%
      data.frame()
  }
  
  if( ydata=='HJ' & os){
    ydata='HJo'
  } else if ( ydata=='HJ' & reduce ){
    ydata='m_HJ'
  } 
  
  # making plots
  if( xdata=='Am' | xdata=='A' | xdata=='PTA' | xdata=='sPTA' ){
    
    par(mfrow=c(2,2))
    
    plot(mom_plot[,c(xdata,ydata)], pch=19, col=col.alpha('black', alpha))
    coef_mom =coefficients( lm(data=mom_plot[, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==1,c(xdata,ydata)], pch=19, main='HS==1', col=col.alpha('black', alpha))
    check = length( unique( mom_plot[mom_plot$HS==1, xdata] ) ) > 1
    if( check ){
      coef_mom = coefficients( lm(data=mom_plot[mom_plot$HS==1, c(ydata, xdata)]) )
      abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    }
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==2,c(xdata,ydata)], pch=19, main='HS==2', col=col.alpha('black', alpha))
    check = length( unique( mom_plot[mom_plot$HS==2, xdata] ) ) > 1
    if( check ){
      coef_mom = coefficients( lm(data=mom_plot[mom_plot$HS==2, c(ydata, xdata)]) )
      abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    }
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    # plot(mom_plot[mom_plot$HS==3,c(xdata,ydata)], pch=19, main='HS==3', col=col.alpha('black', alpha))
    # coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==3, c(ydata, xdata)]) )
    # abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    # abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    par(mfrow=c(1,1))  
    
  } else if( xdata=='HS' ){
    
    par(mfrow=c(2,2))
    
    cxdata = unique(mom_plot[,c(xdata)])
    plot(mom_plot[,c(xdata,ydata)], pch=19, xaxt="n", col=col.alpha('black', alpha))
    axis(side=1, at=cxdata, labels = T)
    coef_mom =coefficients( lm(data=mom_plot[, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    # plot(mom_plot[mom_plot$E==1,c(xdata,ydata)], pch=19, main='E==1', col=col.alpha('black', alpha))
    # coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==1, c(ydata, xdata)]) )
    # abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    # abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$E==2,c(xdata,ydata)], pch=19, main='E==2', col=col.alpha('black', alpha))
    check = length( unique( mom_plot[mom_plot$E==2, xdata] ) ) > 1
    if( check ){
      coef_mom = coefficients( lm(data=mom_plot[mom_plot$E==2, c(ydata, xdata)]) )
      abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    }
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$E==3,c(xdata,ydata)], pch=19, main='E==3', col=col.alpha('black', alpha))
    check = length( unique( mom_plot[mom_plot$E==3, xdata] ) ) > 1
    if( check ){
      coef_mom = coefficients( lm(data=mom_plot[mom_plot$E==3, c(ydata, xdata)]) )
      abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    }
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$E==4,c(xdata,ydata)], pch=19, main='E==4', col=col.alpha('black', alpha))
    check = length( unique( mom_plot[mom_plot$E==4, xdata] ) ) > 1
    if( check ){
      coef_mom = coefficients( lm(data=mom_plot[mom_plot$E==4, c(ydata, xdata)]) )
      abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    }
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    par(mfrow=c(1,1))
    
  } else if( xdata=='E' ){
    
    par(mfrow=c(2,2))
    
    cxdata = unique(mom_plot[,c(xdata)])
    plot(mom_plot[,c(xdata,ydata)], pch=19, xaxt="n", col=col.alpha('black', alpha))
    axis(side=1, at=cxdata, labels = T)
    coef_mom =coefficients( lm(data=mom_plot[, c(ydata, xdata)]) )
    abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==1,c(xdata,ydata)], pch=19, main='HS==1', col=col.alpha('black', alpha))
    check = length( unique( mom_plot[mom_plot$HS==1, xdata] ) ) > 1
    if( check ){
      coef_mom = coefficients( lm(data=mom_plot[mom_plot$HS==1, c(ydata, xdata)]) )
      abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    }
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    plot(mom_plot[mom_plot$HS==2,c(xdata,ydata)], pch=19, main='HS==2', col=col.alpha('black', alpha))
    check = length( unique( mom_plot[mom_plot$HS==2, xdata] ) ) > 1
    if( check ){
      coef_mom = coefficients( lm(data=mom_plot[mom_plot$HS==2, c(ydata, xdata)]) )
      abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    }
    abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    # plot(mom_plot[mom_plot$HS==3,c(xdata,ydata)], pch=19, main='HS==3', col=col.alpha('black', alpha))
    # coef_mom =coefficients( lm(data=mom_plot[mom_plot$HS==3, c(ydata, xdata)]) )
    # abline(a=coef_mom[1], b=coef_mom[2], col='gray', lwd=2 )
    # abline(h=0.5, col=col.alpha('red',0.2), lty=2, lwd=1.5 )
    
    par(mfrow=c(1,1))
    
  }
  
}




# function:
#     data_plots
# description:  
#     It plots all relevant data plots 
# arguments:
#     precis_object = data produced from precis() function
#     var_str = string identifying data to plot
#     yrange = range for y axis
#     a = alpha for color in plot
#
prior_plots = function(precis_obj, var_str, yrange=c(0,1), a=0.2){
  
  # # test
  # precis_obj=par_prior
  # var_str='H'
  # yrange=c(0,1)
  # a=0.2
  # precis_obj[idx,]
  
  # plot
  vars_str = paste0('^',var_str, '[:punct:][:digit:]{1,2}[:punct:]')
  idx = which( str_detect(row.names(par_prior), vars_str) )
  plot(1:length(idx), precis_obj$mean[idx], pch=19, 
       col=col.alpha('black',a),
       xlab='children id', ylab=var_str, ylim=yrange)
  for(i in 1:length(idx)){
    lines( x=rep(i,2), y=with(precis_obj, c(`5.5%`[idx[i]],`94.5%`[idx[i]]) ),
           col=col.alpha('black',a))
  }
  
}





# function:
#     recovery_plots
# description:  
#     It plots all relevant recovery plots 
# arguments:
#     par_object = object generated with parameter_recovery() function
#     con_object = object generated with contrast_recovery() function
#     par_plot = parameters to plot
#
recovery_plots = function(par_object, cont_object=NULL,
                          par_plot){
  
  # # test
  # par_object=par_recovery_NC 
  # cont_object=cont_recovery_NC
  # par_plot=c('m_i','s_i','m_M','a','aHS','aE','bP','bA','SI')
  
  # figure parameters
  opar = par()
  
  # parameters of interest
  par_int = list( p1 = c('m_i','s_i','m_M','s_M','m_j','s_j','m_k'),
                  p1 = c('r','s_SI','s_HJ'),
                  p3 = c('a','aHS','aE','aEHS','bP','bA','bAHS'),
                  p4 = 'Rho',
                  p5 = 're_i',
                  p6 = 're_j',
                  p7 = 'M',
                  p8 = c('SI','m_SI'),
                  p9 = 'Ht' )
  
  
  # selecting variables to plot
  for( i in 1:length(par_int) ){
    idx = par_int[[i]] %in% par_plot
    par_int[[i]] = par_int[[i]][idx]
  }
  idx = sapply(par_int, length)!=0
  par_int = par_int[idx]
  
  
  par_mom = list()
  par_row = c()
  
  # identify parameter
  for(i in 1:length(par_int)){
    idx = index_detect_par(par_object, est_par=par_int[[i]] )
    par_mom[[i]] = par_object[idx,]
    par_row = c(par_row, nrow(par_mom[[i]]) )     # zero data
  }
  par_mom = par_mom[par_row!=0] # only available data
  par_n = length(par_mom)
  
  
  # contrast data
  if( !is.null(cont_object) ){
    
    for(i in 1:length(par_int)){
      idx = index_detect_par(cont_object, est_par=par_int[[i]] )
      par_mom[[i+par_n]] = cont_object[idx,]
      par_row = c(par_row, nrow(par_mom[[i+par_n]]) )     # zero data
    }
    par_mom = par_mom[par_row!=0] # only available data
  }
  
  
  # plot data
  par(mfrow=c( ceiling(length(par_mom)/2) , 2), mar=c(8,4,4,2)+0.1)
  
  # i=2
  for(i in 1:length(par_mom)){
  
    # plot parameters
    y_lim = range( with(par_mom[[i]], c(HPDI_lower, HPDI_upper, mean, true)), na.rm=T )
    
    plot(1:nrow(par_mom[[i]]), par_mom[[i]]$mean, ylim=y_lim, xaxt='n', yaxt='n',
         col=col.alpha('blue', 0.3), pch=19, 
         ylab='estimates', xlab='', main='')
    # abline(v=c(5,10,15,20)+0.5, col=col.alpha('black', 0.5), lty=2)
    abline(h=0, col=col.alpha('black', 0.3), lty=2)
    axis(side=1, at=1:nrow(par_mom[[i]]), labels=rownames(par_mom[[i]]), las=2)
    axis(side=2, at=round( seq(y_lim[1], y_lim[2], by=0.2), 2), las=1)
    for(j in 1:nrow(par_mom[[i]])){
      lines(x=rep(j,2), y=with( par_mom[[i]][j,], c(HPDI_lower, HPDI_upper) ) ,
            col=col.alpha('blue', 0.3))
    }
    points(1:nrow(par_mom[[i]]), par_mom[[i]]$true, col=col.alpha('black', 1), pch=3)
    legend('topleft', legend=c('estimate','true'), pch=c(19,3), cex=.8, bty='n',
           col=c(col.alpha('blue', 0.3), col.alpha('black', 0.5)) )
    
  }
  
  par(mfrow=c(1,1), mar=opar$mar)
  
}



# function:
#     trace_plot
# description:  
#     It creates a trace plot for a stanfit object 
# arguments:
#     stan_object = object produced after using run.jags
#     pars = character with the name of a parameter
#
trace_plot = function(stan_object, pars) {
  
  # # test
  # stan_object = res
  # pars = 'mu_a'
  
  # packages 
  require(RColorBrewer)
  require(rethinking)
  
  # posterior
  post = rstan::extract(stan_object, pars=pars, permuted=FALSE)
  # str(post)
  
  # parameters
  n_chains = dim(post)[2]
  chain.cols = rep_len(rethink_palette, n_chains)
  wstart = 1
  wend = dim(post)[1]
  ylim = range(post[wstart:wend, , ])
  ytick = (ylim[2] - ylim[1])/6
  yaxis = round( seq(ylim[1], ylim[2], by=ytick), 2)
  neff = summary(stan_object)$summary[, "n_eff"]
  neff_use <- neff[names(neff) == pars]
  
  # plot
  plot(NULL, type="l", xlim=c(wstart, wend), ylim=ylim,
       xlab="", ylab="", axes=F)
  box(bty="l")
  axis(side=1, at=seq(0, wend, by=100))
  axis(side=2, at=yaxis, las=1 )
  #mtext(paste("n_eff =", round(neff_use, 0)), 3, adj = 1, cex = 1.1)
  mtext(pars, 3, adj = 0, cex=1.1)
  for(c in 1:n_chains){
    lines(1:wend, post[, c, ], col=chain.cols[c], lwd = 0.5)
  }
  
}



# function:
#     trank_plot
# description:  
#     It creates a trank plot for a stanfit object 
# arguments:
#     stan_object = object produced after using run.jags
#     pars = character with the name of a parameter
#     wide = controls the number of iterations (in the chain) considered.
#           (default 50)
#
trank_plot = function(stan_object, pars, wide=50){
  
  # # test
  # stan_object = res
  # pars = 'mu_a'
  # wide=50
  
  # for colors
  require(RColorBrewer)
  require(rethinking)
  
  # posterior
  post = rstan::extract(stan_object, pars=pars, permuted=FALSE)
  # str(post)
  
  # parameters
  n_chains = dim(post)[2]
  chain.cols = rep_len(rethink_palette, n_chains)
  wstart = 1
  wend = dim(post)[1]
  neff = summary(stan_object)$summary[, "n_eff"]
  neff_use <- neff[names(neff) == pars]
  
  # rank calculation
  ranks = list()
  xrange = rep(1:wide, each=2)
  yrange = vector('list', n_chains)
  for(c in 1:n_chains){
    ranks[[c]] = rank( post[1:(wide+1), c, ] )
    y_ran = c()
    for(i in 2:(wide+1)){
      y_ran = c(y_ran, c( ranks[[c]][i-1], ranks[[c]][i] ) )
    }
    yrange[[c]] = y_ran
  }
  
  
  # plot
  plot(NULL, type='l', xlim=c(0, wide+1), ylim=c(0, wide+1),
       xlab="", ylab="", xaxt ="n", yaxt ="n", axes=F)
  box(bty="l")
  # mtext(paste("n_eff =", round(neff_use, 0)), 3, adj = 1, cex = 0.9)
  # mtext(pars, 3, adj = 0, cex = 1)
  for(c in 1:n_chains){
    lines(xrange, yrange[[c]], col=chain.cols[c], lwd=1.5)  
  }
  
}


# function:
#     acf_plot
# description:  
#     It creates a acf plot for a stanfit object 
# arguments:
#     stan_object = object produced after using run.jags
#     pars = character with the name of a parameter
#
acf_plot = function(stan_object, pars){
  
  # # test
  # stan_object = res
  # pars = 'mu_a'
  
  # posterior
  post = rstan::extract(stan_object, pars=pars, permuted=FALSE)
  # str(post)
  
  # plot
  acf( post[, 1, ], main='', xlab='', ylab='', mar = c(0, 0, 0, 0) )
  # mtext(paste("n_eff =", round(neff_use, 0)), 3, adj = 1, cex = 0.9)
  # mtext(pars, 3, adj = 0, cex = 1)
  
}


# function:
#     tri_plot
# description:  
#     it plots trace, trank, and ACF plots for a maximul of 5 parameters
# arguments:
#     stan_object = object containing a 'precis' object   
#     pars = character VECTOR with the names of parameters of interest
#             only plots a maximum of five (5) parameters.
#
tri_plot = function(stan_object, pars){
  
  # # test
  # stan_object = stan_model
  # pars = paste0('m_b[',1:5,']')
  
  # figure parameters
  opar = par()
  
  # ensure there is only 5 paramaters
  if(length(pars)>5){
    pars = pars[1:5]
  }
  
  # plot
  par(mfrow=c(length(pars), 3), mar=c(3,3.5,1.5,1)+0.1)
  
  for(i in 1:length(pars)){
    trace_plot(stan_object, pars=pars[i]) 
    trank_plot(stan_object, pars=pars[i])
    acf_plot(stan_object, pars=pars[i])
  }
  
  par(mfrow=c(1,1), mar=opar$mar)
  
}





# function:
#     stat_chain
# description:  
#     It extracts the number of effective samples (n_eff) and Rhat statistics
#     from two parameter_recovery() objects
#     Used to compared the centered vs non-centered parametrization.
# arguments:
#     par_object1 = object generated with parameter_recovery() function
#                   (centered parametrization)
#     par_object2 = object generated with parameter_recovery() function
#                   (non-centered parametrization)
#     pars = character vector with the names of parameters of interest
#
stat_chain = function(par_object1, par_object2, pars){
  
  # # test
  # par_object1=par_recovery_C
  # par_object2=par_recovery_NC
  # pars='a_i'
  
  # identify parameters
  idx1 = number_detect_par(par_object1, pars)
  idx2 = number_detect_par(par_object2, pars)
  
  # storage
  stat_mom = list()
  
  # saving info
  stat_mom$n_eff = cbind(par_object1$n_eff[idx1], par_object2$n_eff[idx2])
  stat_mom$n_eff = data.frame(stat_mom$n_eff)
  names(stat_mom$n_eff) = c('neff_C','neff_NC')
  rownames(stat_mom$n_eff) = rownames(par_object1)[idx1]
  
  stat_mom$Rhat = cbind(par_object1$Rhat4[idx1], par_object2$Rhat4[idx2])
  stat_mom$Rhat = data.frame(stat_mom$Rhat)
  names(stat_mom$Rhat) = c('neff_C','neff_NC')
  rownames(stat_mom$Rhat) = rownames(par_object1)[idx1]
  
  # return object
  return(stat_mom)
}


# function:
#     stat_plot
# description:  
#     It plots the n_eff and Rhat from a centered and non-centered models
# arguments:
#     par_object1 = object generated with parameter_recovery() function
#                   (centered parametrization)
#     par_object2 = object generated with parameter_recovery() function
#                   (non-centered parametrization)
#     pars = character vector with the names of parameters of interest
#     cChain = cut-off chain size (default 4000 = 1000 samples x 4 chains)
#     cRhat = cut-off Rhat (default = 1.05)
#
stat_plot = function(par_object1, par_object2, pars, cChain=4000, cRhat=1.05){
  
  # # test
  # par_object1=par_recovery_C
  # par_object2=par_recovery_NC
  # pars=pars='a_i'

  # identify the parameters
  stat_mom = stat_chain(par_object1, par_object2, pars)
  
  # plot
  par(mfrow=c(1, 2))

  range_lim = range( stat_mom$n_eff )
  range_lim[1] = ifelse(range_lim[1]>=cChain, cChain-200, range_lim[1])
  range_lim[2] = ifelse(range_lim[2]<=cChain, cChain+200, range_lim[2])
  x_lim = y_lim = range_lim
  plot(stat_mom$n_eff, xlim=x_lim, ylim=x_lim, main='N effective',
       col=col.alpha('black', 0.4), pch=19,
       xlab='Centered parametrization', ylab='Non-centered parametrization')
  abline(a=0, b=1, lty=2)
  abline(v=cChain, h=cChain, lty=2)
  
  range_lim = range( stat_mom$Rhat )
  range_lim[1] = ifelse(range_lim[1]>=cRhat, 1-0.005, range_lim[1])
  range_lim[2] = ifelse(range_lim[2]<=cRhat, cRhat+0.005, range_lim[2])
  x_lim = y_lim = range_lim
  plot(stat_mom$Rhat, xlim=x_lim, ylim=x_lim, main='Rhat',
       col=col.alpha('black', 0.4), pch=19,
       xlab='Centered parametrization', ylab='Non-centered parametrization')
  abline(v=cRhat, h=cRhat, lty=2)
  
  par(mfrow=c(1, 1))
  
}




# function:
#     distH_plot
# description:  
#     plots the estimated density and observed entropies per children
# arguments:
#     stan_object = object containing a stanfit object (it can be a list also)
#     true_data = repeated measures for entropy per child
#     par_object = object generated with parameter_recovery() function
#     M = shape parameter in dbeta2, default = 10
#     rsize = number of samples form posterior (default = 100)
#     csize = number of children sampled (default = 16)
#     rplot = layout for plot 
#     model = model for which we made the plot (default=H, entropy)
#             available models: H, HJ, CJD, CJO
#     alpha1, alpha2 = color alpha parameters
#
distH_plot = function(stan_object, true_data, par_object, 
                      M=10, rsize=100, csize=6, seed=1,
                      rplot=c(3,2),
                      alpha1=0.3, alpha2=0.4){ 
  
  # # test
  # stan_object = E_NC5b3
  # true_data = data_true
  # par_object = par_recovery
  # M=NULL
  # rsize=100
  # csize=16
  # seed=1
  # rplot=c(3,2)
  
  
  # distribution Ht
  post = extract.samples(stan_object)
  
  # sampling entropy observations
  nchain= dim(post$SI)
  set.seed(seed)
  idx_row = sample(x=1:nchain[1], size=rsize, replace=F)
  idx_col = sample(x=1:nchain[2], size=csize, replace=F)
  idx_col = idx_col[order(idx_col)]
  
  
  # extracting info
  out_mom = post$Ht[idx_row, idx_col]
  out_mean = colMeans(post$Ht)[idx_col] # mean distribution
  
  if( !is.null(M) & length(M)==1 ){
    M_mom = matrix( rep(M, nrow(out_mom)*ncol(out_mom) ), 
                    ncol=ncol(out_mom) )
    M_mean = colMeans(M_mom) # mean distribution
  } else {
    M_mom = post$M[idx_row, idx_col]
    M_mean = colMeans(post$M)[idx_col] # mean distribution
  }
  
  
  # identify Ht mean values and confidence intervals
  idx = number_detect_par(par_object, 'Ht')
  par_object = par_object[idx,]
  par_object = par_object[idx_col,]
  
  
  # plot
  par(mfrow=rplot)
  
  # i=1
  for(i in 1:ncol(out_mom) ){ # children
    
    # first sample
    curve( dbeta2(x, out_mom[1,i], M_mom[1,i]), from=0, to=1, ylim=c(0, 6),
           xlab='Entropy', ylab='Density', col=col.alpha('gray',alpha1) )
    mtext(paste("child = ", idx_col[i]), 3, adj = 1, cex = 1.1)
    
    # rest of samples
    for(s in 2:nrow(out_mom) ){
      curve( dbeta2(x, out_mom[s,i], M_mom[s,i]), from=0, to=1, add=T, 
             xlab='Entropy', ylab='Density', col=col.alpha('gray',alpha1))
    }
    
    # mean distribution
    curve( dbeta2(x, out_mean[i], M_mean[i]), from=0, to=1, lwd=2.5,
           xlab='Entropy', ylab='Density', col='black', add=T)
    
    
    # observations
    points( true_data$H[true_data$child==idx_col[i]], 
            rep( 0.3, sum(true_data$child==idx_col[i]) ), 
            pch=19, col=col.alpha('blue', alpha2))
    points( par_object$mean[i], 0, pch=19, col='red')
    lines(x=with(par_object[i,], c(HPDI_lower, HPDI_upper)), 
          y=rep(0, 2), col='red')
    
  }
  
  par(mfrow=c(1,1))
  
}


# function:
#     distH_plot1
# description:  
#     plots the estimated density and observed entropies per children
#     for real data set
# arguments:
#     stan_object = object containing a stanfit object (it can be a list also)
#     true_data = repeated measures for entropy per child
#     par_object = object generated with parameter_recovery() function
#     M = shape parameter in dbeta2, default = 10
#     rsize = number of samples form posterior (default = 100)
#     csize = number of children sampled (default = 16)
#     rplot = layout for plot 
#     model = model for which we made the plot (default=H, entropy)
#             available models: H, HJ, CJD, CJO
#     alpha1, alpha2 = color alpha parameters
#
distH_plot1 = function(stan_object, true_data, par_object, 
                      M=10, rsize=100, csize=6, seed=1,
                      rplot=c(3,2),
                      alpha1=0.3, alpha2=0.4){ 
  
  # # test
  # stan_object = E_NC5b3
  # true_data = data_true
  # par_object = par_recovery
  # M=6
  # rsize=100
  # csize=6
  # seed=32
  # rplot=c(3,2)
  # alpha1=0.3
  # alpha2=0.4
  
  
  # distribution Ht
  post = extract.samples(stan_object)
  
  # sampling entropy observations
  nchain= dim(post$SI)
  set.seed(seed)
  idx_row = sample(x=1:nchain[1], size=rsize, replace=F)
  
  mom = unique(true_data[,c('child','HS')])
  mom = mom[order(mom$HS),]
  
  idx_col = sample(mom$child[mom$HS==1], size=csize/2, replace=F)
  idx_col = c(idx_col,  
              sample(mom$child[mom$HS==2], size=csize/2, replace=F))
  idx_HS = c( rep(1, csize/2), rep(2, csize/2) )
  
  
  # extracting info
  out_mom = post$Ht[idx_row, idx_col]
  out_mean = colMeans(post$Ht)[idx_col] # mean distribution
  
  if( !is.null(M) & length(M)==1 ){
    M_mom = matrix( rep(M, nrow(out_mom)*ncol(out_mom) ), 
                    ncol=ncol(out_mom) )
    M_mean = colMeans(M_mom) # mean distribution
  } else {
    M_mom = post$M[idx_row, idx_col]
    M_mean = colMeans(post$M)[idx_col] # mean distribution
  }
  
  
  # identify Ht mean values and confidence intervals
  idx = number_detect_par(par_object, 'Ht')
  par_object = par_object[idx,]
  par_object = par_object[idx_col,]
  
  
  # plot
  par(mfrow=rplot)
  layout(matrix(1:csize, ncol=rplot[2], byrow=F))
  
  # i=1
  for(i in 1:ncol(out_mom) ){ # children
    
    # first sample
    curve( dbeta2(x, out_mom[1,i], M_mom[1,i]), from=0, to=1, ylim=c(0, 6),
           xlab='Entropy', ylab='Density', col=col.alpha('gray',alpha1) )
    mtext(paste("child = ", idx_col[i]), 3, adj = 1, cex = 1.1)
    
    # rest of samples
    for(s in 2:nrow(out_mom) ){
      curve( dbeta2(x, out_mom[s,i], M_mom[s,i]), from=0, to=1, add=T, 
             xlab='Entropy', ylab='Density', col=col.alpha('gray',alpha1))
    }
    
    # mean distribution
    curve( dbeta2(x, out_mean[i], M_mean[i]), from=0, to=1, lwd=2.5,
           xlab='Entropy', ylab='Density', col='black', add=T)
    
    # observations
    points( true_data$H[true_data$child==idx_col[i]], 
            rep( 0.3, sum(true_data$child==idx_col[i]) ), 
            pch=19, col=rethink_palette[idx_HS[i]])
    points( par_object$mean[i], 0, pch=19, col='red')
    lines(x=with(par_object[i,], c(`5.5%`, `94.5%`)), 
          y=rep(0, 2), col='red')
    
    if(i==1){
      legend('topright', legend=c('NH','HI/CI'), 
             col=rethink_palette[1:2], bty='n', pch=19)
    }
    
  }
  
  par(mfrow=c(1,1))
  
}



# # function:
# #     distHJ_plot
# # description:  
# #     plots the estimated density and observed HJ scores per children
# # arguments:
# #     stan_object = object containing a stanfit object (it can be a list also)
# #     true_data = repeated measures for entropy per child
# #     par_object = object generated with parameter_recovery() function
# #     M = shape parameter in dbeta2, default = 10
# #     rsize = number of samples form posterior (default = 100)
# #     csize = number of children sampled (default = 16)
# #     model = model for which we made the plot (default=H, entropy)
# #             available models: H, HJ, CJD, CJO
# #     alpha1, alpha2 = color alpha parameters
# #
# distHJ_plot = function(stan_object, true_data, par_object, 
#                       M=10, rsize=100, csize=16, seed=1,
#                       alpha1=0.3, alpha2=0.4){ 
#   
#   # test
#   stan_object = res_C
#   true_data = data_true
#   par_object = par_recovery_C
#   rsize=100
#   csize=16
#   seed=1
#   
#   
#   # distribution Ht
#   post = extract.samples(stan_object)
#   # str(post)
#   
#   # sampling entropy observations
#   nchain= dim(post$SI)
#   set.seed(seed)
#   idx_row = sample(x=1:nchain[1], size=rsize, replace=F)
#   idx_col = sample(x=1:nchain[2], size=csize, replace=F)
#   idx_col = idx_col[order(idx_col)]
#   
#   
#   # extracting info
#   out_mom = post$SI[idx_row, idx_col]
#   out_mean = colMeans(post$SI)[idx_col] # mean distribution
#   out_sd = apply(post$SI, 2, sd)[idx_col]
#   
#   # if( length(s_)==1 ){
#   #   M_mom = matrix( rep(M, nrow(out_mom)*ncol(out_mom) ), 
#   #                   ncol=ncol(out_mom) )
#   #   M_mean = colMeans(M_mom) # mean distribution
#   # } else {
#   #   M_mom = post$M[idx_row, idx_col]
#   #   M_mean = colMeans(post$M)[idx_col] # mean distribution
#   # }
#   
#   
#   # identify Ht mean values and confidence intervals
#   idx = number_detect_par(par_object, 'SI')
#   par_object = par_object[idx,]
#   par_object = par_object[idx_col,]
#   
#   
#   # plot
#   par(mfrow=c(4,4))
#   
#   # i=1
#   for(i in 1:ncol(out_mom) ){ # children
#     
#     # first sample
#     curve( dnorm(x, out_mom[1,i], M_mom[1,i]), from=0, to=1, ylim=c(0, 6),
#            xlab='Entropy', ylab='Density', col=col.alpha('gray',alpha1) )
#     mtext(paste("child = ", idx_col[i]), 3, adj = 1, cex = 1.1)
#     
#     # rest of samples
#     for(s in 2:nrow(out_mom) ){
#       curve( dbeta2(x, out_mom[s,i], M_mom[s,i]), from=0, to=1, add=T, 
#              xlab='Entropy', ylab='Density', col=col.alpha('gray',alpha1))
#     }
#     
#     # mean distribution
#     curve( dbeta2(x, out_mean[i], M_mean[i]), from=0, to=1, lwd=2.5,
#            xlab='Entropy', ylab='Density', col='black', add=T)
#     
#     
#     # observations
#     points( true_data$H[true_data$child==idx_col[i]], 
#             rep( 0.3, sum(true_data$child==idx_col[i]) ), 
#             pch=19, col=col.alpha('blue', alpha2))
#     points( par_object$mean[i], 0, pch=19, col='red')
#     lines(x=with(par_object[i,], c(`5.5%`,`94.5%`)), y=rep(0, 2), col='red')
#     
#   }
#   
#   par(mfrow=c(1,1))
#   
# }







# copied function from psych package
pairs_panels = function (x, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE, 
                         digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE, 
                         jiggle = FALSE, factor = 2, show.points = TRUE,
                         hist.col = "cyan", points.col = "cyan", # MODIFIED
                         rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, smoother = FALSE, 
                         stars = FALSE, ci = FALSE, alpha = 0.05, ...) 
{
  "panel.hist.density" <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1], usr[2], 0, 1.5))
    tax <- table(x)
    if (length(tax) < 11) {
      breaks <- as.numeric(names(tax))
      y <- tax/max(tax)
      interbreak <- min(diff(breaks)) * (length(tax) - 
                                           1)/41
      rect(breaks - interbreak, 0, breaks + interbreak, 
           y, col = hist.col)
    }
    else {
      h <- hist(x, breaks = breaks, plot = FALSE)
      breaks <- h$breaks
      nB <- length(breaks)
      y <- h$counts
      y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    }
    if (density) {
      tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd", 
                               adjust = 1.2), silent = TRUE)
      if (!inherits(tryd, "try-error")) {
        d$y <- d$y/max(d$y)
        lines(d)
      }
    }
    if (rug) 
      rug(x)
  }
  
  "panel.cor" <- function(x, y, prefix = "", ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    if (is.null(wt)) {
      r <- cor(x, y, use = "pairwise", method = method)
    }
    else {
      r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1, 
                                                        2]
    }
    txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (stars) {
      pval <- r.test(sum(!is.na(x * y)), r)$p
      symp <- symnum(pval, corr = FALSE, cutpoints = c(0, 
                                                       0.001, 0.01, 0.05, 1), symbols = c("***", "**", 
                                                                                          "*", " "), legend = FALSE)
      txt <- paste0(txt, symp)
    }
    cex <- cex.cor * 0.8/(max(strwidth("0.12***"), strwidth(txt)))
    if (scale) {
      cex1 <- cex * abs(r)
      if (cex1 < 0.25) 
        cex1 <- 0.25
      text(0.5, 0.5, txt, cex = cex1)
    }
    else {
      text(0.5, 0.5, txt, cex = cex)
    }
  }
  
  "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = "red", 
                               span = 2/3, iter = 3) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) 
        points(x, y, pch=pch, col=points.col) # MODIFIED
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      if (smooth & ci) {
        lml <- loess(y ~ x, degree = 1, family = "symmetric")
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE), 
                                    max(x, na.rm = TRUE), length.out = 47))
        pred <- predict(lml, newdata = tempx, se = TRUE)
        if (ci) {
          upperci <- pred$fit + confid * pred$se.fit
          lowerci <- pred$fit - confid * pred$se.fit
          polygon(c(tempx$x, rev(tempx$x)), c(lowerci, 
                                              rev(upperci)), col = adjustcolor("light grey", 
                                                                               alpha.f = 0.8), border = NA)
        }
        lines(tempx$x, pred$fit, col = col.smooth, ...)
      }
      else {
        if (smooth) 
          lines(stats::lowess(x[ok], y[ok], f = span, 
                              iter = iter), col = col.smooth)
      }
    }
    if (ellipses) 
      draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.smooth, 
                   ...)
  }
  
  "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red", 
                         ...) {
    ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin, xmin), max(ymax, xmax))
    xlim <- ylim
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ylim = ylim, xlim = xlim, 
               ...)
      }
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lml <- lm(y ~ x)
      if (ci) {
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE), 
                                    max(x, na.rm = TRUE), length.out = 47))
        pred <- predict.lm(lml, newdata = tempx, se.fit = TRUE)
        upperci <- pred$fit + confid * pred$se.fit
        lowerci <- pred$fit - confid * pred$se.fit
        polygon(c(tempx$x, rev(tempx$x)), c(lowerci, 
                                            rev(upperci)), col = adjustcolor("light grey", 
                                                                             alpha.f = 0.8), border = NA)
      }
      if (ellipses) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.lm, 
                     ...)
      }
      abline(lml, col = col.lm, ...)
    }
  }
  
  "draw.ellipse" <- function(x = 0, y = 0, xs = 1, ys = 1, 
                             r = 0, col.smooth, add = TRUE, segments = 51, ...) {
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0) 
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + x
      ellipse[, 2] <- ellipse[, 2] * ys + y
      if (show.points) 
        points(x, y, pch = 19, col = col.smooth, cex = 1.5)
      lines(ellipse, ...)
    }
  }
  
  "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red", 
                              ...) {
    segments = 51
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1] - abs(0.05 * usr[1]), usr[2] + abs(0.05 * 
                                                            usr[2]), 0, 1.5))
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ...)
      }
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0) 
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + xm
      ellipse[, 2] <- ellipse[, 2] * ys + ym
      points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
      if (ellipses) 
        lines(ellipse, ...)
    }
  }
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (missing(cex.cor)) 
    cex.cor <- 1
  for (i in 1:ncol(x)) {
    if (is.character(x[[i]])) {
      x[[i]] <- as.numeric(as.factor(x[[i]]))
      colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
    }
  }
  n.obs <- nrow(x)
  confid <- qt(1 - alpha/2, n.obs - 2)
  if (!lm) {
    if (cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
            lower.panel = panel.smoother, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.smoother, 
            lower.panel = panel.smoother, pch = pch, ...)
    }
  }
  else {
    if (!cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm, 
            lower.panel = panel.lm, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
            lower.panel = panel.lm, pch = pch, ...)
    }
  }
}