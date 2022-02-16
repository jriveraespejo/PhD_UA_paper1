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
  # chains_path = file.path(getwd(), 'chains_post')
  # model_int = c('FOLV_CE_mod','FOLV_CE','FOLV_NC_mod','FOLV_NC','SOLV_CE','SOLV_NC')
  # 
  # list all files
  chains_list = list.files( chains_path )
  
  # identify files of interest
  idx1 = str_detect(chains_list, '.csv')
  idx2 = str_detect(chains_list, model_int)
  chains_list = chains_list[idx1 & idx2]
  
  return(chains_list)
}


# function:
#     number_detect_par
# description:
#     it detects the names of the parameters in an object generated with
#     the function precis(). 
#     It outputs the location of the parameters (in numbers).
# arguments:
#     precis_object = object containing a 'precis' object
#     est_par = character vector with the names of parameters of interest
#
number_detect_par = function(precis_object, est_par){
  
  # # test
  # precis_object=res_stan
  # est_par=par_est

  # j=1
  for(j in 1:length(est_par)){
    
    # identify parameters of interest
    if(est_par[j]=='a'){
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
#     the function precis().
#     It outputs a boolean vector of the parameters. 
# arguments:
#     precis_object = result object generated with rmse_pars() object   
#     est_par = character vector with the names of parameters of interest
#
index_detect_par = function(precis_object, est_par){
  
  # # test
  # precis_object=res_stan
  # est_par=par_est
  
  # j=1
  for(j in 1:length(est_par)){
    
    # identify parameters of interest
    if(est_par[j]=='a'){
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
rmse_pars = function(stan_object, est_par, true_par, seed=1){
  
  # # test
  # stan_object = res
  # prec = 3
  # seed = 1

  
  # calculate rmse for samples vs true parameters
  set.seed(seed)
  post = extract.samples( stan_object )
  idx = names(post) %in% est_par
  post = post[idx]
  # str(post)
  
  rmse_sim = rep(NA, length(true_par))
  
  # j=11
  for(j in 1:length(est_par) ){
    
    # extract simulations of parameters
    sim_par = post[[est_par[j]]]
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
                              diff=F, prec=3, seed=1){
  
  # # test
  # stan_object=res
  # est_par=par_est
  # true_par=par_true
  # prec=3
  # seed=1
  
  # get the point estimates
  res_stan = precis(stan_object, depth=4) #, pars=est_par
  
  # identify parameters of interest
  idx = number_detect_par(res_stan, est_par)
  res_stan = round( res_stan[idx,], prec) #
  
  
  # introduce true parameters
  res_stan$true = round(true_par, prec)
  
  # do the parameters have the same sign
  res_stan$same_sign = with(res_stan, as.integer(sign(true) == sign(mean)) )
  
  # identify if parameters are inside compatibility interval
  res_stan$in_CI = with(res_stan, as.integer(true>=`5.5%` & true<=`94.5%`) )
  
  # rmse
  res_stan$RMSE = round( rmse_pars(stan_object, est_par, true_par, seed=seed), prec)  
  
  # return object
  return(res_stan)
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
#
contrast_recovery = function(stan_object, est_diff, true_diff, prec=3, seed=1){
  
  # # test
  # stan_object=res
  # est_diff='aHS'
  # true_diff=true_diff
  # prec=3
  # seed=1
  
  # extract samples
  set.seed(seed)
  post = extract.samples( stan_object )
  idx = names(post) %in% est_diff
  post = post[idx]
  # names(post)
  
  # calculating results
  res_stan = precis(stan_object, depth=4)
  
  # calculations
  # k=1
  for(k in 1:length(est_diff)){
    
    # selecting parameter
    idx = number_detect_par(res_stan, est_diff[k])
    lab_par = rownames(res_stan)[idx]
    npars = length(idx)
    
    # storage
    # i=2
    # j=3
    for(i in 1:npars){
      for(j in 1:npars){
        
        if(j>i){
          if(i == 1 & j==2 & k==1){
            diff = post[[est_diff[k]]][,j] - post[[est_diff[k]]][,i] 
            diff_name = paste( c( lab_par[j], lab_par[i]), collapse=' - ' )
          } else{
            diff = cbind(diff ,
                         post[[est_diff[k]]][,j] - post[[est_diff[k]]][,i] ) 
            diff_name = c(diff_name, 
                          paste( c( lab_par[j], lab_par[i]), collapse=' - ' ) )
          }
        }
        
      }
    }
    
  }
  
  # storage
  diff = as_tibble(diff)
  names(diff) = diff_name
  res_diff = precis( diff, depth=4, hist=F )
  res_diff[,1:4] = round( res_diff[,1:4], prec)
  
  # introduce true differences
  res_diff$true = round( true_diff, prec)
  
  # do the parameters have the same sign
  res_diff$same_sign = with(res_diff, as.integer(sign(true) == sign(mean)) )
  
  # identify if parameters are inside compatibility interval
  res_diff$in_CI = with(res_diff, as.integer(true>=`5.5%` & true<=`94.5%`) )
  
  # rmse
  dimen = dim(diff)
  true_rep = matrix( rep( true_diff, dimen[1] ),
                     ncol=dimen[2], nrow=dimen[1], byrow=T)
  rmse_sim = sqrt( colMeans( (diff - true_rep)^2 ) )
  res_diff$RMSE = round(rmse_sim, prec)  
  
  return(res_diff)
}



# function:
#     recovery_plots
# description:  
#     It plots all relevant recovery plots 
# arguments:
#     par_object = object generated with parameter_recovery() function
#     con_object = object generated with contrast_recovery() function
#
recovery_plots = function(par_object, cont_object=NULL){
  
  # # test
  # par_object = par_recovery
  # cont_object = NULL
  
  
  # figure parameters
  opar = par()
  
  
  # parameters of interest
  par_int = list( pop_par = c('mu_a','sigma_a','mu_the','sigma_the'), 
                  reg_par = c('a','aHS','aE','bP','bA'),
                  ext_par1 = 'a_i',
                  ext_par2 = 'M',
                  ext_par3 = 'SI',
                  ext_par4 = 'Ht')
  
  par_mom = list()
  par_row = c()
  
  
  # identify parameter
  # i=2;j=1
  for(i in 1:length(par_int)){
    idx = index_detect_par(par_object, est_par=par_int[[i]] )
    par_mom[[i]] = par_object[idx,]
    par_row = c(par_row, nrow(par_mom[[i]]) )     # zero data
  }
  par_mom = par_mom[par_row!=0] # only available data
  
  
  # contrast data
  if( !is.null(cont_object) ){
    par_mom = c( par_mom, list(cont_object) )
  }
  
  
  # plot data
  par(mfrow=c( ceiling(length(par_mom)/2) , 2), mar=c(8,4,4,2)+0.1)
  
  # i=2
  for(i in 1:length(par_mom)){
  
    # plot parameters
    y_lim = range( with(par_mom[[i]], c(`5.5%`, `94.5%`, mean, true)), na.rm=T )
    
    plot(1:nrow(par_mom[[i]]), par_mom[[i]]$mean, ylim=y_lim, xaxt='n', yaxt='n',
         col=col.alpha('blue', 0.3), pch=19, 
         ylab='estimates', xlab='', main='')
    # abline(v=c(5,10,15,20)+0.5, col=col.alpha('black', 0.5), lty=2)
    abline(h=0, col=col.alpha('black', 0.3), lty=2)
    axis(side=1, at=1:nrow(par_mom[[i]]), labels=rownames(par_mom[[i]]), las=2)
    axis(side=2, at=round( seq(y_lim[1], y_lim[2], by=0.2), 2), las=1)
    for(j in 1:nrow(par_mom[[i]])){
      lines(x=rep(j,2), y=with( par_mom[[i]][j,], c(`5.5%`, `94.5%`) ) ,
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
  mtext(paste("n_eff =", round(neff_use, 0)), 3, adj = 1, cex = 1.1)
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
  
  # test
  stan_object = res
  pars = 'mu_a'
  
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
#     n_effective
# description:  
#     It joins the number of effective samples from two stan models.
#     normally is used to compared the centered vs non-centered version.
# arguments:
#     stan_object1 = object containing a stanfit object
#     stan_object2 = object containing a stanfit object
#     est_par = character vector with the names of parameters of interest
#
n_effective = function(stan_object1, stan_object2, est_par){
  
  # stan model 1 (centered)
  precis1 = precis(stan_object1, depth=4)
  idx1 = number_detect_par(precis1, est_par)
  
  # stan model 2 (non-centered)
  precis2 = precis(stan_object2, depth=4)
  idx2 = number_detect_par(precis2, est_par)
  
  # plot
  neff_table = data.frame(
    centered=precis1$n_eff[idx1],
    non_centered=precis2$n_eff[idx2]
  )
  
  return(neff_table)
}


# function:
#     stat_chain
# description:  
#     it extraxt the information of Rhat and n_eff 
# arguments:
#     c_list = data.frame generated with file_id() function
#     chains_path = location of csv files corresponding to stanfit objects
#     file_save = location to save files
#     file_name = name of the file to save 
#     contr_pars = set of paramerter to calculate contrasts
#
stat_chain = function(c_list, chains_path, file_save, file_name, contr_pars){
  
  # # test
  # c_list=chains_list
  # chains_path=chains_path
  # file_save=file.path(getwd(), 'figures4')
  # file_name = 'stan_stats_no_mod'
  # contr_pars = c('b_G','b_E','b_X')
  
  # parameters
  models_int = unique(c_list$model)
  sample_sizes = unique(c_list$sample)
  data_number = unique(c_list$data)
  
  # m=1
  # s=1
  # d=1
  for(m in 1:length(models_int)){
    for(s in 1:length(sample_sizes)){
      for(d in 1:length(data_number)){
        
        ####
        # simple parameter section
        ####
        
        # identify files of interest
        idx_files = with(chains_list, 
                         model==models_int[m] &
                           sample==sample_sizes[s] &
                           data == data_number[d])
        
        fit_files = chains_list[idx_files, 1]
        stan_model = rstan::read_stan_csv( file.path(chains_path, fit_files) )
        
        # calculate parameters
        stan_result = precis(stan_model, depth=4)
        
        # storage
        if( all(m==1, s==1, d==1) ){
          stan_int = data.frame( model_type = models_int[m],
                                 sample_size = sample_sizes[s],
                                 data_number = data_number[d],
                                 parameter = row.names(stan_result),
                                 stan_result )
        } else {
          stan_int = rbind(stan_int,
                           data.frame( model_type = models_int[m],
                                       sample_size = sample_sizes[s],
                                       data_number = data_number[d],
                                       parameter = row.names(stan_result),
                                       stan_result ) )
        }
        
        ####
        # contrasts section
        ####
        
        # extract samples
        post = extract.samples( stan_model )
        idx = names(post) %in% contr_pars
        
        if( !all(idx==F) ){
          
          post = post[idx]
          # names(post)
          
          # calculations
          # k=1
          for(k in 1:length(contr_pars) ){
            
            # selecting parameter
            idx = number_detect_par(stan_result, contr_pars[k])
            lab_par = rownames(stan_result)[idx]
            npars = length(idx)
            
            # storage
            # i=2
            # j=3
            for(i in 1:npars){
              for(j in 1:npars){
                
                if(j>i){
                  if(i == 1 & j==2 & k==1){
                    diff = post[[contr_pars[k]]][,j] - post[[contr_pars[k]]][,i] 
                    diff_name = paste( c( lab_par[j], lab_par[i]), collapse=' - ' )
                  } else{
                    diff = cbind(diff ,
                                 post[[contr_pars[k]]][,j] - post[[contr_pars[k]]][,i] ) 
                    diff_name = c(diff_name, 
                                  paste( c( lab_par[j], lab_par[i]), collapse=' - ' ) )
                  }
                }
                
              }
            }
            
          }
          
          # calculations
          diff = data.frame(diff)
          names(diff) = diff_name
          res_diff = precis( diff, depth=4 )
          
          # contrast storage
          res_diff = data.frame( model_type = models_int[m],
                                 sample_size = sample_sizes[s],
                                 data_number = data_number[d],
                                 parameter = row.names(res_diff),
                                 res_diff[,-5], 
                                 n_eff = NA,
                                 Rhat4 = NA)
          stan_int = rbind(stan_int, res_diff)
        }
        
        # remove row.names
        row.names(stan_int) = NULL
        
        # remove unnecessary parameters
        uu = c('zb_k','L_Rho_theta_sub','ztheta_sub','ztheta','m_mult','m_theta')
        for(i in 1:length(uu)){
          idx_mom = str_detect(stan_int$parameter, uu[i])
          if(i==1){
            idx = idx_mom
          } else{
            idx = idx | idx_mom
          }
        }
        stan_int = stan_int[!idx,]
        
        # save file
        save(stan_int, file=file.path(file_save, paste0(file_name, '.RData') ) )
        print( paste0(models_int[m], '_J', sample_sizes[s], '_Ndata', data_number[d]) )
        
      }
    }
  }
  
}



# function:
#     plot_stat
# description:  
#     It plots the statistics of interest 
# arguments:
#     stat_object = object produced by the function stat_chain()
#     info = statistics of interest
#     par_int = parameters of interest
#     model = options according to model in stat_chain
#     ssize = sample size of simulation
#     title = main title for the plot 
#
plot_stat = function(stat_object, info, par_int, model, ssize=100, title='(A)'){
  
  # # test
  # stat_object = stan_int
  # info = 'n_eff'
  # par_int = c( paste0('b_G[',1:2,']'),'b_A', paste0('b_E[',1:3,']'),
  #              paste0('b_X[',1:4,']'))
  # model = 'FOLV'
  # ssize = 100
  # title='(A)'
  
  
  # identify the parameters
  # i=1
  for(i in 1:length(par_int)){
    idx_pars_mom = stat_object$parameter == par_int[i]
    if(i==1){
      idx_pars = idx_pars_mom
    } else{
      idx_pars = idx_pars | idx_pars_mom
    }
  }
  # sum(idx_pars)
  
  # identify models
  model_int = unique(with(stat_object, model_type[str_detect(model_type, model)]))
  for(i in 1:length(model_int)){
    idx = with(stat_object, model_type==model_int[i] & sample_size==ssize & idx_pars)
    stat_mom = stat_object[idx, c('sample_size','data_number','parameter', info)]
    
    if(i==1){
      stat_final = stat_mom
    } else{
      stat_final = merge(stat_final, stat_mom, 
                         by=c('sample_size','data_number','parameter'))
    }
  }
  
  # parameter color
  par_mod = str_locate(stat_final$parameter, '[:digit:]')[,1] - 2
  par_mod = ifelse( is.na(par_mod), 3, par_mod)
  par_mod = str_sub(stat_final$parameter, start = 1, end=par_mod)
  par_mod_un = unique(par_mod)
  col_pars = rep(NA, nrow(stat_final))
  for(i in 1:length(par_mod_un)){
    col_pars = ifelse( par_mod==par_mod_un[i], 
                       col.alpha(i, 0.4), col_pars) 
  }
  
  # plot
  idx_var = str_detect( names(stat_final), info)  
  x_lim = range(stat_final[, idx_var])
  plot(stat_final[,idx_var], xlim=x_lim, ylim=x_lim, main=title,
       col=col_pars, pch=19,
       xlab='Centered parametrization', ylab='Non-centered parametrization')
  abline(a=0, b=1, lty=2)
  
  if(info=='Rhat4'){
    abline(v=1.05, h=1.05, lty=3, col=col.alpha('black', 0.6))
    legend('top', horiz=T, par_mod_un, pch=19, col=unique(col_pars), bty='n')
  } else{
    legend('bottom', horiz=T, par_mod_un, pch=19, col=unique(col_pars), bty='n')
  }
  
}
























































# function:
#     post_corr
# description:  
#     Specific plots for corelations and loading of SOLV 
# arguments:
#     c_list = data.frame generated with file_id() function
#     file_save = location to save file
#
post_corr = function(c_list, file_save){
  
  # # test
  # c_list = chains_list
  # file_save = file.path(getwd(), 'figures5')
  
  # parameters
  mt = unique(c_list$model)
  ss = unique(c_list$sample)
  dn = unique(c_list$data)
  
  # m=1
  # s=1
  # d=1
  for(m in 1:length(mt)){
    for(s in 1:length(ss)){
      for(d in 1:length(dn)){
        
        # load data
        idx_files = with(chains_list, model==mt[m] & sample==ss[s] &
                           data == dn[d])
        
        fit_files = chains_list[idx_files, 1]
        stan_model = rstan::read_stan_csv( file.path(chains_path, fit_files) )
        
        # extract and samples
        post = extract.samples( stan_model )
        idx = names(post) %in% c('Rho_theta_sub','loads')
        post = post[idx]
        post = as_tibble(data.frame(post))
        post = post[,-c(4,7:8,10:12)]
        names(post) = c(paste0('loads[',1:3,']'), 
                        'Rho_theta_sub[1,2]','Rho_theta_sub[1,3]','Rho_theta_sub[2,3]')
        # str(post)
        
        # plot
        figure_name = paste0(mt[m],'_J',ss[s],'_Ndata',dn[d],'_corrplot_LR.png')
        png( file.path(file_save, figure_name), 
             units='cm', width=35, height=25, res=100)
        pairs_panels(post, pch=19, 
                     points.col=col.alpha('blue',0.05),
                     hist.col=col.alpha('blue', 0.3), 
                     col=col.alpha('red', 0.8))
        dev.off()
        
        # advance
        print( paste0(mt[m],'_J', ss[s], '_Ndata', dn[d]) )
      }
    }
  }
}










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