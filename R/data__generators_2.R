
#' @export
generate_artificial_data<-function(n_z = 10,
                                       n_obs = 1000,
                                       n_x = 1,
                                       n_split = 3,
                                       pct_true = 0.5,
                                       snr = 0.30,
                                       rho = 0.25,
                                       min_sd = 0.3,
                                       max_sd = 0.5)
{

  # --- prep:
  if(n_x == 1){
    pct_true = 1
  }
  # --- generate std_dev:
  std_dev = generate_sd(n_vars = n_z,
                         min = min_sd,
                         max = max_sd,
                         scale_factor=1)

  # --- generate correlation matrix:
  cor_mat = generate_cor(n_vars = n_z,rho = rho)

  # --- generate covariance matrix:
  cov_mat = diag(std_dev)%*%cor_mat%*%diag(std_dev)

  # --- create x:
  x = mvrunif(n_vars = n_x,
              n_obs = n_obs,
              min = 0,
              max = 1)

  true_vec = rbinom(n_x,size = 1,prob = pct_true)

  # --- generate costs
  cost = matrix(0,n_obs,n_z)
  cost_list = list()
  for(i in 1:n_x){
    cost1 = generate_costs(x[,i],
                           is_true = true_vec[[i]],
                           n_split = n_split,
                           std_dev = std_dev,
                           cov_mat = cov_mat,
                           snr = snr,
                           n_obs = n_obs)
    cost_list[[i]] = cost1
    cost = cost + cost1$cost
  }
  cost = cost/n_x

  # --- output:
  out = list(x = x,
             cost = cost,
             cov_mat = cov_mat,
             true_vec = true_vec,
             cost_list = cost_list)
  return(out)
}


#' @export
generate_mu<-function(std_dev,
                      snr)
{
  n_z = length(std_dev)
  random_sign = sign(rnorm(n_z))
  mu_abs = snr*std_dev^2
  mu =  runif(n_z,0,2*mu_abs)*random_sign
  return(mu)
}

#' @export
generate_costs<-function(x1,
                         is_true,
                         n_split,
                         std_dev,
                         cov_mat,
                         snr,
                         n_obs)
{
  # --- prep:
  n_dist = n_split+1
  n_z = length(std_dev)
  mus = matrix(0,n_dist,n_z)
  cost = matrix(NA,n_obs,n_z)
  splits = NULL
  if(!is_true){
    cost = mvrnorm(n_obs,mus[1,],cov_mat)
  }
  else{
    # --- generate splits:
    splits = runif(n_split,min(x1),max(x1))
    splits = splits[order(splits)]

    # --- generate mean vectors and conditional costs:
    for(i in 1:n_dist){
      mus[i,] =  generate_mu(std_dev = std_dev, snr = snr)
      if(i == 1){
        idx = x1 <= splits[i]
      }
      else if(i == n_dist){
        idx = x1 >= splits[i-1]
      }
      else{
        idx = x1 > splits[i-1] & x1 <= splits[i]
      }
      len_idx = length(idx[idx])
      cost[idx,] = mvrnorm(len_idx,mus[i,],cov_mat)
    }
  }

  # --- list
  out = list(cost = cost,
             splits = splits,
             mus = mus)


  return(out)



}

#' @export
generate_artificial_data_ols<-function(n_y = 5,
                                   n_obs = 1000,
                                   n_x = 3,
                                   pct_true = 0.5,
                                   snr = 0.05,
                                   rho = 0.70,
                                   min_sd = 1,
                                   max_sd = 1,
                                   polys = c(1,3),
                                   scale_factor = 1)
{
  # --- generate std_dev:
  std_devs = generate_sd(n_vars = n_y,
                         min = min_sd,
                         max = max_sd,
                         scale_factor = scale_factor)


  # --- generate coefficients
  theta_min = -2*snr*std_devs^2
  theta_max = 2*snr*std_devs^2
  theta = generate_coef(n_y = n_y,
                        n_x = n_x,
                        pct_true = pct_true,
                        method = 'runif',
                        min = theta_min,
                        max = theta_max)


  # --- create x:
  #x = generate_x(n_x = n_x,
  #               n_obs = n_obs,
  #               V = diag(n_x) )
  x = mvrunif(n_vars = n_x,
              n_obs = n_obs,
              min = -1,
              max = 1)

  #x = x/4# --- std-dev of 0.25
  f = generate_y(x = x, b = theta)
  f_polys = lapply(polys,function(i) f^i)
  f = Reduce("+",f_polys)


  # --- create polynomials of x:
  #x_list = lapply(polys,function(i) x^i)
  #f = Map(generate_y,x = x_list, b = theta)
  #f = Reduce("+",f)

  # ---- generate errors:
  rho_mat = generate_cor(n_vars = n_y,rho = rho)
  cov_mat = diag(std_devs)%*%rho_mat%*%diag(std_devs)
  var_f = matrixStats::colVars(f,na.rm=T)
  std_dev_errors =  sqrt(var_f/snr)
  std_dev_errors[std_dev_errors == 0] = mean(std_dev_errors)
  V_errors = generate_cov(std_dev_errors,rho_mat)
  errors = generate_x(n_x = n_y,
                      n_obs = n_obs,
                      V = V_errors)

  # --- Tests:
  if(F){
    test = f+errors
    cor(test)
    diag(cov(f))/diag(cov(errors))
    lm(test[,1]~x[,1:n_x]+0)
  }

  # --- generate y with error
  y = f + errors


  # --- output:
  out = list(x = x,
             cost = y,
             cov_mat = cov_mat,
             theta = theta)
  return(out)
}


#' @export
generate_P<-function(n_vars,
                     n_x)
{
  m = n_vars*n_x
  P = matrix(0,n_vars,m)
  for(i in 1:n_vars){
    idx = (i*n_x - n_x+1):(i*n_x)
    P[i,idx] =1
  }
  return(P)
}

#' @export
generate_noise<-function(y,
                         snr)
{
  v = var(y)
  sigma = sqrt(v/snr)
  noise = rnorm(length(y),0,sigma)
  return(noise)
}


