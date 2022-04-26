#' @export
generate_problem_data<-function(n_x = 3,
                                n_z = 5,
                                n_obs = 1000,
                                pct_true = 0.5,
                                noise_multiplier_tau = 0,
                                polys = c(1,3),
                                intercept = T,
                                intercept_mean = 10,
                                x_min = -1,
                                x_max = 1)
{

  # --- generate coefficients
  theta_min = -1
  theta_max = 1
  theta_list =list()
  for(i in 1:length(polys)){
    theta_list[[i]] = generate_coef(n_y = n_z,
                                    n_x = n_x,
                                    pct_true = pct_true,
                                    method = 'runif',
                                    min = theta_min,
                                    max = theta_max)
  }



  # --- create x:
   x = mvrunif(n_vars = n_x,n_obs = n_obs,min = x_min,max = x_max)


  # --- true f(x)
  f_polys = Map(function(theta,p){
    generate_y(x = x^p, b = theta)
  },theta = theta_list, p =polys)
  f = Reduce("+",f_polys)

  # --- intercept:
  if(intercept){
    intercept_value = rnorm(n_z,intercept_mean)
    f = t(t(f)+intercept_value)
    theta_list = c(list(intercept_value),theta_list)
  }

  # ---- generate errors:
  errors = mvrnorm(n_obs = n_obs,mu = rep(intercept_mean,n_z),V =  diag(n_z))


  # --- generate y with error
  y = f + noise_multiplier_tau*errors


  # --- output:
  colnames(x) = paste0('x',1:n_x)
  out = list(x = x,
             cost = y,
             theta = theta_list)
  return(out)
}



#' @export
generate_popt_data<-function(n_x = 5,
                                   n_y = 50,
                                   n_obs = 100,
                                   n_factors = 4,
                                   pct_true = 0.5,
                                   poly_degree = 16,
                                   noise_multiplier_tau = 1,
                                   ...)
{

  # --- factor size and noise:
  factor_size = 0.0025*noise_multiplier_tau
  sigma_noise = 0.01*noise_multiplier_tau

  # --- factor matrix:
  one_vec = rep(1,n_y)
  Id = diag(one_vec)
  L_mat = matrix(runif(n_y*n_factors,-factor_size,factor_size),n_y,n_factors)
  #L_mat = 2*factor_size*matrix(runif(n_y*n_factors),n_y,n_factors) - factor_size*matrix(1,n_y,n_factors)
  Sigma = L_mat%*%t(L_mat) + sigma_noise^2*Id

  # --- avg_vec
  avg_vec = one_vec/n_y
  gamma = avg_vec%*%Sigma%*%avg_vec
  gamma = gamma*2.25

  # --- theta and x:
  x = generate_x(n_x = n_x,
                  n_obs = n_obs,
                  mu = rep(0,n_x),
                  V = diag(n_x))
  theta = generate_coef(n_y = n_y,
                        n_x = n_x,
                        pct_true = pct_true,
                        method = 'runif',
                        min = 1,
                        max = 1)

  # --- generate y:
  y = generate_y(x=x,b=theta)
  alpha_factor = 0.05/sqrt(n_x)
  inner_constant = 0.1^(1/poly_degree)
  cost = (alpha_factor*y + inner_constant)^poly_degree

  # --- add L_mat and sigma noise:
  rand_factor = matrix(rnorm(n_obs*n_factors),n_obs,n_factors)
  rand_factor = rand_factor%*%t(L_mat)
  rand_noise = sigma_noise*matrix(rnorm(n_obs*n_y),n_obs,n_y)

  cost = cost + rand_factor + rand_noise

  # --- output:
  colnames(x) = paste0('x',1:n_x)
  out = list(x = x,
             cost = cost,
             theta = theta)
  return(out)
}


#' @export
generate_network_data<-function(n_x = 5,
                                n_z = 50,
                                n_obs = 100,
                                pct_true = 0.5,
                                poly_degree = 3,
                                noise_multiplier_tau = 1,
                             ...)
{
  # --- create x:
  x = mvrunif(n_vars = n_x,n_obs = n_obs,min = 0,max = 1)
  theta = generate_coef(n_y = n_z,
                        n_x = n_x,
                        pct_true = pct_true,
                        method = 'runif',
                        min = 1,
                        max = 1)

  # --- generate y:
  y = (1/sqrt(n_x))*generate_y(x=x,b=theta)
  cost = (y + 1)^poly_degree

  # --- add sigma noise:
  sigma_noise = noise_multiplier_tau
  rand_noise = mvrunif(n_vars = n_z,n_obs = n_obs,min = 1-sigma_noise,max = 1 + sigma_noise)

  cost = cost*rand_noise

  # --- output:
  colnames(x) = paste0('x',1:n_x)
  out = list(x = x,
             cost = cost,
             theta = theta)
  return(out)
}

#' @export
generate_network_data_dep<-function(n_x = 5,
                                n_y = 50,
                                n_obs = 100,
                                pct_true = 0.5,
                                poly_degree = 16,
                                eps = 0.25,#
                                ...)
{
  # --- theta and x:
  x = generate_x(n_x = n_x,
                 n_obs = n_obs,
                 mu = rep(0,n_x),
                 V = diag(n_x))
  theta = generate_coef(n_y = n_y,
                        n_x = n_x,
                        pct_true = pct_true,
                        method = 'runif',
                        min = 1,
                        max = 1)

  # --- generate y:
  y = generate_y(x=x,b=theta)
  alpha_factor = 1/sqrt(n_x)
  y = alpha_factor*y + 3
  cost = (y^poly_degree + 1)

  # --- noise factor
  noise = runif(n_y*n_obs,1-eps,1+eps)
  noise = matrix(noise,n_obs,n_y)


  cost = cost*noise


  # --- output:
  out = list(x = x,
             cost = cost,
             theta = theta)
  return(out)
}



#' @export
generate_coef_list<-function(n_y = 10,
                             n_x = 3,
                             pct_true = 0.5,
                             method = c('rnorm','constant','runif'),
                             polys = c(1),
                             ...)
{
  out = lapply(polys,
               generate_coef,
               n_y = n_y,
               n_x = n_x,
               pct_true = pct_true,
               method = method,
               ...)
  return(out)
}

#' @export
generate_coef<-function(n_y = 5,
                        n_x = 3,
                        pct_true = 0.5,
                        method = c('rnorm','constant','runif'),
                        ...)
{
  # --- prep:
  n_total_x = n_y * n_x
  # --- sparsity matrix:
  smat = rbinom(n_total_x,size = 1,prob = pct_true)
  smat = matrix(smat,nrow = n_x,ncol = n_y)

  # --- generate random coefficients
  fn = match.fun(method[1])
  b = fn(n_total_x, ...)
  b = matrix(b,nrow = n_x,ncol = n_y)

  # --- apply sparsity:
  b = smat*b
  return(b)
}

#' @export
constant<-function(n,
                   value = 1)
{
  rep(value,n)
}

#' @export
mvrnorm<-function(n_obs,
                  mu,
                  V)
{
  p <- length(mu)
  d_V = dim(V)
  if(d_V[1]!=p | d_V[2]!=p){
    stop('length of mu does not match dim of V')
  }

  D <- chol(V)
  z = matrix(rnorm(n_obs * p), ncol = p)
  out = z %*% D + rep(mu, rep(n_obs, p))
  colnames(out) = colnames(V)
  return(out)
}

#' @export
mvrunif<-function(n_vars,
                  n_obs,
                  min = 0,
                  max = 1)
{
  if(length(min)==1){
    min = rep(min,n_vars)
  }
  if(length(max)==1){
    max = rep(max,n_vars)
  }
  mat = matrix(NA,n_obs,n_vars)
  for(i in 1:n_vars){
    mat[,i] = runif(n_obs,min = min[i],max = max[i])
  }
  return(mat)
}

#' @export
generate_cor<-function(n_vars,
                       rho = 0,
                       v = 1:n_vars)
{
  if(rho==0){
    mat = diag(n_vars)
  }
  else{
    mat = dist(v,method = 'manhattan',upper=T,diag=T)
    mat = as.matrix(mat)
    mat = rho^mat
  }
  colnames(mat) = rownames(mat) = 1:n_vars
  return(mat)
}


#' @export
generate_sd<-function(n_vars,
                      min = 0.10,
                      max = 0.25,
                      scale_factor = sqrt(252),
                      ...)
{
  std_devs = runif(n_vars,min = min,max = max)
  return(std_devs/scale_factor)
}

#' @export
generate_cov<-function(std_devs,
                       rho_mat
)
{
  std_devs = diag(std_devs)
  covar = std_devs%*%rho_mat%*%std_devs
  colnames(covar) = rownames(covar) = colnames(rho_mat)
  return(covar)
}

#' @export
generate_x<-function(n_x,
                     n_obs,
                     mu = rep(0,n_x),
                     V = diag(n_x)
)
{
  mvrnorm(n_obs = n_obs,mu = mu,V = V)
}

#' @export
generate_y<-function(x,
                     b)
{
  y = x%*%b
  return(y)
}
