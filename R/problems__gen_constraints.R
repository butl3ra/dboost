#generate_socp
#generate_qp
#generate_lp
#generate_box_constraint
#generate_spd_mat
#generate_cone_constraint




#' @export
generate_lp<-function(n_vars,
                      min_lb = -3,
                      max_lb = 0,
                      min_ub = 0,
                      max_ub = 3,
                      ...)
{


  # --- generate eq constraints:
  lp_eq = generate_eq_constraint(n_vars = n_vars,
                                 n_con = 1,
                                 value = 0)

  # --- generate box constraints:
  lp_ineq = generate_box_constraint(n_vars,
                                   min_lb = min_lb,
                                   max_lb = max_lb,
                                   min_ub = min_ub,
                                   max_ub = max_ub )



  # --- lp cone:
  cone_lp = lp_cone_prep(A = lp_eq$A,
                         b = lp_eq$b,
                         G = lp_ineq$A,
                         h = lp_ineq$b,
                         cone = list(z = 0, l = 0, q = NULL))

  return(cone_lp)
}

#' @export
generate_qp<-function(n_vars,
                      n_obs=n_vars,
                      eps = 10^-3,
                      method = c('rnorm','constant','runif'),
                      min_lb = -3,
                      max_lb = 0,
                      min_ub = 0,
                      max_ub = 3,
                      ...)
{
  # --- generate box constraints:
  lp_con = generate_box_constraint(n_vars,
                                   min_lb = min_lb,
                                   max_lb = max_lb,
                                   min_ub = min_ub,
                                   max_ub = max_ub )

  # --- lp cone:
  cone_lp = lp_cone_prep(A = NULL,
                         b = NULL,
                         G = lp_con$A,
                         h = lp_con$b,
                         cone = list(z = 0, l = 0, q = NULL))

  # --- generate QP spd mat:
  P = generate_spd_mat(n_vars = n_vars,
                       n_obs = n_obs,
                       eps = eps,
                       method = method,
                       ...)

  cone_lp$P = P

  return(cone_lp)
}

#' @export
generate_socp<-function(n_vars,
                        n_obs=n_vars,
                        factor = 2,
                        eps = 10^-3,
                        method = c('rnorm','constant','runif'),
                        min_lb = -3,
                        max_lb = 0,
                        min_ub = 0,
                        max_ub = 3,
                        ...)
{
  # --- generate box constraints:
  lp_con = generate_box_constraint(n_vars,
                                   min_lb = min_lb,
                                   max_lb = max_lb,
                                   min_ub = min_ub,
                                   max_ub = max_ub )

  # --- generate quadratic cone:
  socp_con = generate_cone_constraint(n_vars,
                                     n_obs = n_obs,
                                     factor = factor,
                                     eps = eps,
                                     method = method,
                                     ...)

  # --- socp cone:
  cone_socp = mvt_cone_prep(A = NULL,
                            b = NULL,
                            G = lp_con$A,
                            h = lp_con$b,
                            Q = socp_con$Q,
                            vol_target = socp_con$tau,
                            scale_factor = 1,
                            rescale_factor = 100,
                            cone = list(z = 0, l = 0, q = NULL))

  # --- Q and tau
  cone_socp$Q = socp_con$Q
  cone_socp$tau = socp_con$tau


  return(cone_socp)
}

#' @export
generate_eq_constraint<-function(n_vars,
                                 n_con = 1,
                                 value = 0)
{
  #A = runif(n_vars*n_con)
  n = n_vars*n_con
  A = matrix(0,n_con,n_vars)
  run = T
  iter = 0
  while(run){
    iter = iter+1
    A = rbinom(n,size=1,prob=0.5)
    A = matrix(A,n_con,n_vars)
    A = A[!duplicated(A),,drop=F]

    # --- no empty rows
    check_1 = any(rowSums(A) == 0)

    # --- n_con unique rows:
    nr_A = nrow(A)
    check_2 = nr_A < n_con

    # --- non empty nullspace:
    null_A = nullspace(A)
    check_3 = is.null(null_A)

    if(check_3 & iter >10) {
      n_con = n_con - 1
    }

    run = check_1 | check_2 | check_3

  }

  #if(length(value) < n_con){
  #  value =rep(value,n_con)
  #}
  #b = value
  z_valid = runif(n_vars)
  b = A%*%z_valid
  out = list(A = A,
             b = b)
  return(out)
}

#' @export
generate_box_constraint<-function(n_vars,
                                  min_lb = -3,
                                  max_lb = 0,
                                  min_ub = 0,
                                  max_ub = 3
                                  )
{
  lb = generate_box_lb(n_vars,min = min_lb, max = max_lb)
  ub = generate_box_ub(n_vars,min = min_ub, max = max_ub)

  A = rbind(lb$A,ub$A)
  b = rbind(lb$b,ub$b)



  return(list(A = A, b = b ))


}
#' @export
generate_box_ub<-function(n_vars,
                          min = 0,
                          max = 1)
{
  A = diag(n_vars)
  b = matrix(runif(n_vars,min = min,max = max))
  return(list(A = A, b = b ))

}

#' @export
generate_box_lb<-function(n_vars,
                          min = -1,
                          max = 0)
{
  A = -diag(n_vars)
  b = -matrix(runif(n_vars,min = min,max = max))
  return(list(A = A, b = b ))
}

#' @export
generate_cone_constraint<-function(n_vars,
                                   n_obs,
                                   factor = 2,
                                   eps = 10^-3,
                                   method = c('rnorm','constant','runif'),
                                   ...
                                   )
{
  # --- generate mat
  mat = generate_spd_mat(n_vars = n_vars,
                         n_obs = n_obs,
                         eps = eps,
                         method = method,
                         ...)
  w = rep(1/n_vars,n_vars)
  tau = as.numeric(w%*%mat%*%w)*factor

  out = list(Q = mat,
             tau = tau)

  return(out)


}

#' @export
generate_spd_mat<-function(n_vars,
                           n_obs,
                           eps = 10^-3,
                           method = c('rnorm','constant','runif'),
                           ...)
{
  # --- generate random coefficients
  n_total = n_vars*n_obs
  fn = match.fun(method[1])
  mat = fn(n_total, ...)
  mat = matrix(mat,nrow = n_obs,ncol = n_vars)

  # --- crossprod and spd:
  mat = t(mat)%*%mat + eps*diag(n_vars)
  mat = mat/n_obs
  return(mat)
}
