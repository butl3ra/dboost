#' @export
objective_dtree_weighted_avg<-function(y_true,
                                       y_false,
                                       value_true,
                                       value_false)
{
  n_true = nrow(y_true)
  n_false = nrow(y_false)
  n_total = n_true + n_false

  value = (n_true/n_total)*value_true + (n_false/n_total)*value_false
  return(value)
}



#' @export
objective_dtree_mse<-function(y_true,
                              y_hat_true,
                              y_false,
                              y_hat_false,
                              reduce = NULL,
                              weighted_avg = FALSE,
                              ...)
{
  # -- core objective
  resid_true = get_residuals(y = y_true, y_hat = y_hat_true)
  resid_false = get_residuals(y = y_false, y_hat = y_hat_false)

  value_true = sum(resid_true^2)
  value_false = sum(resid_false^2)


  # --- weighted average:
  if(weighted_avg){
    value = objective_dtree_weighted_avg(y_true = y_true,
                                         y_false = y_false,
                                         value_true = value_true,
                                         value_false = value_false)
  }
  else{
    value = value_true + value_false
  }
  value = reduce_objective(value,reduce = reduce)
  # --- note: we would want to MINIMIZE this:
  return(value)
}


#' @export
objective_dtree_pve<-function(y_true,
                              y_hat_true,
                              y_false,
                              y_hat_false,
                              y,
                              y_hat,
                              reduce = NULL,
                              ...)
{
  # --- core objective:
  resid_true = get_residuals(y = y_true, y_hat = y_hat_true)
  resid_false = get_residuals(y = y_false, y_hat = y_hat_false)
  resid_total = get_residuals(y = y, y_hat = y_hat)

  ss_true = sum(resid_true^2)
  ss_false = sum(resid_false^2)
  ss_total = sum(resid_total^2)

  # --- note: we would want to MAXIMIZE this:
  value = ss_total - (ss_true + ss_false)

  return(value)

}



#' @export
objective_dtree_spo<-function(y_true,
                               y_hat_true,
                               y_false,
                               y_hat_false,
                               opt_oracle,
                               reduce = NULL,
                               weighted_avg = FALSE,
                               ...)
{

  # --- z_star
  #z_star_true = data$data_true$z_star
  #z_star_false = data$data_false$z_star
  #cost_star_true = rowSums(y_true*z_star_true)
  #cost_star_false = rowSums(y_false*z_star_false)

  # --- compute optimal z
  z_true = opt_oracle(y_hat_true)
  z_false = opt_oracle(y_hat_false)

  cost_true = y_true%*%t(z_true)
  cost_false = y_false%*%t(z_false)

  # --- SPO loss
  value_true = sum(cost_true)#- cost_star_true
  value_false = sum(cost_false)#- cost_star_false

  # --- weighted average:
  if(weighted_avg){
    value = objective_dtree_weighted_avg(y_true = y_true,
                                         y_false = y_false,
                                         value_true = value_true,
                                         value_false = value_false)
  }
  else{
    value = value_true + value_false
  }
  value = reduce_objective(value,reduce = reduce)
  # --- note: we would want to MINIMIZE this:
  return(value)

}


#' @export
objective_dtree_qpo<-function(y_true,
                              y_hat_true,
                              y_false,
                              y_hat_false,
                              opt_oracle,
                              reduce = NULL,
                              weighted_avg = FALSE,
                               ...)
{

  # --- prep:
  n_true = length(y_true)
  n_false = length(y_false)


  # --- convert to P:
  P_true = mat_to_array(y_true)
  P_false = mat_to_array(y_false)

  # --- convert to P:
  P_hat_true = vech_inv(y_hat_true)
  P_hat_false = vech_inv(y_hat_false)

  # --- create zero costs:
  n_z = ncol(P_hat_true)
  cost_zero = rep(0,n_z)


  # --- compute optimal z
  z_true = opt_oracle(cost = cost_zero,P = P_hat_true)
  z_false = opt_oracle(cost = cost_zero,P = P_hat_false)

  # --- quadratic componenet
  q_true = quad_form(z_true,P_true)
  q_false = quad_form(z_false,P_false)

  # --- SPO loss
  value_true = sum(q_true)
  value_false = sum(q_false)

  # --- weighted average:
  if(weighted_avg){
    value = objective_dtree_weighted_avg(y_true = y_true,
                                         y_false = y_false,
                                         value_true = value_true,
                                         value_false = value_false)
  }
  else{
    value = value_true + value_false
  }
  value = reduce_objective(value,reduce = reduce)
  # --- note: we would want to MINIMIZE this:
  return(value)

}

#' @export
objective_dtree_qspo<-function(y_true,
                              y_hat_true,
                              y_false,
                              y_hat_false,
                              opt_oracle,
                              reduce = NULL,
                              weighted_avg = FALSE,
                              ...)
{

  # --- prep:
  n_true = length(y_true)
  n_false = length(y_false)

  # --- compute optimal z
  z_true = opt_oracle(y_hat_true)
  z_true_t = t(z_true)
  z_false = opt_oracle(y_hat_false)
  z_false_t = t(z_false)

  # --- linear componenet
  cost_true = y_true%*%z_true_t
  cost_false = y_false%*%z_false_t

  # --- quadratic componenet
  P = get_P_eval(opt_oracle)
  is_array_P = is_array(P)
  if(!is_array_P){
    q_true = z_true%*%P%*%z_true_t
    q_false = z_false%*%P%*%z_false_t
  }
  else{
    q_true = mean(quad_form(z_true,P))
    q_false = mean(quad_form(z_false,P))
  }

  # --- SPO loss
  value_true = sum(cost_true) + 0.5*n_true * q_true
  value_false = sum(cost_false) + 0.5*n_false * q_false

  # --- weighted average:
  if(weighted_avg){
    value = objective_dtree_weighted_avg(y_true = y_true,
                                         y_false = y_false,
                                         value_true = value_true,
                                         value_false = value_false)
  }
  else{
    value = value_true + value_false
  }
  value = reduce_objective(value,reduce = reduce)
  # --- note: we would want to MINIMIZE this:
  return(value)

}


objective_dtree_dspo_dep<-function(y_true,
                              y_hat_true,
                              y_false,
                              y_hat_false,
                              opt_oracle,
                              data,
                              reduce = NULL,
                              weighted_avg = FALSE,
                              lambda = 0,
                              ...)
{


  # --- compute optimal z
  z_true = opt_oracle(y_hat_true)
  z_true = matrix(z_true,nrow(z_x_true),ncol(z_true),byrow=T)
  z_false = opt_oracle(y_hat_false)
  z_false = matrix(z_false,nrow(z_x_false),ncol(z_false),byrow=T)





  # --- z_x:
  z_x_true = data$data_true$z_x
  z_x_false = data$data_false$z_x

  # --- z_x:
  y_full_true = data$data_true$y_full
  y_full_false = data$data_false$y_full

  # --- compute optimal z
  z_true = opt_oracle(y_hat_true)
  z_true = matrix(z_true,nrow(z_x_true),ncol(z_true),byrow=T)
  z_false = opt_oracle(y_hat_false)
  z_false = matrix(z_false,nrow(z_x_false),ncol(z_false),byrow=T)

  # --- combo
  z_x = rbind(z_x_true,z_x_false)
  z_hat = rbind(z_true,z_false)
  y = rbind(y_true,y_false)
  y_full = rbind(y_full_true,y_full_false)

  # --- interval:

  if(sum(abs(z_x)) == 0 ){
    value = objective_dtree_dspo_cost(y = y_full, z_x = z_hat)
  }
  else{
    interval = c(0,1)
    # --- find optimal weighting
    sol = optimize(fit_dboost_w_star,
                   interval = interval,
                   z_x = z_x,
                   z_hat = z_hat,
                   y = y_full,
                   obj_fn = objective_dtree_dspo_cost,
                   sum_1 = TRUE,
                   maximum = FALSE,
                   lambda = lambda)
    value = sol$objective
  }





  return(value)

}

#' @export
objective_dtree_dspo<-function(y_true,
                               y_hat_true,
                               y_false,
                               y_hat_false,
                               opt_oracle,
                               z_costs,
                               data,
                               reduce = NULL,
                               weighted_avg = FALSE,
                               lambda = 0,
                                   ...)
{
  # --- prep:
  n_true = nrow(y_true)
  n_false = nrow(y_false)
  n_z = ncol(y_true)
  # --- unpack: y_full:
  y_full_true = data$data_true$y_full
  y_full_false = data$data_false$y_full
  y_full = rbind(y_full_true,y_full_false)

  # --- compute optimal z
  z_true = opt_oracle(y_hat_true)
  z_true = matrix(z_true,n_true,n_z,byrow=T)
  z_false = opt_oracle(y_hat_false)
  z_false = matrix(z_false,n_false,n_z,byrow=T)
  z_hat = rbind(z_true,z_false)


  z_cost = objective_dtree_dspo_cost(z_hat,y_full)

  # -- append
  z_costs = c(z_cost,z_costs)

  # --- compute regularized loss:
  sol = get_dboost_w_star(z_costs = z_costs,lambda = lambda)

  value = sol$value



  return(value)

}



#' @export
objective_dtree_dspo_cost<-function(y,
                                    z_x,
                                    ...)
{
  mean(rowSums(z_x*y))
}
