#' @export
y_hat_mean<-function(y_true,
                     y_false,
                     ...)
{
  y_hat_true = colMeans2(y_true,na.rm=T)
  y_hat_false = colMeans2(y_false,na.rm=T)
  out = list(y_hat_true = y_hat_true,
             y_hat_false = y_hat_false)

  return(out)
}

#' @export
y_hat_weight<-function(y_true,
                       y_false,
                       ...)
{
  y_hat_true = colMeans2(y_true,na.rm=T)
  y_hat_false = colMeans2(y_false,na.rm=T)

  if(y_hat_true > y_hat_false ){
    y_hat_false = 0
    if(y_hat_true < 0){
      y_hat_true = 0
    }
  }
  else{
    y_hat_true = 0
    if(y_hat_false < 0){
      y_hat_false = 0
    }
  }

  out = list(y_hat_true = y_hat_true,
             y_hat_false = y_hat_false)

  return(out)


}

#' @export
y_hat_abs_region<-function(y_true,
                          y_false,
                          ...)
{
  y_hat_true = colMeans2(y_true,na.rm=T)
  y_hat_true_mag = abs(y_hat_true)
  y_hat_false = colMeans2(y_false,na.rm=T)
  y_hat_false_mag = abs(y_hat_false)

  # --- only choose the region with the highest abs of y_hat
  if(y_hat_true_mag > y_hat_false_mag ){
    y_hat_false = 0
  }
  else{
    y_hat_true = 0
  }

  out = list(y_hat_true = y_hat_true,
             y_hat_false = y_hat_false)

  return(out)


}

#' @export
y_hat_tstat_region<-function(y_true,
                             y_false,
                             ...)
{
  n_true = nrow(y_true)
  y_hat_true = colMeans2(y_true,na.rm=T)
  y_hat_true_mag = abs(y_hat_true)*sqrt(n_true)

  n_false = nrow(y_false)
  y_hat_false = colMeans2(y_false,na.rm=T)
  y_hat_false_mag = abs(y_hat_false)*sqrt(n_false)

  # --- only choose the region with the highest tstat of y_hat
  if(y_hat_true_mag > y_hat_false_mag ){
    y_hat_false = 0
  }
  else{
    y_hat_true = 0
  }

  out = list(y_hat_true = y_hat_true,
             y_hat_false = y_hat_false)

  return(out)


}

#' @export
y_hat_majority_vote<-function(y_true,
                              y_false,
                              ...)
{
  classes = c(0,1)
  counts_true = sapply(classes,function(value) colCounts(x=y_true,value = value))
  counts_false = sapply(classes,function(value) colCounts(x=y_false,value = value))

  true_positive_true = counts_true[2]/sum(counts_true)
  true_positive_false = counts_false[2]/sum(counts_false)

  y_hat_true = true_positive_true
  y_hat_true[y_hat_true > 0.5] = 1
  y_hat_true[y_hat_true <= 0.5] = 0

  y_hat_false = true_positive_false
  y_hat_false[y_hat_false > 0.5] = 1
  y_hat_false[y_hat_false <= 0.5] = 0

  out = list(y_hat_true = y_hat_true,
             y_hat_false = y_hat_false)

  return(out)
}

#' @export
y_hat_true_positive<-function(y_true,
                              y_false,
                              ...)
{
  classes = c(0,1)
  counts_true = sapply(classes,function(value) colCounts(x=y_true,value = value))
  counts_false = sapply(classes,function(value) colCounts(x=y_false,value = value))

  true_positive_true = counts_true[2]/sum(counts_true)
  true_positive_false = counts_false[2]/sum(counts_false)

  if(true_positive_true > true_positive_false ){
    y_hat_true = 1
    y_hat_false = 0
  }
  else{
    y_hat_true = 0
    y_hat_false = 1
  }

  out = list(y_hat_true = y_hat_true,
             y_hat_false = y_hat_false)

  return(out)

}

#' @export
y_hat_mse_mean<-function(y_true,
                         y_false,
                         data,
                         ...)
{
  # --- get f_x
  f_x_true = data$data_true$f_x
  f_x_false = data$data_false$f_x

  # --- get y res
  y_res_true = y_true - f_x_true
  y_res_false = y_false - f_x_false

  # --- y_false:


  # --- z_star
  z_star_true = data$data_true$z_star
  z_star_false = data$data_false$z_star
  cost_star_true = rowSums(y_true*z_star_true)
  cost_star_false = rowSums(y_false*z_star_false)

  # --- compute optimal z
  z_true = opt_oracle(y_hat_true)
  z_false = opt_oracle(y_hat_false)

  cost_true = y_true%*%t(z_true)
  cost_false = y_false%*%t(z_false)

  # --- SPO loss
  value_true = sum(cost_true - cost_star_true)
  value_false = sum(cost_false - cost_star_false)

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
