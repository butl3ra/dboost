#' @export
reduce_objective<-function(value,
                           reduce = NULL)
{
  if(missing(reduce)){
    reduce = NULL
  }
  if(!is.null(reduce) & length(value) > 1){
    reduce = tolower(reduce)
    if(reduce == 'mean'){
      value = mean(value)
    }
    else if(reduce == 'sum'){
      value = sum(value)
    }
    else if(reduce == 'max'){
      value = max(value)
    }
    else if(reduce == 'min'){
      value = min(value)
    }
    else if(reduce == 'identity'){
      value = identity(value)
    }
  }
  return(value)
}


#' @export
objective_mse<-function(y,
                        y_hat,
                        reduce = 'sum',
                        ...)
{
  resid = get_residuals(y = y, y_hat = y_hat)
  resid_sq = resid^2
  resid_norm = colMeans2(resid_sq,na.rm=T)
  value = reduce_objective(resid_norm,reduce = reduce)
  return(value)

}

#' @export
get_residuals<-function(y,
                        y_hat)
{
  dim_y_hat = get_size(y_hat)
  dim_y = get_size(y)
  is_vector_y_hat = is_vector(y_hat)
  if(dim_y[2] == 1){
    resid = y[,1] - y_hat
  }
  else if(is_vector_y_hat & dim_y[2] == dim_y_hat[1]){
    resid = t( t(y) - y_hat)
  }
  else if(dim_y[2]!= dim_y_hat[2]){
    stop('dimensions of y and y_hat are incompatible')
  }
  else{
    resid = y - y_hat
  }
  resid = make_matrix(resid)
  return(resid)
}


#' @export
objective_spo<-function(y,
                        y_hat,
                        opt_oracle,
                        reduce = 'sum',
                        ...)
{
  # --- compute optimal z:
  z_hat = opt_oracle(y_hat)

  # --- SPO loss:
  value = compute_cost(z = z_hat, cost = y)
  value = reduce_objective(value,reduce = reduce)
  return(value)
}

#' @export
objective_qspo<-function(y,
                         y_hat,
                         opt_oracle,
                         reduce = 'sum',
                        ...)
{
  # --- compute optimal z:
  z_hat = opt_oracle(y_hat)

  # --- SPO loss:
  P = get_P_eval(opt_oracle)
  value = compute_qcost(z = z_hat, cost = y, P = P)
  value = reduce_objective(value,reduce = reduce)
  return(value)
}

