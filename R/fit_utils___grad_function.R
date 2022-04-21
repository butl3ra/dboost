#' @export
grad_mse<-function(y,
                   y_hat,
                   ...)
{
  # --- note that this is the negative gradient: of ||y - y_hat||_2^2
  resid = get_residuals(y = y, y_hat = y_hat)
  return(resid)

}

#' @export
grad_spo<-function(y,
                   y_hat,
                   opt_oracle,
                   ...)
{
  # --- compute optimal z:
  z_hat = opt_oracle(y_hat)
  grads = opt_oracle$grad(dl_dz = y)

  return( -grads$dl_dc )

}

#' @export
grad_qspo<-function(y,
                    y_hat,
                    opt_oracle,
                    ...)
{
  # --- prep:
  P = get_P_eval(opt_oracle)
  is_array_P = is_array(P)
  # --- compute optimal z:
  z_hat = opt_oracle(y_hat)
  if(!is_array_P){
    dl_dz = z_hat%*%P + y
  }
  else{
    dl_dz =  amult(P,z_hat) + y
  }
  grads = opt_oracle$grad(dl_dz = dl_dz)

  return( -grads$dl_dc )

}
