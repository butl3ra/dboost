#' @export
roll_cov<-function(x,
                   weights = window_ema(0.94),
                   eps = 10^-4,
                   min_obs = 2,
                   do_fill = TRUE,
                   ...)
{
  n_obs = nrow(x)
  n_x = ncol(x)
  width = length(weights)
  if(is.null(min_obs)){
    min_obs = min(n_x + 2, width)
  }
  cov_mat = roll::roll_cov(data = x, width, weights = weights,min_obs = min_obs,...)
  cov_mat = aperm(cov_mat,c(3,1,2))

  # --- Fill:
  if(do_fill){
    idx = 1:(min_obs-1)
    cov_mat[idx,,] = cov_mat[min_obs,,]
  }

  # --- do regularization
  if(eps > 0){
    I = diag(n_x)
    for(i in 1:n_obs){
      cov_mat[i,,] = cov_mat[i,,] + eps*I
    }
  }

  return(cov_mat)

}

#' @export
window_ema<-function(l,
                     threshold = 0.95)
{
    n = (seq(max(10/(1-l),250)-1,0,-1))
    w = (1-l)*l^(n)
    w = w/sum(w)
    cs = cumsum(rev(w))
    lookback = min(which(cs > threshold))
    out = tail(w,lookback)
    out = out/sum(out)
  return(out)
}
