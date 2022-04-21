#' @export
lp_norm<-function(x, p = 2)
{
  x_abs = abs(x)
  if( p >= Inf){
    norm_value = max(x_abs)
  }
  else{
    norm_value = sum(x_abs^p)^(1/p)
  }
  return(norm_value)

}
