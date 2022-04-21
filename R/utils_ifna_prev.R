#' @export
ifna_prev<-function(x)
{
  x1 = !is.na(x)
  x1[1]=T
  return( x[cummax( (1:length(x)) * x1 )]	)
}

#' @export
ifna_prev_mat<-function(x)
{
  x[] = apply(x,2,ifna_prev)
  return(x)
}
