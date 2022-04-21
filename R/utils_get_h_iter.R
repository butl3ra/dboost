#' @export
get_h_iter<-function(x,
                     h_dim)
{
  nc_x = ncol(x)
  if(is.character(h_dim)){
    h_dim = nc_x
  }
  h_iter = lapply(h_dim,combn,x=nc_x,simplify = F)
  h_iter = unlist(h_iter,recursive = F)
  return(h_iter)
}
