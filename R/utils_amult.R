#' @export
amult<-function(a,
                m)
{
  nr_a = nrow(a)
  nr_m = nrow(m)
  if(nr_a != nr_m){
    stop('nrow(a) must equal nrow(m) ')
  }
  nc_a = ncol(a)
  v = matrix(0,nr_a,nc_a)
  for(i in 1:nr_a){
    v[i,] = a[i,,]%*%m[i,]
  }
  return(v)
}


