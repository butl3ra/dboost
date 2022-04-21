#' @export
array_to_mat<-function(a,
                       fn = 'vech')
{
  d = dim(a)
  nr = d[1]
  nc1 = d[2]
  nc2 = d[3]
  if(fn == 'vech'){
    nc = nc1*(nc1+1)/2
  }
  else{
    nc = nc1*nc2
  }

  # --- match function
  fn = match.fun(fn)


  mat = matrix(0,nr,nc)
  # --- main loop
  for(i in 1:nr){
    mat[i,] = fn(a[i,,])
  }
  return(mat)

}


#' @export
mat_to_array<-function(m)
{
  nr = nrow(m)
  nc = ncol(m)
  nc = (-1 + sqrt(1 + 4*2*nc))/2
  a = array(0,c(nr,nc,nc))

  # --- main loop
  for(i in 1:nr){
    a[i,,] = vech_inv(m[i,])
  }
  return(a)

}
