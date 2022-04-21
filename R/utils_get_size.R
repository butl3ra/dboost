#' @export
get_size<-function(x,null_size = c(0,0))
{
  len_x = length(x)
  if(len_x ==0){
    sz = null_size
  }
  else if(is.vector(x)){
    sz = c(len_x,1)
  }
  else if(is.array(x) | is.data.frame(x)){
    sz = dim(x)
  }
  
  return(sz)
  
}

#' @export
get_ncon<-function(x)
{
  get_size(x)[1]
}