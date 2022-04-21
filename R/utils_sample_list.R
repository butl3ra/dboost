#' @export
sample_list<-function(x,
                      idx,
                      drop = FALSE)
{
  if(is.list(x)){
    x = lapply(x,sample_list,idx = idx,drop = drop)
  }
  else{
    x = x[idx,,drop = drop]
  }
  return(x)
}
