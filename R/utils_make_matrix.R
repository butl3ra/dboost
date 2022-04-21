#' @export
make_matrix<-function(x)
{
  nr=nrow(x)
  cls = class(x)
  is_list = any(cls == 'list')
  if(is.null(nr) & !is_list){
    y=matrix(x,length(x))
  }
  else if(is.data.frame(x) | is_xts(x)){
    y = as.matrix(x)
  }
  else if(is_list){
    y = do.call(rbind,x)
  }
  else if(!is.matrix(x)){
    d=dim(x)
    if(length(d) == 2){
      nc=ncol(x)
      #y=matrix(x,nr,nc)
      y=matrix(as.matrix(x),nr,nc,dimnames=dimnames(x))
    }
    else{
      y = x
    }
  }
  else{
    y=x
  }
  return(y)
}

#' @export
is_xts<-function (x)
{
  inherits(x, "xts")
}
