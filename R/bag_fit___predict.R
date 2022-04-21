#' @export
predict.bag_fit<-function(object,
                          x,
                          ...)
{
  y_hats = predict.list(object$models,x=x,...)
  y_hats = Reduce("+",y_hats)/length(y_hats)
  return(y_hats)

}

#' @export
predict.list<-function(object,
                       ...)
{
  #object_fit = lapply(object,function(x) predict(x,...))
  lst = lapply(object,predict,...)
  return(lst)

}
