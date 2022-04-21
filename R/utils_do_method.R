#' @export
do_method<-function(method,
         method_args,
         ...)
{
  if(missing(method_args)){
    method_args = NULL
  }
  var_args=list(...)
  args = c(method_args,var_args)
  do_call(method,args)
}
