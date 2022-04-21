#' @export
is_vector<-function(x,
         mode = c('character','numeric'),
         operator = "|"
)
{
  out = lapply(mode,is.vector,x=x)
  Reduce(operator,out)
}
