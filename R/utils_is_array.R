#' @export
is_array<-function(x)
{
  d = dim(x)
  test = is.array(x) & length(d) > 2
  return(test)
}
