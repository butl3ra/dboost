#' @export
linspace<-function(x1,
                   x2,
                   n=100)
{
  d = x2-x1
  by = (d)/(n-1)
  x = (0:(d/by))
  x*by+x1
}
