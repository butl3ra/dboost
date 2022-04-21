#' @export
roc<-function(x,
         nlag = 1
)
{
  y=x/mlag(x,nlag)-1
}
