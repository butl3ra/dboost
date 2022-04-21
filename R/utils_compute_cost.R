
#' @export
compute_cost<-function(z,
                       cost)
{
  return(rowSums(z*cost,na.rm=T))
}

#' @export
compute_qcost<-function(z,
                        cost,
                        P,
                        lambda = 1)
{
  l = compute_cost(z,cost)
  q = quad_form(z,P)
  value = l + 0.5*lambda*q
  return(value)
}



#' @export
compute_avg_cost<-function(z,
                           cost)
{
  zc = compute_cost(z,cost)
  mean(zc,na.rm = T)
}
