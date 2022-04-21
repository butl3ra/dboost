#' @export
cost_prep<-function(cost)
{
  if(is.vector(cost)){
    cost = matrix(cost,1,length(cost))
  }

  return(cost)
}

#' @export
robust_cost_prep<-function(cost,
                           nn_cost)
{
  cost = cost_prep(cost)
  cost = array(cost,c(nrow(cost),1,ncol(cost)))
  cost = abind::abind(cost,nn_cost,along = 2)
  return(cost)
}
