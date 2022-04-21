#' @export
objective_boost_spo<-function(y,
                               z_x,
                               z_star,
                               reduce = 'mean',
                                 ...)
{
  # --- spo cost:
  z_diff = z_x - z_star
  cost = rowSums(z_diff*y)

  if(!is.null(reduce)){
    cost = reduce_objective(cost,reduce = reduce)
  }
  return(cost)
}


