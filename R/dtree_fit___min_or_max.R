#' @export
objective_dtree_min_max_switch<-function(method)
{
  min_or_max = switch(method,
                      objective_dtree_mse = 'minimize',
                      objective_dtree_pve = 'maximize',
                      objective_dtree_spo = 'minimize',
                      objective_dtree_qpo = 'minimize',
                      objective_dtree_qspo = 'minimize'
                     )

  return(min_or_max)
}
#' @export
get_dtree_min_max<-function(method)
{
  min_or_max = objective_dtree_min_max_switch(method)
  if(min_or_max == 'maximize'){
    out = TRUE
  }
  else{
    out = FALSE
  }
  return(out)
}
