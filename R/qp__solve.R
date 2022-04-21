#' @export
solve_qp<-function(cost,
                   M,
                   b)
{
  # --- prep:
  cost = cost_prep(cost)
  n_cost = nrow(cost)
  n_z = ncol(cost)
  z_idx = 1:n_z
  is_array_M = is_array(M)

  # --- append b
  n_b = ncol(M)-n_z
  b = matrix(b,n_cost,n_b,byrow=T)
  rhs = cbind(-cost, b)

  # --- solution:
  if(!is_array_M){
    rhs = t(rhs)
    sol = solve(M,rhs)
    sol = t(sol)
  }
  else{
    sol = matrix(0,n_cost,n_z + n_b)
    for(i in 1:n_cost){
      sol[i,] =  solve(M[i,,],rhs[i,])
    }
  }

  z_star = sol[,z_idx,drop=F]
  y_star = sol[,-z_idx,drop=F]

  out = list(z_star = z_star,
             y_star = y_star)
  return(out)
}
