#' @export
solve_scs<-function(cost,
                    A,
                    b,
                    P,
                    cone,
                    initial,
                    control){
  # --- prep:
  cost = cost_prep(cost)
  n_cost = nrow(cost)
  n_z = ncol(cost)
  n_A = nrow(A)
  z_star = matrix(0,n_cost,n_z)
  y_star = s_star =  matrix(0,n_cost,n_A)

  # --- is_array_P
  is_array_P = is_array(P)
  P_i = P


  # --- invoke solver
  for(i in 1:nrow(cost)){
    # --- check if is array
    if(is_array_P){
      P_i = P[i,,]
    }
    sol = scs::scs(obj = cost[i,],
                   P = P_i,
                   A = A,
                   b = b,
                   cone = cone,
                   initial = initial,
                   control = control)
    z_star[i,] = sol$x
    y_star[i,] = sol$y
    s_star[i,] = sol$s
  }
  out = list(z_star = z_star,
             y_star = y_star,
             s_star = s_star)
  return(out)
}



