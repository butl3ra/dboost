#' @export
get_cone_problem_info<-function(A,cone,default = list(z = 0, l = 0, q = 0))
{
  # --- prep cone with defaults
  if(is.null(cone$z)){
    cone$z = default$z
  }
  if(is.null(cone$l)){
    cone$l = default$l
  }
  if(is.null(cone$q)){
    cone$q = default$q
  }

  # --- variables:
  n_z = ncol(A)
  n_con = nrow(A)
  n_eq = cone$z
  n_ineq = cone$l
  n_soc = cone$q
  any_eq = n_eq > 0
  any_ineq = n_ineq > 0
  any_soc = any(n_soc > 0)

  # --- indexes
  idx_z = 1:n_z
  idx_eq = NULL
  u_idx_eq = NULL
  idx_ineq = NULL
  u_idx_ineq = NULL
  idx_soc = NULL
  idx_y= NULL
  u_idx_soc = NULL

  # --- equality index
  if(any_eq){
    idx_eq = 1:n_eq
    u_idx_eq = n_z + idx_eq
    idx_y = c(idx_y,u_idx_eq)
  }
  # --- inequality index
  if(any_ineq){
    idx_ineq =  (n_eq+1):(n_eq + n_ineq)
    u_idx_ineq = n_z + idx_ineq
    idx_y = c(idx_y,u_idx_ineq)
  }
  # --- soc index list
  if(any_soc){
    idx_count = 0
    idx_soc = list()
    u_idx_soc = list()
    for(i in 1:length(n_soc)){
      n_soc1 = n_soc[i]
      idx_soc[[i]] = (idx_count + n_eq+n_ineq + 1):(idx_count+n_eq+n_ineq + n_soc1)
      u_idx_soc[[i]] = n_z + idx_soc[[i]]
      idx_count = n_soc1
    }
    idx_y = c(idx_y,unlist(u_idx_soc))
  }

  # --- return out:
  out = list(n_z = n_z,
             n_con = n_con,
             n_eq = n_eq,
             n_ineq = n_ineq,
             n_soc = n_soc,
             any_eq = any_eq,
             any_ineq = any_ineq,
             any_soc = any_soc,
             idx_z = idx_z,
             idx_eq = idx_eq,
             idx_ineq = idx_ineq,
             idx_soc = idx_soc,
             idx_y = idx_y,
             u_idx_eq = u_idx_eq,
             u_idx_ineq = u_idx_ineq,
             u_idx_soc = u_idx_soc )
  return(out)

}
