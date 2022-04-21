#' @export
update_cone<-function(cone  = list(z = 0, l = 0, q = NULL),
                      z = 0,
                      l = 0,
                      q = NULL)
{
  if(is.null(cone$z)){
    cone$z = 0
  }
  if(is.null(cone$l)){
    cone$l = 0
  }
  cone$z = cone$z + z
  cone$l = cone$l + l
  cone$q = c(cone$q, q)
  return(cone)
}


#' @export
lp_cone_prep<-function(A,
                       b,
                       G,
                       h,
                       cone = list(z = 0, l = 0, q = NULL)
                             )
{
  n_eq = get_ncon(A)
  n_ineq = get_ncon(G)

  if(n_eq > 0 & n_ineq > 0){
    A_out = rbind(A,G)
    b_out = rbind(b,h)
  }
  else if(n_eq > 0 ){
    A_out = A
    b_out = b
  }
  else{
    A_out = G
    b_out = h
  }

  cone = update_cone(cone, z = n_eq, l = n_ineq)

  return(list(A = A_out,b = b_out,cone = cone))
}

#' @export
mvt_cone_prep<-function(A,
                        b,
                        G,
                        h,
                        Q,
                        vol_target = 0.10,
                        scale_factor = 1,
                        rescale_factor = 100,
                        cone = list(z = 0, l = 0, q = NULL))
{
  # --- prep:
  n_x = ncol(Q)
  n_s = n_x+1


  # --- lp cone:
  cone_lp = lp_cone_prep(A = A,
                         b = b,
                         G = G,
                         h = h,
                         cone = cone)

  A = cone_lp$A
  b = cone_lp$b
  cone = cone_lp$cone
  n_A = get_ncon(A)

  # ---- add second-order cone constraint:
  vol_target = vol_target/scale_factor*sqrt(rescale_factor)
  Q12 = chol(Q * rescale_factor)

  A = rbind(A,rep(0,n_x),Q12)
  b = rbind(b,vol_target,matrix(0,n_x))
  cone = update_cone(cone,q = n_s)




  return(list(A = A,b = b,cone = cone))

  cone_list = list(z = 1,l=n_y,q=n_y+1)

  # --- remaining second order cone variables equals Vx
  A_Q12 = rbind(matrix(0,1,n_x),Q12)
  A_Q12 = cbind(A_Q12, -diag(n_s))
  A_Q12[1,n_s] = 1
  A = cbind(A,matrix(0,n_A,n_s ))
  A = rbind(A_Q12,A)
  b = rbind(matrix(vol_target),matrix(0,n_x),b)
  cone = update_cone(cone,z = n_s)

  # --- socp:
  n_socp = n_s + n_x
  A_s = -diag(n_socp)[(n_x+1) : n_socp,]
  A = rbind(A,A_s)

  b = rbind(b,matrix(0,n_s))
  cone = update_cone(cone,q = n_s)
}

#' @export
ellipsoidal_cost_cone_prep<-function(A,
                                     b,
                                     cone,
                                     cov_cost)
{
  # --- prep:
  con_total = sum(unlist(cone))
  n_cost = ncol(A)
  n_soc = n_cost + 2
  con_aug = con_total + n_soc

  # --- index for adding new cost vectors:
  col_idx = 1:n_cost
  row_idx = con_total + 1

  # --- augment with zeros:
  A = cbind(A,0)

  # --- compute cholesky decomposition:
  cov_chol = chol(cov_cost)

  # --- SOC Constraint:
  A_top = matrix(c(rep(0,n_cost),-1),1)
  A_bot = rbind(cbind(cov_chol,0),0)
  Anew = rbind(A_top,A_bot)
  bnew = rep(0,n_soc)

  # --- Final constraint matrix:
  A_aug = rbind(A,Anew)
  b_aug = rbind(matrix(b),matrix(bnew))
  cone_aug = update_cone(cone,q = n_soc)


  # --- return list:
  out = list(A = A_aug,
             b = b_aug,
             cone = cone_aug,
             cost_col_idx = col_idx,
             cost_row_idx = row_idx
             )








}

#' @export
robust_cost_cone_prep<-function(A,
                                b,
                                cone,
                                n_cost)
{
  # --- prep:
  con_total = sum(unlist(cone))
  con_aug = con_total + n_cost
  vars_total = ncol(A)
  vars_aug = vars_total + 1
  col_idx = 1:vars_total

  # --- holders:
  A_aug = matrix(0,con_aug,vars_aug)
  b_aug = matrix(0,con_aug)

  # --- create the augmented robust counterpart:
  n_eq = sum(cone$z)
  if(n_eq > 0){
    row_idx = 1:n_eq
    A_aug[row_idx,col_idx] = A[row_idx,,drop=F]
    b_aug[row_idx] = b[row_idx]
  }

  # --- add the robust inequality constraints:
  cost_idx = (n_eq+1):(n_eq + n_cost)
  A_aug[cost_idx,vars_aug] = -1
  cone$l = sum(cone$l) + n_cost

  # --- add back all all other constraints:
  n_other = con_total - n_eq
  if(n_other > 0){
    row_idx = (n_eq + n_cost +1):con_aug
    row_idx2 = (n_eq + 1):con_total
    A_aug[row_idx,col_idx] = A[row_idx2,,drop=F]
    b_aug[row_idx] = b[row_idx2]
  }

  # --- return list:
  out = list(A = A_aug,
             b = b_aug,
             cone = cone,
             cost_idx = cost_idx)








}
