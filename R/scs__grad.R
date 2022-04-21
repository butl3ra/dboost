#' @export
grad_scs<-function(dl_dz,
                   cost,
                   A,
                   b,
                   M,
                   z_star,
                   y_star,
                   s_star,
                   cone,
                   info)
{
  # --- prep:
  cost = cost_prep(cost)
  dl_dz = cost_prep(dl_dz)
  n_grads = nrow(dl_dz)
  n_z = ncol(cost)
  n_A = nrow(A)
  n_b = nrow(b)

  # --- holders:
  dl_dc = matrix(0,n_grads ,n_z)
  dl_db = matrix(0,n_grads ,n_A)
  dl_dA = array(0,c(n_grads,n_A,n_z))
  dl_dP = array(0,c(n_grads,n_z,n_z))

  # --- u and v matrices:
  u_star = cbind(z_star,y_star)
  zero = matrix(0,n_grads,n_z)
  v_star = cbind(zero,s_star)

  # --- Identity:
  I = diag(nrow(M))

  # --- augment dl_dz
  zero = matrix(0,n_grads,n_A)# + 1
  dl_dz = cbind(dl_dz,zero)# --- doesnt support option for dl_dy

  # --- is_array_M
  is_array_M = is_array(M)
  M_i = M

  # --- invoke solver
  for(i in 1:n_grads){
    # --- check if is array
    if(is_array_M){
      M_i = M[i,,]
    }

    # --- get grad:
    sol  = grad_scs_core(dl_dz = dl_dz[i,],
                         z_star = z_star[i,],
                         y_star = y_star[i,],
                         s_star = s_star[i,],
                         u_star = u_star[i,],
                         v_star = v_star[i,],
                         M = M_i,
                         I = I,
                         info = info)
    dl_dc[i,] = sol$dl_dc
    dl_db[i,] = sol$dl_db
    dl_dA[i,,] = sol$dl_dA
    dl_dP[i,,] = sol$dl_dP
  }
  out = list(dl_dc =  dl_dc,
             dl_db =  dl_db,
             dl_dA =  dl_dA,
             dl_dP = dl_dP)
  return(out)
}

#' @export
grad_scs_core<-function(dl_dz,
                        z_star,
                        y_star,
                        s_star,
                        u_star,
                        v_star,
                        M,
                        I = diag(nrow(M)),
                        info,
                        eps = 10^-6
                        )
{
  # --- init:
  #M_I = M + I
  w = u_star - v_star

  # --- Derivative of euclidean projection operator:
  D = d_proj(w[info$idx_y],info)

  # --- Core system of Equations: (can maybe make more efficient)
  rhs = D%*%(-dl_dz)
  mat = M%*%D - D + I  + eps*I
  d = solve(t(mat),rhs)

  # --- d:
  dz = d[info$idx_z,,drop=F]
  dy = d[info$idx_y,,drop=F]

  # --- gradients:
  dl_dc = dz
  dl_dP = 0.5*(dz%*%t(z_star) + z_star%*%t(dz)) #dz%*%t(z_star)
  dl_db = dy
  dl_dA = y_star%*%t(dz) - dy%*%t(z_star)

  grads = list(dl_dc = dl_dc,
               dl_db = dl_db,
               dl_dA = dl_dA,
               dl_dP = dl_dP)

  return(grads)


}

#' @export
d_proj<-function(y,info)
{
  # --- prep:
  n_z = info$n_z
  n_y = length(y)
  n_total = n_z + n_y# + 1
  D = diag(n_total)
  if(info$any_ineq){
    idx = info$idx_ineq
    idx_D = n_z+idx
    D[idx_D,][,idx_D] = d_proj_nno(y[idx])
  }
  if(info$any_soc){
    for(i in 1:length(info$u_idx_soc)){
      idx = info$idx_soc[[i]]
      idx_D = n_z+idx
      D[idx_D,][,idx_D] = d_proj_soc(y[idx])
    }
  }
  return(D)
}

#' @export
d_proj_nno<-function(x)
{
  x = 0.5*(sign(x)+1)
  d = diag(as.numeric(x))
  return(d)
}

#' @export
d_proj_soc<-function(x,eps = 10^-8)
{
  n_x = length(x)
  x1 = x[1]
  x2 = x[-1]
  x_norm = lp_norm(x2, p =2)

  # --- satisfies cone:
  if(x_norm < (x1 + eps) ){
    Dx = diag(n_x)
  }
  # --- cone has zero volume:
  else if(x_norm < -(x1 - eps)) {
    Dx = diag(rep(0,n_x))
  }
  # --- outside the cone:
  else{
    x2_mat = matrix(x2/x_norm)
    xx = x2_mat%*%t(x2_mat)

    d = diag(n_x-1)
    Dx = diag(rep(0,n_x))
    Dx[1,1] = x_norm
    Dx[1,2:n_x] = x2
    Dx[2:n_x,1]=x2
    Dx[2:n_x,][,2:n_x] = (x1 + x_norm)*d - x1*xx
    Dx = (1/(2*x_norm))*Dx
  }

  return(Dx)
}

# dep:
if(F){
  # --- init:
  M_I = M + I
  w = u_star - v_star

  # --- Derivative of euclidean projection operator:
  D = d_proj(w[info$idx_y],info)

  # --- Core system of Equations: (can make more efficient)
  rhs = D%*%(-dl_dz)
  mid = D - solve(M,2*D - I) + eps*I

  # --- d:
  d = solve(t(mid),rhs)
  d = solve(t(M),d)

  # --- dz, dy, dtau
  dz = d[info$idx_z,,drop=F]
  dy = d[info$idx_y,,drop=F]
  dtau = d[length(d),,drop=F]

  # --- grads:
  dl_dc = (dz - dtau[1]*z_star)
  dl_db = (dy - dtau[1]*y_star)
  dl_dA = y_star%*%t(dz) - dy%*%t(z_star)
}
