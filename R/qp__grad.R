#' @export
grad_qp<-function(dl_dz,
                  M,
                  A,
                  z_star,
                  y_star)
{
  # ---  solve system
  sol = solve_qp(cost = dl_dz,
                 M = M,
                 b = 0)

  dz = sol$z_star
  dy = sol$y_star

  # --- gradients:
  dl_dc = dz
  #dl_dP = 0.5*(dz%*%t(z_star) + z_star%*%t(dz)) #dz%*%t(z_star)
  #dl_db = -dy
  #dl_dA = y_star%*%t(dz) + dy%*%t(z_star)

  # --- compute dl_dz and dl_dy:
  out = list(dl_dc =  dl_dc)#,
             #dl_db =  dl_db,
             #dl_dA =  dl_dA,
             #dl_dP = dl_dP)
}
