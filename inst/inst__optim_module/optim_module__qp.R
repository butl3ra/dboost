# --- optim_scs oracle:
if(F){
  optim_qp = optim_module(
    classname = "OptimQP",
    # --- init
    initialize = function(P,
                          A = NULL,
                          b = NULL,
                          P_eval = NULL,
                          ...)
    {
      # --- prep A and b and P
      n_z = ncol(P)

      # --- init P:
      if(!is.null(A) & !is.null(b) ){
        A = make_matrix(A)
        b = make_matrix(b)
      }

      # --- constraint specs:
      self$A = A
      self$b = b
      self$P = P
      self$P_eval = P_eval


      # --- store matrix::
      self$M = init_M_mat_qp(P = P,A = A)

    },
    # --- solve function:
    solve = function(cost,
                     P = NULL,
                     ...) {
      # --- dynamic P
      if(is.null(P)){
        P = self$P
        M = self$M
      }
      else{
        M = update_M_mat(P,self$M)
        self$M = M
      }
      # --- call to main solver:
      sol = solve_qp(cost = cost,
                      M = M,
                      b = self$b)

      # --- store z_star,y_star, s_star
      self$z_star = sol$z_star
      self$y_star = sol$y_star
      self$cost = cost

      return(sol$z_star)

    },
    # --- grad function:
    grad = function(dl_dz,
                    ...)
    {

      # --- call to main grad solver:
      grads =  grad_qp(dl_dz = dl_dz,
                       A = self$A,
                       M = self$M,#self$M,
                       z_star = self$z_star,
                       y_star = self$y_star)

      return(grads)

    }
  )
}

