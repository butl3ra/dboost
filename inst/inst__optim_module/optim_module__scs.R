# --- optim_scs oracle:
optim_scs = optim_module(
  classname = "OptimSCS",
  # --- init
  initialize = function(A,
                        b,
                        cone,
                        P = NULL,
                        initial = NULL,
                        control = scs::scs_control(),
                        ...)
    {
    # --- prep A and b and P
    n_z = ncol(A)
    A = make_matrix(A)
    b = make_matrix(b)
    # --- init P:
    if(is.null(P) | length(P)==1 ){
      P = matrix(0,n_z,n_z)
    }

    # --- constraint specs:
    self$A = A
    self$b = b
    self$P = P
    self$cone = cone
    self$initial = initial
    self$control = control

    # --- store info about the problem:
    self$info = get_cone_problem_info(A = A, cone = cone)

    # --- store matrix::
    #self$Q = init_Q_mat(A = A,b = b,cost = NULL)
    self$M = init_M_mat(P = P,A = A)

  },
  # --- solve function:
  solve = function(cost,
                   P = NULL,
                   ...) {

    # --- dynamic P
    if(is.null(P)){
      P = self$P
    }
    # --- call to main solver:
    sol = solve_scs(cost = cost,
                    P = P,
                    A = self$A,
                    b = self$b,
                    cone = self$cone,
                    initial = self$initial,
                    control = self$control)

    # --- store z_star,y_star, s_star
    self$z_star = sol$z_star
    self$y_star = sol$y_star
    self$s_star = sol$s_star
    self$cost = cost

    return(sol$z_star)

  },
  # --- grad function:
  grad = function(dl_dz,
                  P = NULL,
                  ...)
  {
    # --- Update M for potential change in P:
    M = update_M_mat(P,self$M)

    # --- call to main grad solver:
    grads =  grad_scs(dl_dz = dl_dz,
                      cost = self$cost,
                      A = self$A,
                      b = self$b,
                      M = M,#self$M,
                      z_star = self$z_star,
                      y_star = self$y_star,
                      s_star = self$s_star,
                      cone = self$cone,
                      info = self$info)

    return(grads)

  }
)


