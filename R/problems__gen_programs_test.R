# --- testing generation of programs:
if(F){

  # --- testing network flow problem:
  n_nodes = 5
  cone = generate_network_flow(n_nodes,0.75)
  n_vars = ncol(cone$A)
  opt_oracle_lp = optim_scs(A = cone$A,
                            b = cone$b,
                            P = cone$P,
                            cone = cone$cone,
                            control = scs::scs_control())
  cost = matrix(runif(n_vars),1,n_vars)
  test = opt_oracle_lp$solve(cost=cost)
  test

  test2 = optimr::solve_lp(cost = t(cost),
                           A = cone$A[1,,drop=F],
                           b = cone$b[1],
                           lb = -cone$b[2:(n_vars+1)],
                           ub = cone$b[(n_vars+2):(2*n_vars+1)],
                           cone = list(eq = 1))

  # --- testing LP
  n_vars = 100
  cone = generate_lp(n_vars,
                    min_lb = -3,
                    max_lb = 0,
                    min_ub = 0,
                    max_ub = 3)
  # --- define optimization oracle:
  opt_oracle_lp = optim_scs(A = cone$A,
                            b = cone$b,
                            P = cone$P,
                            cone = cone$cone,
                            control = scs::scs_control())
  cost = matrix(rnorm(n_vars),1,n_vars)
  test = opt_oracle_lp$solve(cost=cost)
  test

  test2 = optimr::solve_lp(cost = t(cost),
                           A = cone$A[1,,drop=F],
                           b = cone$b[1],
                           lb = -cone$b[2:(n_vars+1)],
                           ub = cone$b[(n_vars+2):(2*n_vars+1)],
                           cone = list(eq = 1))


  # --- testing QP
  n_vars = 100
  cone = generate_qp(n_vars,
                     min_lb = -3,
                     max_lb = 0,
                     min_ub = 0,
                     max_ub = 3)

  # --- define optimization oracle:
  opt_oracle_qp = optim_scs(A = cone$A,
                            b = cone$b,
                            P = cone$P,
                            cone = cone$cone,
                            control = scs::scs_control())
  cost = matrix(rnorm(n_vars),1,n_vars)
  test = opt_oracle_qp$solve(cost=cost)
  test

  test2 = optimr::solve_qp(Q = cone$P,
                           p = cost[1,],
                           A = NULL,
                           b = NULL,
                           G = cone$A,
                           h = cone$b,
                           control = optimr::qp_int_control(),
                           method = 'qp_int')

  plot(test[1,])
  points(test2$x,col='red')

  # --- testing SOCP
  n_vars = 100
  cone = generate_socp(n_vars,
                     min_lb = -3,
                     max_lb = 0,
                     min_ub = 0,
                     max_ub = 3)

  # --- define optimization oracle:
  opt_oracle_qp = optim_scs(A = cone$A,
                            b = cone$b,
                            P = cone$P,
                            cone = cone$cone,
                            control = scs::scs_control())
  cost = matrix(rnorm(n_vars),1,n_vars)
  test = opt_oracle_qp$solve(cost=cost)
  test

  sqrt(test%*%cone$Q%*%t(test))
  cone$tau






}
