# --- set seed
set.seed(1)
load_dboost_modules()

# --- cone program:
n_z = 2
n_obs = 500
eps = 1/3
A = matrix(1,1,n_z)
b = 1
G = -diag(n_z)
h = matrix(0,n_z)#rep(0,n_z)

# --- lp cone:
control = scs::scs_control(acceleration_lookback = 7,eps_rel = 1e-02, eps_abs = 1e-02)
cone_lp = lp_cone_prep(A = A,
                       b = b,
                       G = G,
                       h = h,
                       cone = list(z = 0, l = 0, q = NULL))

# --- generate data:
x = runif(n_obs,0,3.14)
x = x[order(x)]
x = matrix(x)

# --- synthetic costs:
P = eps*diag(n_z)
tau = 0.15
cost1 = x + tau*rnorm(n_obs)
cost2 = x + sin(3*x) + tau*rnorm(n_obs)
cost = cbind(cost1,cost2)

# --- create oracle:
opt_oracle_lp = optim_scs(A = cone_lp$A,
                          b = cone_lp$b,
                          P = P,
                          cone = cone_lp$cone,
                          control = control)

# --- z_star true:
z_star = opt_oracle_lp(cost,P = diag(n_z)*0)
total_cost_star = sum(compute_cost(z_star,cost))

# --- mse boosting fit:
b_mse = boost_fit(x = x,
                  y = cost,
                  demean = FALSE,
                  model_method = 'dtree_fit',
                  model_args = list(max_depth = 0,
                                   min_obs = 0.01,
                                   step_size = 0.01,
                                   objective_method = 'objective_dtree_pve',
                                   maximize = NULL,
                                   y_hat_fn = NULL),
                 objective_method = 'objective_mse',
                 grad_method = 'grad_mse',
                 do_grad_project = TRUE,
                 maximize = NULL,
                 verbose = TRUE,
                 max_iter = 100,
                 weight_tol = 0.01,
                 objective_tol = 10^-4,
                 alpha_max = 1)

# --- full predictions:
cost_hat_mse = predict(b_mse,x=x)
z_hat_mse = opt_oracle_lp(cost_hat_mse)
total_cost_mse = sum(compute_cost(z_hat_mse,cost))


# --- decision aware:
b_spo = boost_fit(x = x,
                  y = cost,
                  demean = FALSE,
                  model_method = 'dtree_fit',
                  model_args = list(max_depth = 0,
                                    min_obs = 0.01,
                                    step_size = 0.01,
                                    objective_method = 'objective_dtree_qspo',
                                    maximize = NULL,
                                    y_hat_fn = NULL),
                  objective_method = 'objective_qspo',
                  grad_method = 'grad_spo',
                  do_grad_project = TRUE,
                  maximize = NULL,
                  verbose = TRUE,
                  max_iter = 100,
                  weight_tol = 0.01,
                  objective_tol=10^-4,
                  opt_oracle = opt_oracle_lp,
                  alpha_max = 1)

cost_hat_spo = predict(b_spo,x=x)
z_hat_spo = opt_oracle_lp(cost_hat_spo)
total_cost_spo = sum(compute_cost(z_hat_spo,cost))

total_cost_mse-total_cost_star
total_cost_spo-total_cost_star

# --- plot and prediction:
plot(x,cost1,type='l',lwd=3,ylim = c(0,5))
lines(x,cost2,col = 'darkred',lwd=4)
lines(x,cost_hat_mse[,1],col='grey',lwd=3)
lines(x,cost_hat_mse[,2],col='pink',lwd=3)
lines(x,cost_hat_spo[,1],col='grey',lwd=5)
lines(x,cost_hat_spo[,2],col='pink',lwd=5)



