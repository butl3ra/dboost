# ------------------------------------------------------
# Generate lp network flow problems:
# ------------------------------------------------------
dir_out = '~/Dropbox (Personal)/workspace_phd/papers/paper_8_dboost/experiment_qp/'
dir.create(dir_out,recursive = T)
file_names_0 = c('in_sample_cost','out_of_sample_cost','in_sample_opt_cum_cost','out_of_sample_opt_cum_cost')
file_names_0 = paste0(dir_out,file_names_0)
# --- poly degree 3 and poly degree 9
# --- noise_multiplier 1, 10
noise_multiplier_taus = c(0,0.50,1)

# --- problem variables:
n_obs = 1000
n_x = 5
pct_true = 0.5
poly_degree = 1:3
intercept = F
intercept_mean = 0.0
# --- popt
eps = 0.01
n_z = 25
control = scs::scs_control(acceleration_lookback = 7)
A = matrix(1,1,n_z)
b = 1
n_A = 3
#G = rbind(-diag(n_z),diag(n_z))
#h = rbind(matrix(1,n_z),matrix(1,n_z))

# --- experiment:
n_sims = 10
idx_all = 1:(2*n_obs)
idx_train = idx_all[1:n_obs]
idx_oos = idx_all[-idx_train]

# --- model specs:
model_methods = c(cart_0 = 'dtree_fit',
                  cart_1 = 'dtree_fit',
                  cart_2 = 'dtree_fit',
                  random_forest_0 = 'bag_fit',
                  random_forest_1 = 'bag_fit',
                  random_forest_2 = 'bag_fit',
                  spot_0 = 'dtree_fit',
                  spot_1 = 'dtree_fit',
                  spot_2 = 'dtree_fit',
                  spot_forest_0 = 'bag_fit',
                  spot_forest_1 = 'bag_fit',
                  spot_forest_2 = 'bag_fit',
                  boost_0 = 'boost_fit',
                  boost_1 = 'boost_fit',
                  boost_2 = 'boost_fit',
                  dboost_0 = 'boost_fit',
                  dboost_1 = 'boost_fit',
                  dboost_2 = 'boost_fit')

# --- cart args:


# --- args:
max_depth = 0:2
min_obs = 0.05
step_size = 0.05
n_samples = 100
obs_fraction = 0.5
vars_fraction = 0.5
verbose = TRUE
weight_tol = 10^-4
objective_tol = 10^-4

# --- args templates:
cart_args = lapply(max_depth,function(md){
  cart = list(max_depth = md,
              min_obs = min_obs,
              step_size = step_size,
              objective_method = 'objective_dtree_pve')
})
rf_args = lapply(max_depth,function(md){
  list(model_method = 'dtree_fit',
       model_args = list(max_depth = md,
                         min_obs = min_obs,
                         step_size = step_size,
                         objective_method = 'objective_dtree_pve'),
       n_samples = n_samples,
       obs_fraction = obs_fraction,
       vars_fraction = vars_fraction,
       verbose = verbose
  )
})
spot_args = lapply(max_depth,function(md){
  spot = list(max_depth = md,
              min_obs = min_obs,
              step_size = step_size,
              objective_method = 'objective_dtree_qspo')
})

spot_forest_args = lapply(max_depth,function(md){
  list(model_method = 'dtree_fit',
       model_args = list(max_depth = md,
                         min_obs = min_obs,
                         step_size = step_size,
                         objective_method = 'objective_dtree_qspo'),
       n_samples = n_samples,
       obs_fraction = obs_fraction,
       vars_fraction = vars_fraction,
       verbose = verbose)
})

boost_args = list(model_method = 'dtree_fit',
                  model_args = list(max_depth = 0,
                                    min_obs = min_obs,
                                    step_size = step_size,
                                    objective_method = 'objective_dtree_pve'),
                  objective_method = 'objective_mse',
                  grad_method = 'grad_mse',
                  do_grad_project = TRUE,
                  maximize = NULL,
                  verbose = verbose,
                  max_iter = n_samples,
                  weight_tol = weight_tol,
                  objective_tol = objective_tol)

boost_args = lapply(max_depth,function(md){
  list(model_method = 'dtree_fit',
       model_args = list(max_depth = md,
                         min_obs = min_obs,
                         step_size = step_size,
                         objective_method = 'objective_dtree_pve'),
       objective_method = 'objective_mse',
       grad_method = 'grad_mse',
       do_grad_project = TRUE,
       maximize = NULL,
       verbose = verbose,
       max_iter = n_samples,
       weight_tol = weight_tol,
       objective_tol = objective_tol)
})

dboost_args = list(model_method = 'dtree_fit',
                   model_args = list(max_depth = 0,
                                     min_obs = min_obs,
                                     step_size = step_size,
                                     objective_method = 'objective_dtree_qspo'),
                   objective_method = 'objective_qspo',
                   grad_method = 'grad_qspo',
                   do_grad_project = TRUE,
                   maximize = NULL,
                   verbose = verbose,
                   max_iter = n_samples,
                   weight_tol = weight_tol,
                   objective_tol = objective_tol)

dboost_args = lapply(max_depth,function(md){
  list(model_method = 'dtree_fit',
       model_args = list(max_depth = md,
                         min_obs = min_obs,
                         step_size = step_size,
                         objective_method = 'objective_dtree_qspo'),
       objective_method = 'objective_qspo',
       grad_method = 'grad_qspo',
       do_grad_project = TRUE,
       maximize = NULL,
       verbose = verbose,
       max_iter = n_samples,
       weight_tol = weight_tol,
       objective_tol = objective_tol)
})

#boost_args = lapply(max_depth,function(md){
#  list(model_method = 'dtree_fit',
#       model_args = list(max_depth = md,
#                         min_obs = min_obs,
#                         step_size = step_size,
#                         objective_method = 'objective_dtree_pve'),
#       objective_method = 'objective_mse',
#       grad_method = 'grad_mse',
#       do_grad_project = TRUE,
#       maximize = NULL,
#       verbose = verbose,
#       max_iter = n_samples,
#       weight_tol = weight_tol,
#       objective_tol = objective_tol)
#})

model_args = c(cart_args,rf_args,spot_args,spot_forest_args,
               boost_args,dboost_args)
names(model_args) = names(model_methods)

# --- holders:
n_models = length(model_methods)
model_names = names(model_methods)

# --- big loop:

for(tau in noise_multiplier_taus){
  cat('tau:',tau)
  noise_multiplier_tau = tau
  in_sample_cost = out_of_sample_cost = matrix(NA,n_sims,n_models,dimnames = list(NULL,model_names))
  #in_sample_spo_loss = out_of_sample_spo_loss = matrix(NA,n_sims,n_models,dimnames = list(NULL,model_names))
  in_sample_opt_cum_loss = out_of_sample_opt_cum_loss = matrix(NA,n_sims,n_models,dimnames = list(NULL,model_names))


  #mean(rowSums(costs*z_hat_db)[-1]) + 0.05*mean(rowSums(abs(diff(z_hat_db))))
  # --- main loop:
  for(i in 1:n_sims){

    # --- set seed
    set.seed(i)
    cat('simulation: ',i,'\n')

    # --- generate data:
    dat = generate_problem_data(n_x = n_x,
                                n_z = n_z,
                                n_obs = 2*n_obs,
                                pct_true = pct_true,
                                noise_multiplier_tau = noise_multiplier_tau,
                                polys = poly_degree,
                                intercept = intercept,
                                intercept_mean = intercept_mean)


    # --- cost and x
    x_train = dat$x[idx_train,,drop=F]
    costs_train = dat$cost[idx_train,,drop = F]
    x_oos = dat$x[idx_oos,,drop=F]
    costs_oos = dat$cost[idx_oos,,drop = F]

    L_mat = matrix(runif(n_z^2,-1,1),n_z,n_z)
    P_true = L_mat%*%t(L_mat)+ eps*diag(n_z)
    L_mat = matrix(rnorm(n_z^2*10,0,1),10*n_z,n_z)
    error_train = t(L_mat)%*%L_mat/(10*n_z)
    L_mat = matrix(rnorm(n_z^2*10,0,1),10*n_z,n_z)
    error_oos = t(L_mat)%*%L_mat/(10*n_z)
    P_train = P_true + 0.10*error_train#max(tau,0.10)
    P_oos = P_true + 0.10*error_oos
    #P_train = P_true + tau*diag(n_z)
    #P_oos = P_true + tau*diag(n_z)

    # --- random equality constraint:
    con = generate_eq_constraint(n_z,n_A)
    # --- generate oracle:
    cone = lp_cone_prep(A = con$A,
                        b = con$b,
                        G = NULL,
                        h = NULL)


    # --- create oracle:
    opt_oracle_qp = optim_qp(A = cone$A,
                             b = cone$b,
                             P = P_train,
                             P_eval = P_true)


    # --- optimal decision:
    z_star_train = opt_oracle_qp(costs_train,P = P_true)
    z_star_oos = opt_oracle_qp(costs_oos,P = P_true)

    cost_star_train = compute_qcost(z_star_train,costs_train,P=P_true)
    cost_star_oos = compute_qcost(z_star_oos,costs_oos,P=P_true)

    in_sample_opt_cum_loss[i,] = sum(cost_star_train)
    out_of_sample_opt_cum_loss[i,] = sum(cost_star_oos)

    # --- sub looop across models:
    for(nm in model_names){
      cat('model: ',nm,'\n')

      opt_oracle_qp = optim_qp(A = cone$A,
                               b = cone$b,
                               P = P_train,
                               P_eval = P_true)

      method = model_methods[[nm]]
      args = model_args[[nm]]
      args$opt_oracle = opt_oracle_qp

      if(grepl('forest',nm)){
        args$model_args$opt_oracle = opt_oracle_qp
      }

      # --- fit model
      model = do_method(method = method,method_args = args, x = x_train, y = costs_train)

      costs_hat_train = predict(model,x = x_train)
      costs_hat_oos = predict(model,x = x_oos)

      z_hat_train = opt_oracle_qp(costs_hat_train,P = P_train)
      z_hat_oos = opt_oracle_qp(costs_hat_oos,P = P_oos)

      # --- optimal predictions
      cost_hat_train = compute_qcost(z_hat_train,costs_train,P=P_true)
      cost_hat_oos = compute_qcost(z_hat_oos,costs_oos,P=P_true)

      in_sample_cost[i,nm] = sum(cost_hat_train)
      out_of_sample_cost[i,nm] = sum(cost_hat_oos)




    }



    #in_sample_cost[i,]
    #out_of_sample_cost[i,]

    nmt = format(noise_multiplier_tau,nsmall=2)
    nmt = gsub("[.]","_",nmt)
    file_names = paste0(file_names_0,"_",nmt,".csv")

    write.csv(in_sample_cost,file_names[1],row.names=F)
    write.csv(out_of_sample_cost,file_names[2],row.names=F)
    write.csv(in_sample_opt_cum_loss,file_names[3],row.names=F)
    write.csv(out_of_sample_opt_cum_loss,file_names[4],row.names=F)

  }



}

#--- define dir out
# --- file name a write:
