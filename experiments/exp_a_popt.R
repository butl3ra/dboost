# ------------------------------------------------------
# Generate lp network flow problems:
# ------------------------------------------------------
dir_out = '~/Dropbox (Personal)/workspace_phd/papers/paper_8_dboost/experiment_popt/'
dir.create(dir_out,recursive = T)
file_names_0 = c('in_sample_cost','out_of_sample_cost','in_sample_opt_cum_cost','out_of_sample_opt_cum_cost')
file_names_0 = paste0(dir_out,file_names_0)
load_dboost_modules()
# --- noise_multiplier 1, 10
noise_multiplier_taus = c(0,0.5,1)

# --- problem variables:
n_obs = 1000
n_x = 5
n_factors = 4
pct_true = 0.5
rho = 0
poly_degree = 1:5
noise_multiplier_tau = 0
intercept = T
intercept_mean = 0
# --- popt
eps = 1
n_z = 10
control = scs::scs_control(acceleration_lookback = 7)
A = matrix(1,1,n_z)
b = 1
G = -diag(n_z)
h = matrix(0,n_z)#rep(0,n_z)

# --- experiment:
n_sims = 10
idx_all = 1:(2*n_obs)
idx_train = idx_all[1:n_obs]
idx_oos = idx_all[-idx_train]

# ---
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
n_samples = 250
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
              objective_method = 'objective_dtree_spo')
})

spot_forest_args = lapply(max_depth,function(md){
  list(model_method = 'dtree_fit',
       model_args = list(max_depth = md,
                         min_obs = min_obs,
                         step_size = step_size,
                         objective_method = 'objective_dtree_spo'),
       n_samples = n_samples,
       obs_fraction = obs_fraction,
       vars_fraction = vars_fraction,
       verbose = verbose)
})


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


dboost_args = lapply(max_depth,function(md){
  list(model_method = 'dtree_fit',
       model_args = list(max_depth = md,
                         min_obs = min_obs,
                         step_size = step_size,
                         objective_method = 'objective_dtree_spo'),
       objective_method = 'objective_spo',
       grad_method = 'grad_spo',
       do_grad_project = TRUE,
       maximize = NULL,
       verbose = verbose,
       max_iter = n_samples,
       weight_tol = weight_tol,
       objective_tol = objective_tol)
})


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
    #dat$cost = -dat$cost
    #dat = generate_popt_data(n_x = n_x,
    #                         n_y = n_z,
    #                         n_obs = 2*n_obs,
    #                         n_factors = n_factors,
    #                         pct_true = pct_true,
    #                         noise_multiplier_tau = noise_multiplier_tau,
    #                         poly_degree = poly_degree)

    # --- generate covariance:
    L_mat = matrix(runif(n_z*n_factors,-1,1),n_z,n_factors)
    V = L_mat%*%t(L_mat) + eps*diag(n_z)

    # --- cost and x
    x_train = dat$x[idx_train,,drop=F]
    costs_train = dat$cost[idx_train,,drop = F]
    x_oos = dat$x[idx_oos,,drop=F]
    costs_oos = dat$cost[idx_oos,,drop = F]

    # --- generate oracle:
    #V = cov(dat$cost) + eps*diag(n_z)
    ew = rep(1/n_z,n_z)
    vol_target = as.numeric(sqrt(ew%*%V%*%ew))
    cone = mvt_cone_prep(A = A,
                         b = b,
                         G = G,
                         h = h,
                         Q = V,
                         vol_target = vol_target,
                         scale_factor = 1,
                         rescale_factor = 100,
                         cone = list(z = 0, l = 0, q = NULL))

    # --- create oracle:
    opt_oracle_socp = optim_scs(A = cone$A,
                              b = cone$b,
                              P = cone$P,
                              cone = cone$cone,
                              control = control)





    # --- optimal decision:
    z_star_train = opt_oracle_socp(costs_train)
    z_star_oos = opt_oracle_socp(costs_oos)

    cost_star_train = compute_cost(z_star_train,costs_train)
    cost_star_oos = compute_cost(z_star_oos,costs_oos)

    in_sample_opt_cum_loss[i,] = sum(cost_star_train)
    out_of_sample_opt_cum_loss[i,] = sum(cost_star_oos)

    # --- sub looop across models:
    for(nm in model_names){
      cat('model: ',nm,'\n')

      method = model_methods[[nm]]
      args = model_args[[nm]]
      args$opt_oracle = opt_oracle_socp

      if(grepl('forest',nm)){
        args$model_args$opt_oracle = opt_oracle_socp
      }
      #if(grepl('boost',nm)){
      #  args$model_args$opt_oracle = opt_oracle_lp
      #  #args$model_args$model_args$opt_oracle = opt_oracle_lp
      #}

      # --- fit model
      model = do_method(method = method,method_args = args, x = x_train, y = costs_train)

      costs_hat_train = predict(model,x = x_train)
      costs_hat_oos = predict(model,x = x_oos)

      z_hat_train = opt_oracle_socp(costs_hat_train)
      z_hat_oos = opt_oracle_socp(costs_hat_oos)

      # --- optimal predictions
      cost_hat_train = compute_cost(z_hat_train,costs_train)
      cost_hat_oos = compute_cost(z_hat_oos,costs_oos)

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
