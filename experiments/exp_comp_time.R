# ------------------------------------------------------
# Generate lp network flow problems:
# ------------------------------------------------------
dir_out = '~/Dropbox (Personal)/workspace_phd/papers/paper_8_dboost/experiment_comp_time/'
dir.create(dir_out,recursive = T)
load_dboost_modules()
# --- params
n_z_all = c(5, 10, 25, 50, 100)
n_sims = 10
noise_multiplier_tau = 1
n_factors = 4

# --- args:
max_depth = 0
min_obs = 0.05
step_size = 0.05
n_samples = 10
obs_fraction = 0.5
vars_fraction = 0.5
verbose = TRUE
weight_tol = -1# 10^-4
objective_tol = -1# 10^-4

model_methods = c(boost_0 = 'boost_fit',
                  dboost_0 = 'boost_fit')
                  #boost_1 = 'boost_fit',
                  #boost_2 = 'boost_fit',
                  #dboost_0 = 'boost_fit'
                  #dboost_1 = 'boost_fit',
                  #dboost_2 = 'boost_fit')

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

model_args = c(boost_args,dboost_args)
names(model_args) = names(model_methods)

# --- holders:
n_models = length(model_methods)
model_names = names(model_methods)

# --- problem variables:
n_x = 5
pct_true = 0.5
poly_degree = 1:3
intercept = F
intercept_mean = 0.0
# --- popt
eps = 0.01
control = scs::scs_control(acceleration_lookback = 7)
bound_range = c(1,2)



#n_obs = 50
#n_obs = 250
n_obs = 1000
# --- n_z:
for(n_z in n_z_all){
  cat('n_z:',n_z)
  n_A = min(ceiling(n_z/5),10)

  time_mat = matrix(NA,n_sims,n_models)
  colnames(time_mat) = model_names

  # --- main loop:
  for(i in 1:n_sims){

    # --- set seed
    set.seed(i)
    cat('simulation: ',i,'\n')

    # --- generate data:
    dat = generate_problem_data(n_x = n_x,
                                n_z = n_z,
                                n_obs = n_obs,
                                pct_true = pct_true,
                                noise_multiplier_tau = noise_multiplier_tau,
                                polys = poly_degree,
                                intercept = intercept,
                                intercept_mean = intercept_mean)

    # --- cost and x
    x_train = dat$x
    costs_train = dat$cost


    # --- lb and ub:
    lb = -runif(n_z,min=bound_range[1],max=bound_range[2])
    ub = runif(n_z,min=bound_range[1],max=bound_range[2])

    # --- G_model:
    G0 = diag(n_z)
    G = rbind(-G0,G0)

    # --- h_model:
    h = rbind(matrix(-lb),matrix(ub))

    A = matrix(1,1,n_z)
    b = 1

    # --- generate covariance:
    L_mat = matrix(runif(n_z*n_factors,-1,1),n_z,n_factors)
    V = L_mat%*%t(L_mat) + eps*diag(n_z)

    # --- generate oracle:
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





    # --- sub looop across models:
    for(nm in model_names){
      cat('model: ',nm,'\n')

      method = model_methods[[nm]]
      args = model_args[[nm]]
      args$opt_oracle = opt_oracle_socp

      if(grepl('forest',nm)){
        args$model_args$opt_oracle = opt_oracle_socp
      }

      # --- fit model
      # --- forward pass
      tic()
      model = do_method(method = method,method_args = args, x = x_train, y = costs_train)
      comp_time = toc(echo=F)

      time_mat[i,nm] = comp_time


    }

    file_name = paste0(dir_out,"comp_time_n_obs_",n_obs,"_n_z_",n_z,".csv")
    write.csv(time_mat,file_name,row.names=F)


  }



}
