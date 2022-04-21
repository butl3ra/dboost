#' @export
bag_fit<-function(x,
                  y,
                  data = NULL,
                  model_method = 'dtree_fit',
                  model_args = list(demean = FALSE,
                                    max_depth =3,
                                    min_obs = 0.01,
                                    step_size = 0.01,
                                    objective_method = 'objective_dtree_spo',
                                    maximize = FALSE,
                                    y_hat_fn = NULL),
                  n_samples = 100,
                  obs_fraction = 0.50,
                  vars_fraction = 0.25,
                  verbose = TRUE,
                  ...)
{
  # --- prep:
  x = make_matrix(x)
  y = make_matrix(y)

  # --- prep obs:
  obs_fraction = max(min(obs_fraction,1),0)
  n_obs = nrow(x)
  s_obs =  ceiling(n_obs*obs_fraction)
  obs_all = 1:n_obs

  # --- prep vars:
  vars_fraction = max(min(vars_fraction,1),0)
  n_vars = ncol(x)
  s_vars = max(1, ceiling(vars_fraction*n_vars))
  vars_all = 1:n_vars

  # --- holders:
  models_list = list()

  # --- main loop:
  for( i in 1:n_samples){
    # --- verbose:
    if(verbose){
      cat('---------- ','\n')
      cat('re-sample iteration: ', i, '\n')
      cat('---------- ','\n')
    }

    # --- sample observations and variables:
      idx_obs_in_bag = sample(obs_all,s_obs,replace = FALSE)
      idx_vars = sample(vars_all,s_vars,replace = FALSE)


    # --- index data:
      x_in_bag = x[idx_obs_in_bag,idx_vars,drop=F]
      y_in_bag = y[idx_obs_in_bag,,drop=F]
      data_in_bag = sample_list(data,idx_obs_in_bag)

    # --- solution:
    model = do_method(method = model_method,
                      method_args = model_args,
                      x = x_in_bag,
                      y = y_in_bag,
                      data = data_in_bag)


    models_list[[i]] = model

  }

  # --- contains: models, out_of_bag predictions, objective for convergence plots
  object = list(models = models_list)

  object = make_class_object(object,'bag_fit')


  return(object)

}



