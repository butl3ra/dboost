#' @export
boost_fit<-function(x,
                    y,
                    data = NULL,
                    demean = FALSE,
                    model_method = 'dtree_fit',
                    model_args = list(max_depth = 0,
                                      min_obs = 0.05,
                                      step_size = 0.05,
                                      objective_method = 'objective_dtree_pve',
                                      maximize = NULL,
                                      y_hat_fn = NULL),
                    objective_method = 'objective_mse',
                    grad_method = 'grad_mse',
                    do_grad_project = TRUE,
                    maximize = NULL,
                    verbose = FALSE,
                    max_iter = 100,
                    weight_tol = 0.01,
                    objective_tol = 10^-4,
                    local_tol = 0.25,
                    alpha_min = 0,
                    alpha_max = 100,
                    ...
)
{
  # --- prep:
  x = make_matrix(x)
  y = y_true = make_matrix(y)
  f_x = y_true*0

  # --- demean:
  if(demean){
    y = scale2(y,center=T,normalize=F,na.rm=T)
  }

  # --- prep objective fn and gradient fn:
  if(is.null(maximize)){
    maximize = get_boost_min_max(method = objective_method)
  }
  is_null_objective = is.null(objective_method)
  maximize = ifelse(is_null_objective,FALSE,maximize)
  obj_fn = ifelse(is_null_objective,'objective_mse',objective_method)
  grad_fn = ifelse(is.null(grad_method),'grad_mse',grad_method)
  obj_fn = match.fun(obj_fn)
  grad_fn = match.fun(grad_fn)


  # --- prep for main loop:
  models_list = list()
  models_is = NULL
  weight = NULL
  objective = NULL
  k = 0
  do_run = TRUE
  # --- main loop
  while(do_run){
    k = k+1
    # --- switch to projection loss on gradient after first iteration:
    if(k == 2 & do_grad_project){
      model_args$objective_method = 'objective_dtree_pve'
    }

    # --- in-sample models
    model = fit_boost_core(x = x,
                           y = y,
                           data = data,
                           model_method =  model_method,
                           model_args = model_args,
                           ...)

    # --- predict y_hats
    y_hats = predict(model,x = x)


    # --- append optimal model:
    if(k == 1){
      w_star = 1
    }
    else{
      sol = optimize(fit_boost_w_star,
                        interval = c(alpha_min,alpha_max),
                        f_x = f_x,
                        y_hat = y_hats,
                        y = y_true,
                        data = data,
                        obj_fn = obj_fn,
                        ...,
                        maximum = maximize,
                        tol = 0.01)
      w_star = sol[[1]]

      # --- local optimum handling:
      min_ob = min(objective)
      #local_flag = (sol$objective - objective[k-1])/abs(objective[k-1]) > local_tol
      local_flag = (sol$objective - min_ob)/abs(min_ob) > local_tol
      if(local_flag){
        w_star = 0
      }
    }

    # --- updates:
    weight = c(weight,w_star)
    f_x = f_x + w_star * y_hats
    models_list[[k]] = model

    # --- residualize: y = y - w_star*y_hat_opt <==> y = y_true - f_x
    y = grad_fn(y = y_true, y_hat = f_x,...)

    #  --- append objective
    objective_value = obj_fn(y = y_true, y_hat = f_x,...)
    objective = c(objective,objective_value)

    # --- verbose:
    if(verbose ){
      feature = get_features(model)
      feature = paste(feature,collapse = ", ")
      cat('iteration: ',k, '\n')
      cat('feature: ',feature,'\n')
      cat('w_star: ',w_star,'\n')
      cat('objective_value: ',objective_value,'\n')
    }

    # --- keep running as long as we are making improvements
    run_1 = k < max_iter
    run_2 =  w_star > weight_tol
    run_3 = TRUE
    if(k > 1){
      obj_imp = abs(objective[k]-objective[k-1])/abs(objective[k])
      run_3 = obj_imp > objective_tol
    }
    do_run = run_1 & run_2 & run_3

  }

  # --- return a boost_fit which has a list of meta models and their associated weight
  out = list(weight = weight,
             models = models_list,
             objective = objective)
  out = make_class_object(out,'boost_fit')
  return(out)


}


#' @export
get_boost_min_max<-function(method)
{
  min_or_max = objective_boost_min_max_switch(method)
  if(min_or_max == 'maximize'){
    out = TRUE
  }
  else{
    out = FALSE
  }
  return(out)
}

#' @export
objective_boost_min_max_switch<-function(method)
{
  min_or_max = switch(method,
                      objective_mse = 'minimize',
                      objective_spo = 'minimize',
                      objective_qspo = 'minimize')

  return(min_or_max)
}


#' @export
fit_boost_w_star<-function(w_star,
                           f_x,
                           y_hat,
                           y,
                           obj_fn,
                           sum_1 = FALSE,
                           ...)
{
  if(sum_1){
    f_x_new = (1-w_star)*f_x + w_star*y_hat
  }
  else{
    f_x_new = f_x + w_star*y_hat
  }
  value = obj_fn(y = y, y_hat = f_x_new,...)
  return(value)
}

#' @export
fit_boost_core<-function(x,
                         y,
                         model_method,
                         model_args,
                         ...)
{
  # --- return list of models
  model = do_method(model_method,
                    method_args = model_args,
                    x = x,
                    y = y,
                    ...)
  return(model)
}






