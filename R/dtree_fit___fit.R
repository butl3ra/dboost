#' @export
dtree_fit<-function(x,
                    y,
                    data = NULL,
                    demean = FALSE,
                    max_depth = 2,
                    min_obs = 0.10,
                    step_size = 0.05,
                    objective_method = 'objective_dtree_abs_y_hat',
                    maximize = NULL,
                    y_hat_fn = NULL,
                    ...
                    )
{
  # --- prep:
  x =  make_matrix(x)
  y = make_matrix(y)
  rownames(x) = NULL
  rownames(y) = NULL
  if(min_obs < 1){
    min_obs = floor(min_obs * nrow(x))
  }

  # --- get maximize:
  if(is.null(maximize)){
    maximize = get_dtree_min_max(method = objective_method)
  }

  # --- define objective function
  obj_fn = match.fun(objective_method)
  if(!is.null(y_hat_fn)){
    y_hat_fn = match.fun(y_hat_fn)
  }

  # --- demean:
  if(demean){
    y = scale2(y,center=T,normalize=F,na.rm=T)
  }

  # --- get x_quantiles: candidate splits
  n_steps = round(1/step_size)-1
  probs = linspace(step_size,1-step_size,n_steps)
  x_quants =  colQuantiles(x,probs = probs,type=1L,drop=F)
  x_quants = t(x_quants)

  # --- compute decision tree node:
  node = dtree_fit_core(x = x,
                        y = y,
                        data = data,
                        x_quants = x_quants,
                        max_depth = max_depth,
                        min_obs = min_obs,
                        obj_fn = obj_fn,
                        maximize = maximize,
                        y_hat_fn = y_hat_fn,
                        ...)

  # --- make tree:
  tree = make_tree(node = node,x_names = colnames(x))
  tree = make_class_object(tree,'dtree_fit')

  return(tree)





}

#' @export
make_tree<-function(node,
                    x_names = NULL,
                    keep = c("class",
                             "x_index",
                             "rhs_value",
                             "y_hat",
                             "child_true",
                             "child_false"
                             ))
{
  # --- root node:
  tree = node[names(node)%in%keep]
  if(tree$class == "node"){
    # --- convetion to take  <
    tree$operator = "<"
    # -- replace x_index with name
    if(!is.null(x_names)){
      tree$x_index = x_names[tree$x_index]
    }
    # --- recursive call to make_trre
    tree$child_true = make_tree(tree$child_true,x_names = x_names,keep = keep)
    tree$child_false = make_tree(tree$child_false,x_names = x_names,keep = keep)
  }
  return(tree)






  # --- root node:
  if(is.null(tree)){
    tree = data.frame(x_index = c(node$x_index,NA,NA),
                      operator = c("<",NA,NA),
                      rhs_value = c(node$rhs_value,NA,NA),
                      if_true = c(2,NA,NA),
                      if_false = c(3,NA,NA),
                      y_hat_true = c(node$y_split$y_hat_true,NA,NA),
                      y_hat_false = c(node$y_split$y_hat_false,NA,NA)
    )
  }
}

#' @export
dtree_fit_core<-function(x,
                         y,
                         data,
                         x_quants,
                         max_depth,
                         min_obs,
                         obj_fn,
                         maximize,
                         y_hat_fn = NULL,
                         ...)
{
  # --- dtree greedy splits:
  node = get_split(x = x,
                   y = y,
                   data = data,
                   x_quants = x_quants,
                   min_obs = min_obs,
                   obj_fn = obj_fn,
                   maximize = maximize,
                   y_hat_fn = y_hat_fn,
                   ...)
  # --- recursive splits:
  node = dtree_fit_splits(node = node,
                          x_quants = x_quants,
                          max_depth = max_depth,
                          min_obs = min_obs,
                          obj_fn = obj_fn,
                          maximize = maximize,
                          y_hat_fn = y_hat_fn,
                          depth = 0,
                          ...)

  return(node)



}



#' @export
dtree_fit_splits<-function(node,
                           x_quants,
                           max_depth,
                           min_obs,
                           obj_fn,
                           maximize,
                           y_hat_fn = NULL,
                           depth = 0,
                           ...)
{
  # --- prep:
  x_true = node$x_split$x_true
  x_false = node$x_split$x_false
  y_true = node$y_split$y_true
  y_false = node$y_split$y_false
  y_hat_true = node$y_split$y_hat_true
  y_hat_false = node$y_split$y_hat_false
  data_true = node$data_split$data_true
  data_false = node$data_split$data_false
  x_index = node$x_index

  # --- check for max depth:
  if(depth >= max_depth){
    node$child_true = get_leaf(y_hat = y_hat_true)
    node$child_false = get_leaf(y_hat = y_hat_false)
  }
  # --- recursively partition
  else{
    # --- if left is valid:
    if(nrow(x_true) >= 2*min_obs ){
      node$child_true = get_split(x = x_true,
                                  y = y_true,
                                  data = data_true,
                                  x_quants = x_quants,
                                  min_obs = min_obs,
                                  obj_fn = obj_fn,
                                  maximize = maximize,
                                  y_hat_fn = y_hat_fn,
                                  ...)
      # --- if no valid splits exists
      if(is.null(node$child_true$x_index)){
        node$child_true = get_leaf(y_hat = y_hat_true)
      }
      else{
        node$child_true = dtree_fit_splits(node$child_true,
                                           x_quants = x_quants,
                                           max_depth = max_depth,
                                           min_obs = min_obs,
                                           obj_fn = obj_fn,
                                           maximize = maximize,
                                           y_hat_fn = y_hat_fn,
                                           depth = depth + 1,
                                           ...)
      }
    }
    else{
      node$child_true = get_leaf(y_hat = y_hat_true)
    }
    # --- if right is valid:
    if(nrow(x_false) >= 2*min_obs ){
      node$child_false = get_split(x = x_false,
                                  y = y_false,
                                  data = data_false,
                                  x_quants = x_quants,
                                  min_obs = min_obs,
                                  obj_fn = obj_fn,
                                  maximize = maximize,
                                  y_hat_fn = y_hat_fn,
                                  ...)
      # --- if no valid splits exists
      if(is.null(node$child_false$x_index)){
        node$child_false = get_leaf(y_hat = y_hat_false)
      }
      else{
        node$child_false = dtree_fit_splits(node$child_false,
                                            x_quants = x_quants,
                                            max_depth = max_depth,
                                            min_obs = min_obs,
                                            obj_fn = obj_fn,
                                            maximize = maximize,
                                            y_hat_fn = y_hat_fn,
                                            depth = depth + 1,
                                            ...)
      }

    }
    else{
      node$child_false = get_leaf(y_hat = y_hat_false)
    }
  }

  return(node)


}

#' @export
get_leaf<-function(y_hat){
  out = list(class = "leaf",
             y_hat = y_hat)
  return(out)
}


#' @export
get_split<-function(x,
                    y,
                    data,
                    x_quants,
                    min_obs,
                    obj_fn,
                    maximize,
                    y_hat_fn = NULL,
                    ...
                    )
{
  # --- prep:
  best_x_index = NULL
  best_rhs_value = NULL
  best_loss_value = Inf
  best_y_hat_list = NULL
  best_split_index_list = NULL
  best_x_split = NULL
  best_data_split = NULL

  nc_x = ncol(x)
  nr_x = nrow(x)
  n_splits = nrow(x_quants)
  y_hat = colMeans2(y,na.rm=T)

  # --- if we have enough obs to make a split:
  if(nr_x >= 2*min_obs){
    # --- main loop:
    for(x_index in 1:nc_x){

      x1 = x[,x_index,drop=F]

      for(j in 1:n_splits){
        rhs_value = x_quants[j,x_index]
        # --- get split
        split_index_list = get_split_row_index(x1 = x1,rhs_value = rhs_value)
        # --- check split:
        is_valid_split = check_split(split_index_list = split_index_list,min_obs = min_obs)
        # --- compute loss if split is valid
        if(is_valid_split){
          # --- get data split:
          data_split = get_split_data(data = data,split_index_list = split_index_list)
          # --- get y_hat
          y_hat_list = get_split_y_hat(y = y,split_index_list,y_hat_fn = y_hat_fn,data = data_split)
          # --- get loss:
          loss = evaluate_split(y_hat_list = y_hat_list,
                                data_split = data_split,
                                obj_fn = obj_fn,
                                maximize = maximize,
                                y = y,
                                y_hat = y_hat,
                                ...)
          # --- check if this is the best loss:
          if(loss < best_loss_value){
            best_loss_value = loss
            best_x_index = x_index
            best_rhs_value = rhs_value
            best_y_hat_list = y_hat_list
            best_split_index_list = split_index_list
            best_data_split = data_split
          }
        }
      }
    }
  }
  # --- get best_x_split
  if(!is.null(best_x_index)){
    best_x_split = do_split(x = x,best_split_index_list)
  }

  # --- return info:
  out = list(class = "node",
             x_index = best_x_index,
             rhs_value = best_rhs_value,
             x_split = best_x_split,
             y_split = best_y_hat_list,
             data_split = best_data_split)

 return(out)
}



#' @export
get_split_row_index<-function(x1,
                              rhs_value)
{
  true = x1 < rhs_value
  false = !true
  out = list(true = true,
             false = false)
  return(out)
  #x1 = x[,x_index,drop=F]
}

#' @export
check_split<-function(split_index_list,
                      min_obs)
{
  # --- compute n_obs
  n_obs_true = sum(split_index_list$true,na.rm=T)
  n_obs_false = sum(split_index_list$false,na.rm=T)

  is_valid = n_obs_true >= min_obs & n_obs_false >= min_obs

  return(is_valid)

}

#' @export
do_split<-function(x,
                   split_index_list)
{
  # --- compute x_true and x_false:
  x_true = x[split_index_list$true,,drop=F]
  x_false = x[split_index_list$false,,drop=F]

  out = list(x_true = x_true,
             x_false = x_false)
  return(out)
}

#' @export
get_split_data<-function(data,
                         split_index_list)
{
  if(!is.null(data)){
    data = lapply(data,do_split,split_index_list = split_index_list)
    data = transpose_list(data)
    names(data) = c('data_true','data_false')
  }
  return(data)
}

#' @export
get_split_y_hat<-function(y,
                          split_index_list,
                          y_hat_fn = NULL,
                          data = NULL,
                          ...)
{
  # --- compute y_true and y_false:
  lst = do_split(y,split_index_list)
  y_true = lst$x_true
  y_false = lst$x_false

  # --- compute y_hat:
  if(is.null(y_hat_fn)){
    y_hat_true = colMeans2(y_true,na.rm=T)
    y_hat_false = colMeans2(y_false,na.rm=T)
  }
  else{
    y_hat_list = y_hat_fn(y_true = y_true,y_false = y_false,data = data, ...)
    y_hat_true = y_hat_list$y_hat_true
    y_hat_false = y_hat_list$y_hat_false
  }

  # --- return list
  out = list(y_true = y_true,
             y_false = y_false,
             y_hat_true = y_hat_true,
             y_hat_false = y_hat_false)

  return(out)
}



#' @export
evaluate_split<-function(y_hat_list,
                         data_split,
                         obj_fn,
                         maximize,
                         y,
                         y_hat,
                         ...)
{
  obj_value = obj_fn(y_true = y_hat_list$y_true,
                     y_hat_true = y_hat_list$y_hat_true,
                     y_false = y_hat_list$y_false,
                     y_hat_false = y_hat_list$y_hat_false,
                     y = y,
                     y_hat = y_hat,
                     data = data_split,
                     ...)
  if(maximize){
    obj_value = -obj_value
  }
  return(obj_value)

  # --- compute obj_true
  obj_true = obj_fn(y =y_hat_list$y_true,
                    y_hat = y_hat_list$y_hat_true)

  # --- compute_obj_false
  obj_false = obj_fn(y =y_hat_list$y_false,
                     y_hat = y_hat_list$y_hat_false)

  obj_value = obj_true + obj_false

}

