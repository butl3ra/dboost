#' @export
predict.dtree_fit<-function(object,
                            x,
                            ...)
{
  # --- prep:
  x = make_matrix(x)
  nr_x = nrow(x)
  nc_y_hat = get_dtree_y_hat_dim(object)
  y_hat = matrix(0,nr_x,nc_y_hat)
  index = rep(T,nr_x)

  # --- vectorized predictions:
  y_hat = predict_dtree_fit_core(tree = object,
                                 x = x,
                                 y_hat = y_hat,
                                 index = index)
  return(y_hat)

  # --- loop through each x
  for(i in 1:nr_x){
    y_hat[i] = predict_dtree_fit_core_slow(tree = object, x1 = x[i,])
  }
}

#' @export
predict_dtree_fit_core<-function(tree,
                                 x,
                                 y_hat = matrix(0,nr_x),
                                 index = rep(T,nrow(x)))
{
  if(tree$class == "node"){
    true = x[,tree$x_index] < tree$rhs_value

    # --- if true
    true_index = true & index
    y_hat_true = predict_dtree_fit_core(tree = tree$child_true,x = x,y_hat = y_hat,index = true_index)
    # --- if false
    false_index = !true & index
    y_hat_false = predict_dtree_fit_core(tree = tree$child_false,x = x,y_hat = y_hat,index = false_index )
    y_hat = y_hat_true + y_hat_false
  }
  else{
    if(ncol(y_hat)>1){
      y_hat = t(y_hat)
      y_hat[,index] = tree$y_hat
      y_hat = t(y_hat)
    }
    else{
      y_hat[index,] = tree$y_hat
    }
  }
  return(y_hat)
}

#' @export
predict_dtree_fit_core_slow<-function(tree,
                                 x1)
{
  if(tree$class == "node"){
    # --- if true
    if(x1[tree$x_index] < tree$rhs_value){
      y_hat = predict_dtree_fit_core_slow(tree = tree$child_true,x1 = x1)
    }
    else{
      y_hat = predict_dtree_fit_core_slow(tree = tree$child_false,x1 = x1)
    }
  }
  else{
    y_hat = tree$y_hat
  }
  return(y_hat)
}

#' @export
get_dtree_y_hat_dim<-function(tree)
{
  if(tree$class == "node"){
    dim = get_dtree_y_hat_dim(tree = tree$child_true)
  }
  else{
    dim = length(tree$y_hat)
  }
  return(dim)

}


