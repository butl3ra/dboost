#' @export
predict.boost_fit<-function(object,
                            x,
                            ...)
{
  # --- prep:
  x = make_matrix(x)
  # --- unpack
  weight = object$weight
  sub_models = object$models
  y_hats = NULL
  n_models = length(sub_models)
  # --- main loop:
  for(i in 1:n_models){
    # --- unpack
    obj = sub_models[[i]]
    # --- single prediction
    y_hat = predict(obj,x=x)
    w = weight[i]
    if(is.null(y_hats)){
      y_hats = w*y_hat
    }
    else{
      y_hats = y_hats + w*y_hat
    }

  }

  return(y_hats)

}
