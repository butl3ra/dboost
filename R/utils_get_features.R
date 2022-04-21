#' @export
get_features<-function(object,
                       ...)
{
  UseMethod("get_features")
}

#' @export
get_features.list<-function(object,
                            ...
)
{
  features = lapply(object,get_features)
  return(features)
}

#' @export
get_features.bag_fit<-function(object,
                               ...
)
{
  features = get_features(object$models)
  return(features)
}

#' @export
get_features.boost_fit<-function(object,
                                 ...
)
{
  features = get_features(object$models)
  return(features)
}

#' @export
get_features.bin_fit<-function(object,
                               ...)
{
  features = colnames(object$x)
  return(features)
}

#' @export
get_features.bbt_fit<-function(object,
                               ...)
{
  features = colnames(object$x)
  return(features)
}

#' @export
get_features.dtree_fit<-function(object,
                                 ...)
{
  features = get_features_dtree(object,features = NULL)
  return(features)
}

#' @export
get_features.dunit_fit<-function(object,
                                 ...)
{
  features = get_features_dtree(object,features = NULL)
  return(features)
}

#' @export
get_features_dtree<-function(tree,
                             features = NULL)
{
  if(tree$class == "node"){
    features = c(features,tree$x_index)
    features = get_features_dtree(tree$child_true,features = features)
    features = get_features_dtree(tree$child_false,features = features)
  }
  return(features)
}
