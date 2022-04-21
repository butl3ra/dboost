#' @export
scale2<-function(x,
                 center = T,
                 normalize = T,
                 na.rm =T,
                 mean_x = NULL,
                 sd_x = NULL,
                 mean_factor = 1)
{
  ncol_x = ncol(x)
  if(length(ncol_x)<1){
    x = scale_vector(x,
                     center=center,
                     normalize=normalize,
                     na.rm=na.rm,
                     mean_x = mean_x,
                     sd_x = sd_x,
                     mean_factor = mean_factor)
  }
  else{
    x = scale_matrix(x,
                     center=center,
                     normalize=normalize,
                     na.rm=na.rm,
                     mean_x = mean_x,
                     sd_x = sd_x,
                     mean_factor = mean_factor)
  }
  return(x)
}

#' @export
scale_vector<-function(x,
                       center =T,
                       normalize = T,
                       na.rm =T,
                       mean_x = NULL,
                       sd_x = NULL,
                       mean_factor = 1)
{
  if(center){

    if(is.null(mean_x)){
      mean_x = mean(x,na.rm=na.rm)
    }
    mean_x = mean_x * mean_factor
    x = x - mean_x
  }
  if(normalize){
    if(is.null(sd_x)){
      sd_x = sd(x,na.rm=na.rm)
    }
    x = x/sd_x
  }
  return(x)
}

#' @export
scale_matrix<-function(x,
                       center =T,
                       normalize = T,
                       na.rm =T,
                       mean_x = NULL,
                       sd_x = NULL,
                       mean_factor = 1)
{
  ncol_x = ncol(x)
  if(center){
    if(is.null(mean_x)){
      mean_x =  matrixStats::colMeans2(x,na.rm=na.rm)
    }
    mean_x = mean_x * mean_factor
    x = sweep(x, 2, mean_x, '-',check.margin=F)
  }
  if(normalize){
    if(is.null(sd_x)){
      sd_x =  matrixStats::colSds(x,na.rm=na.rm)
    }
    x = sweep(x, 2, sd_x, '/',check.margin=F)
  }
  return(x)
}
