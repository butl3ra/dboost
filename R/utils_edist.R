#' @export
edist<-function(x,
                x_new = NULL)
{
  if(missing(x_new)){
    x_new = NULL
  }
  # --- self distance:
  if(is.null(x_new)){
    d = edist_self(x)
  }
  else{
   d = edist_core(x_new = x_new, x = x)
    if(F){
    # --- prep: slow but stable
    nr_x = nrow(x)
    nr_x_new = nrow(x_new)
    idx_new = nr_x+(1:nr_x_new)
    xx = rbind(x,x_new)
    d = edist_self(xx)
    # --- distance of new x to all x values
    d = d[idx_new,,drop=F]
    # --- distance of new x to training x values
    d = d[,-idx_new,drop=F]
    }
  }
  return(d)
}

#' @export
edist_core<-function(x_new,
                     x)
{
  dim_x = dim(x)
  dim_x_new = dim(x_new)
  mm = x_new%*%t(x)
  m_sq_x = rowSums(x*x)
  m_sq_x_new = rowSums(x_new*x_new)
  d = outer(m_sq_x_new,m_sq_x,"+") - 2*mm
  d[ d < 0] = 0
  d = sqrt(d)
}

#' @export
edist_self<-function(m)
{
  mm = m%*%t(m)
  m_sq = rowSums(m*m)
  d = outer(m_sq,m_sq,"+") - 2*mm
  d[ d < 0] = 0
  d = sqrt(d)
  return(d)
}


