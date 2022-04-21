#' @export
vec <- function(x,
                byrow=FALSE
)
{
  if(is.matrix(x)|is.data.frame(x)|is.array(x)){
    if(byrow){
      x = t(x)
    }
    dim(x) = length(x)

  }
  else if(!is.vector(x)){
    cl = class(x)
    msg = paste('cannot vectorize object of class',cl)
    stop(msg)
  }
  return(x)
}

#' @export
vech <- function(x)
{
  d = dim(x)
  len_d = length(d)
  if(len_d!=2 | d[1]!=d[2]){#isSymmetric
    stop("vech only defined for square-symmetric matrices")
  }
  idx = lower.tri(x,diag=T)
  y = x[idx]
  return(y)
}

#' @export
vec_inv <- function(x,
                    nrow,
                    ncol,
                    byrow = FALSE,
                    dimnames=NULL)
{
  y = matrix(x,nrow=nrow,ncol=ncol,byrow=byrow,dimnames=dimnames)
  return(y)
}

#' @export
vech_inv <- function(x)
{
  len_x = length(x)
  d = (-1+sqrt(1+ 8*len_x))/2
  if(round(d)!= d){
    stop("Number of elements in x will not form a square matrix")
  }
  mat = matrix(0,nrow=d,ncol=d)
  idx_lower = lower.tri(mat)
  idx_lower_diag = idx_lower
  diag(idx_lower_diag)=T
  idx_upper = upper.tri(mat)
  mat[idx_lower_diag] = x
  mat[idx_upper] <- mat[idx_lower]

  return(mat)
}



#' @export
elem <- function(i, d)
{
  elem_vec <- rep(0, d)
  elem_vec[i] <- 1
  return(elem_vec)
}
