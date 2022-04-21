#' @export
nullspace<-function (M)
{
  if (!is.numeric(M))
    stop("Argument 'M' must be a numeric matrix.")
  if (is.vector(M))
    M <- matrix(c(M), nrow = length(M), ncol = 1)
  qrM <- qr(t(M))
  rnk <- qrM$rank
  if (rnk == ncol(M))
    return(NULL)
  inds <- if (rnk == 0)
    1:ncol(M)
  else -(1:rnk)
  qrQ <- qr.Q(qrM, complete = TRUE)[, inds, drop = FALSE]
  if (length(qrQ) == 0)
    return(NULL)
  else return(qrQ)
}
