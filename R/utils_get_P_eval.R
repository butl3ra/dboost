#' @export
get_P_eval<-function(opt_oracle){
  P = opt_oracle$P_eval
  if(is.null(P)){
    P = opt_oracle$P
  }
  return(P)
}
