#' @export
quad_form<-function(m,P)
{
  # --- prep:
  is_array_P = is_array(P)
  nr_m = nrow(m)
  nr_P = nrow(P)
  if(nr_m > 1 & nr_P >1 & nr_m!=nr_P & is_array_P){
    stop('m and P dimension not compatible')
  }
  nr = max(nr_m,nr_P)
  v = rep(0,nr)
  P_i = P
  m_i = m[1,]
  # --- main_loop
  for(i in 1:nr){
    if(is_array_P){
      P_i = P[i,,]
    }
    if(nr_m > 1){
      m_i = m[i,]
    }
    v[i] = m_i%*%P_i%*%m_i
  }
  return(v)
}
