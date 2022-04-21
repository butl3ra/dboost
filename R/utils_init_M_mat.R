#' @export
init_M_mat<-function(P,A)
{
  if(is_array(P)){
    nr = nrow(P)
    nc = ncol(P) + nrow(A)
    M = array(0,c(nr,nc,nc))
    for(i in 1:nr){
      M[i,,] = init_M_mat1(P[i,,],A)
    }
  }
  else{
    M = init_M_mat1(P,A)
  }

  return(M)
}

#' @export
init_M_mat1<-function(P,A)
{
  n_con = nrow(A)
  M1= cbind(P,t(A))
  M2 = cbind(-A,matrix(0,n_con,n_con))
  M =  rbind(M1,M2)
  return(M)
}

#' @export
update_M_mat<-function(P,M)
{
  if(!is.null(P)){
    if(is_array(P)){
      M_0 = M
      nr = nrow(P)
      nc = ncol(M)
      M = array(0,c(nr,nc,nc))
      for(i in 1:nr){
        M[i,,] = update_M_mat1(P[i,,],M_0[i,,])
      }
    }
    else{
      M = update_M_mat1(P,M)
    }
  }

  return(M)
}

#' @export
update_M_mat1<-function(P,M)
{
  n_z = ncol(P)
  idx = 1:n_z
  M[idx,idx] = P
  return(M)
}


#' @export
init_M_mat_qp<-function(P,A)
{
  if(is_array(P)){
    nr = nrow(P)
    nc = ncol(P) + nrow(A)
    M = array(0,c(nr,nc,nc))
    for(i in 1:nr){
      M[i,,] = init_M_mat1_qp(P[i,,],A)
    }
  }
  else{
    M = init_M_mat1_qp(P,A)
  }

  return(M)
}

#' @export
init_M_mat1_qp<-function(P,A)
{
  n_con = nrow(A)
  M1= cbind(P,t(A))
  M2 = cbind(A,matrix(0,n_con,n_con))
  M =  rbind(M1,M2)
  return(M)
}
