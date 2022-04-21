#' @export
generate_network_flow<-function(n_nodes = 5,
                                decay = 0.75
                                )
{
  # --- probability matrix:
  v = 1:n_nodes
  mat = dist(v,method = 'manhattan',upper=T,diag=T)
  mat = as.matrix(mat)
  mat = mat-1
  mat[upper.tri(mat,TRUE)]=Inf
  mat = decay^mat
  # --- convert binomial connections:
  cmat = apply(mat,2,rbinom,n=n_nodes,size = 1)
  n_edges = sum(cmat)

  # --- flow_0
  A = matrix(0,n_nodes,n_edges)
  A[1,1] = -1
  A[n_nodes,n_edges] = 1

  # --- construct graph
  edge = 0
  for(i in 1:nrow(cmat)){
    for(j in 1:ncol(cmat)){
      if(cmat[i,j] == 1){
        edge = edge+1
        A[j,edge] = -1
        A[i,edge] = 1
      }

    }
  }

  # --- constraints:
  b = rep(0,n_nodes)
  b[1] = -1
  b[n_nodes] = 1
  b = matrix(b)
  # --- add 0 1 constraints:
  G = rbind(-diag(n_edges),diag(n_edges))#diag(n_edges)#
  h = rbind(matrix(0,n_edges),matrix(1,n_edges))#matrix(1,n_edges)#


  # --- network cone:
  cone_network = lp_cone_prep(A = A,
                              b = b,
                              G = G,
                              h = h,
                              cone = list(z = 0, l = 0, q = NULL))

  return(cone_network)


}
