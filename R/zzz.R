# --- tic/toc structure like pracma
.dboostEnv<- new.env()
assign("elapsedTime", 0, envir = .dboostEnv)


.onLoad <-function(libname,
                   pkgname)
{
  # --- package dependencies:
  #install.packages("scs")
  #install.packages("R6")
  #install.packages(("microbenchmark"))
  #install.packages("matrixStats")

  # --- source packages:
  library("scs")
  library("R6")
  library("microbenchmark")
  library('matrixStats')


  # --- define microbenchmarking tool
  mb = microbenchmark::microbenchmark

  # --- source defined classes: inst
  spor_inst_root = system.file("",package = 'dboost')
  folders = list.files(spor_inst_root,full.names=T)
  idx = grepl("inst__",folders)
  folders = folders[idx]
  files = list.files(folders,full.names=T)
  for(tmp in files){
    source(tmp)
  }
  rm('tmp')

  environment(.dboostEnv) <- asNamespace("dboost")

}
