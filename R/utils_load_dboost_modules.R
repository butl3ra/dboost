#' @export
load_dboost_modules<-function(...)
{
  inst_root = system.file("",package = 'dboost')
  folders = list.files(inst_root,full.names=T)
  idx = grepl("inst__",folders)
  folders = folders[idx]
  files = list.files(folders,full.names=T)
  files = files[!grepl('class',files)]
  for(tmp in files){
    source(tmp)
  }
}
