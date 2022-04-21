#' @export
make_class_object<-function(object,
         class_name)
{
  if(missing(object)){
    self<-list()
  }
  else{
    self = object
  }
  attr(self, "class") <- class_name
  return(self)
}
