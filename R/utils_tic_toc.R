#' @export
tic<-function(...)
{
  assign("elapsedTime", proc.time()[3], envir =  .dboostEnv)
  invisible()
}

#' @export
toc<-function (echo = TRUE,
               ...)
{
  prevTime <- get("elapsedTime", envir =  .dboostEnv)
  diffTimeSecs <- proc.time()[3] - prevTime
  if (echo) {
    cat(sprintf("elapsed time is %f seconds", diffTimeSecs),"\n")
    return(invisible(diffTimeSecs))
  }
  else {
    return(diffTimeSecs)
  }
}
