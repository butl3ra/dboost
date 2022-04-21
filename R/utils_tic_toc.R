#' @export
tic<-function(...)
{
  assign("elapsedTime", proc.time()[3], envir = .sporEnv)
  invisible()
}

#' @export
toc<-function (echo = TRUE,
               ...)
{
  prevTime <- get("elapsedTime", envir = .sporEnv)
  diffTimeSecs <- proc.time()[3] - prevTime
  if (echo) {
    cat(sprintf("elapsed time is %f seconds", diffTimeSecs),"\n")
    return(invisible(diffTimeSecs))
  }
  else {
    return(diffTimeSecs)
  }
}
