#' @export
reduce_loss<-function(loss,
                      reduce = 'mean')
{
  if(missing(reduce)){
    reduce = NULL
  }
  if(!is.null(reduce)){
    reduce = tolower(reduce)
    if(reduce == 'mean'){
      loss = mean(loss)
    }
    else if(reduce == 'sum'){
      loss = sum(loss)
    }
    else if(reduce == 'max'){
      loss = max(loss)
    }
  }
  return(loss)
}
