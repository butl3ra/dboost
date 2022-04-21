#' @export
load_ff_data<-function( dir = "/Users/WOPR/Dropbox (Personal)/workspace_phd/data/",
                        x_file = 'vix_features.csv',#ff_5_prices.csv
                        y_file = 'stock_prices.csv',
                        dates = '1990::2020-10',
                        freq = 'weeks')
{

  library('xts')

  x = read.csv(paste0(dir,x_file),header=T)
  y = read.csv(paste0(dir,y_file),header=T)

  x = as.xts(x[,-1],order.by = as.Date(x[,1]))
  y = as.xts(y[,-1],order.by = as.Date(y[,1]))

  # -- cleanup:
  x[] = ifna_prev_mat(x)
  y[] = ifna_prev_mat(y)

  date_int = intersect(as.character(index(x)),as.character(index(y)))
  date_int = as.Date(date_int)
  x = x[date_int,]
  y = y[date_int,]

  x = x[dates,]
  y = y[dates,]

  idx = endpoints(x,freq)
  x = x[idx,]
  y = y[idx,]

  # --- make y returns:
  y = roc(y)
  y = y[-1,]

  # --- make x returns:
  if(x_file == "ff_5_prices.csv"){
    x = roc(x)
  }
  x = x[-1,]

  data = list(x = x,
              y = y)
  return(data)
}




