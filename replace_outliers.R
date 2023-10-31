replace_outliers <- function(x, na.rm = TRUE, ...) {
  # first and third quartile not including NAs 
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...) 
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  x<-y
  # get rid of any NAs
  # x[!is.na(x)]
  
} 

