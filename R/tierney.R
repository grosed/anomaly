
tierney<-function(X,burnin=10)
{
    if(is.vector(X))
    {
        ests<-sequential_ests(X,burnin)
        return((X-ests[[1]])/ests[[2]])
    }
    else if(is.matrix(X))
    {
        return(Reduce(cbind,Map(function(i) array(tierney(X[,i],burnin),c(nrow(X),1)),1:ncol(X))))  
    }
    else
    {
        # incorrect type - throw an exception
    }   
}


sequential_ests<-function(data,burnin = 10)
{
  
  # Error Traps
  ## UPDATED VERSION - LB - sequential_estimates
  # Traps for burnin
  
  if(!(is.numeric(burnin))){
    stop("burnin has to be numeric.")
  }
  
  if (length(burnin) == 0 ){
    stop("input for burnin has length 0.")
  }
  
  if (length(burnin) > 1 ){
    burnin = burnin[1]
    warning("length of input for burnin exceeds 1. Only the first entry was kept.")
  }
  
  if(!(is.numeric(burnin))){
    stop("burnin has to be numeric.")
  }
  
  if((is.infinite(burnin))){
    stop("input for burnin is infinite.")
  }
  
  if((is.nan(burnin))){
    stop("input for burnin is NaN.")
  }
  
  if(!(is.integer(burnin))){
    burnin = as.integer(burnin)
    warning("non-integer input for burnin. The input was converted to an integer using as.integer.")
  }
  
  if (burnin < 10){
    stop("argument for burnin needs to be at least 10.")
  }
  
  # Traps for data
  
  if(!(is.numeric(data))){
    stop("data has to be numeric.")
  }
  
  data = as.vector(data)
  
  if(length(data) <= burnin){
    stop("length of data less than burnin.")
  }
  
  if(sum(is.na(data)) > 0){
    stop("Input for data contains NAs.")
  }
  
  if(sum(is.nan(data)) > 0){
    stop("Input for data contains NaNs.")
  }
  
  if(sum(is.infinite(data)) > 0){
    stop("Input for data contains Infinite entries.")
  }
  
  
  # Return 
  
  # initial estimates done in R
  slq = quantile(data[1:burnin], probs = 0.25)
  smed = quantile(data[1:burnin], probs = 0.5)
  suq = quantile(data[1:burnin], probs = 0.75)
  
  scale = IQR(data[1:burnin])
  c = (scale/burnin) * sum( (1:burnin)^(-0.5) )
  
  flq = (1/(2*c*burnin)) * max( sum( abs(data[1:burnin] - slq) <= c ), 1)
  fmed = (1/(2*c*burnin)) * max( sum( abs(data[1:burnin] - smed) <= c ), 1)
  fuq = (1/(2*c*burnin)) * max( sum( abs(data[1:burnin] - suq) <= c ), 1)
  
  n = length(data)
  return(marshall_sequential_ests(data, n, burnin, slq, flq, smed, fmed, suq, fuq))
  
} 
