#' Detection of multivariate anomalous segments using PASS.
#'
#'
#' Implements the PASS (Proportion Adaptive Segment Selection) procedure of Jeng et al. (2012). PASS uses a higher criticism statistic to pool the information about the 
#' presence or absence of a collective anomaly across the components. It uses Circular Binary Segmentation to detect multiple collective anomalies.
#' 
#' 
#' @param x A numeric matrix with n rows and p columns containing the data which is to be inspected. The time series data classes ts, xts, and zoo are also supported.
#' @param alpha A positive integer > 0. This value is used to stabilise the higher criticism based test statistic used by PASS leading to a better finite sample familywise error rate. 
#' Anomalies affecting fewer than alpha components will however in all likelihood escape detection. The default is 2.
#' @param lambda A positive real value setting the threshold value for the familywise Type 1 error. The default value
#' is \eqn{(1.1 {\rm log}(n \times max\_seg\_len) +2 {\rm log}({\rm log}(p))) / \sqrt{{\rm log}({\rm log}(p))}}. 
#' @param max_seg_len A positive integer (\code{max_seg_len} > 0) corresponding to the maximum segment length. This parameter corresponds to Lmax in Jeng et al. (2012). The default value is 10. 
#' @param min_seg_len A positive integer (\code{max_seg_len} >= \code{min_seg_len} > 0) corresponding to the minimum segment length. This parameter corresponds to Lmin in Jeng et al. (2012).
#' The default value is 1. 
#'
#' @return An instance of an S4 object of type \code{.pass.class} containing the data \code{X}, procedure parameter values, and the results.
#' 
#' @references \insertRef{10.1093/biomet/ass059}{anomaly}
#' 
#' @examples
#' library(anomaly)
#' # generate some multivariate data
#' data(simulated)
#' res<-pass(sim.data)
#' summary(res)
#' plot(res,variate_names=TRUE)
#'
#' @export

pass<-function(x,alpha=2,lambda=NULL,max_seg_len=10,min_seg_len=1)
{
    # reflect renamed variable
    Lmax <- max_seg_len
    Lmin <- min_seg_len
    # check the data
    x<-to_array(x)
    Xi<-x
    # check dimensions,types and values
    if(!check.alpha(Lmax))
    {
        stop("max_seg_len must be a positive integer")
    }
    if(!check.alpha(Lmin))
    {
        stop("min_seg_len must be a positive integer")
    }
    if(!(Lmax >= Lmin))
    {
        stop("max_seg_len must be greater than min_seg_len")
    }
    if(!check.alpha(alpha))
    {
        stop("alpha must be a positive integer")
    }
    ## LB ADDED - if number of variates < default alpha (2) -> alpha = 1
    if(dim(x)[2] < alpha){
      alpha <- 1
    }
    if(is.null(lambda))
    {
      n_rows<-dim(x)[1]
      n_cols<-dim(x)[2]
      if (n_cols < 3){
        lambda <- 10
        message <- paste("The data has only N =", n_cols, "variates.", "Since the value of",  "\U03BB", "is based on asymptotic theory as the number of variates, N tends to infinity we suggest using simulations to determine a data-driven threshold to control the number of overselections. A default value of", "\U03BB = 10 has been used here.")
        warning(message)
      }
      else{
        lambda<-(3.0*log(n_rows*Lmax) + 2*log(log(n_cols)))/sqrt(2*log(log(n_cols)))
      }
    }
    else{
      if(!check.lambda(lambda))
      {
        stop("lambda must be a positive real number")
      }
    }
    pass.results<-tryCatch(
    {
        marshall_pass(Map(function(j) unlist(Xi[,j]),1:ncol(Xi)),Lmax,Lmin,alpha,lambda)
    },
    error = function(e) {print(e$message);stop();}
    )
    # post process results
    if(length(pass.results) == 0) # no anomalies
    {
        results<-data.frame("start"=integer(0), "end"=integer(0), "xstar"=integer(0))
        results.S4<-pass.class(Xi,results,Lmax,Lmin,alpha,lambda)
        return(results.S4)
    }
    else
    {
        results<-Reduce(rbind,pass.results,data.frame())
    }
    
    colnames(results)<-c("start","end","xstar")
    results$start<-results$start+1
    results$end<-results$end+1
    results.S4<-pass.class(Xi,results,Lmax,Lmin,alpha,lambda)
    return(results.S4)

    #colnames(results)<-c("left","right","xstar")
    #results$left<-results$left+1
    #results$right<-results$right+1
    #results.S4<-pass.class(Xi,results,Lmax,Lmin,alpha,lambda)
    #return(results.S4)
}


check.alpha <- function(input){
  
  res <- (length(input) == 1) && (is.numeric(input)) && (!is.nan(input)) && (input > 0) && (!is.infinite(input)) && (input%%1 == 0)
  return(res)
  
}

check.lambda <- function(input){
  
  res <- (length(input) == 1) && (is.numeric(input)) && (!is.nan(input)) && (input > 0)
  return(res)
  
}
