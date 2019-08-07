#' Proportion Adaptive Segment Selection (PASS).
#'
#'
#' Implements the Proportion Adaptive Segment Selection procedure of REF HERE to (what does it do ???)
#' 
#' 
#' @param x An n x m real matrix representing n observations of m variates.
#' @param alpha A positive integer > 1. This value is used to stabilise the PASS procedure and control the familywise error for finite samples. 
#' @param lambda A positive real value setting the threshold value for the familywise Type 1 error. The default value is 15.0. 
#' @param Lmax A positive integer (\code{Lmax} > 1) corresponding to the maximum segment length. The default value is 10.
#' @param Lmin A positive integer (\code{Lmax} > \code{Lmin} > 1) corresponding to the minimum segment length. The default value is 0. . 
#' @param transform A function used to transform the data prior to analysis. The default value is to scale the data using the median and the median absolute deviation.
#'
#' @return An S4 object of type \code{.pass.class} containing the data \code{Xi}, procedure parameter values and the results.
#' 
#' @references \insertRef{10.1093/biomet/ass059}{anomaly}
#' 
#' @examples
#' library(anomaly)
#' data(simulated)
#' res<-pass(sim.data)
#' summary(res)
#' plot(res,variate_names=FALSE)
#'
#' @export

pass<-function(x,alpha=2,lambda=15.0,Lmax=10,Lmin=0,transform=robustscale)
{
    # check the data
    x<-as.array(as.matrix(x))
    if(!is_array(x))
    {
        stop("cannot convert x to an array")
    }
    if(!all(is_not_na(x)))
    {
        stop("x contains NA values")
    }
    if(!all(is_not_null(x)))
    {
        stop("x contains NULL values")
    }
    if(!is_numeric(x))
    {
        stop("x must be of type numeric")
    }
    # transform data
    Xi<-transform(x)
    # check dimensions,types and values
    if(!(is_whole_number(Lmax) && is_positive(Lmax)))
    {
        stop("Lmax must be a positive whole number")
    }
    if(!(is_whole_number(Lmax) && is_positive(Lmax)))
    {
        stop("Lmin must be a positive whole number")
    }
    if(!is_positive(Lmax-Lmin))
    {
        stop("Lmin must be greater than Lmin")
    }
    if(!(is_whole_number(alpha) && is_non_negative(alpha)))
    {
        stop("alpha must be a non-negative whole number")
    }
    if(!(is_numeric(lambda) && is_positive(lambda)))
    {
        stop("lambda must be numeric value greater than 0")
    }
    assert_is_matrix(Xi)
    assert_is_non_empty(Xi)
    assert_all_are_real(Xi)
    if(!is_positive(nrow(Xi) - Lmax))
    {
        stop("number of rows (observations) in Xi must be greater than Lmax") 
    }
    # if the columns (variates) do not have names - give them default ones
    if(!has_colnames(Xi))
    {
        colnames(Xi)<-unlist(Map(function(i) paste("V",i,sep=""),1:ncol(Xi)))
    }
    # analyse data and catch c++ exceptions (NB ctrl-c is handled as an exception)
    pass.results<-tryCatch(
    {
        marshall_pass(Map(function(j) unlist(Xi[,j]),1:ncol(Xi)),Lmax,Lmin,alpha,lambda)
    },
    error = function(e) {e$message<-"pass stopped because of user interrupt";print(e$message);stop();}
    )
    # post process results
    if(length(pass.results) == 0) # no anomalies
    {
        results<-data.frame(NA,NA,NA)
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
