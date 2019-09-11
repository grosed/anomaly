#' Detection of multivariate anomalous segments using PASS.
#'
#'
#' Implements the PASS (Proportion Adaptive Segment Selection) procedure of Jeng et al. (2012). PASS uses a higher criticism statistic to pool the information about the 
#' presence or absence of a collective anomaly across the components. It uses Circular Binary Segmentation to detect multiple collective anomalies.
#' 
#' 
#' @param x An n x m real matrix representing n observations of m variates.
#' @param alpha A positive integer > 0. This value is used to stabilise the higher criticism based test statistic used by PASS leading to a better finite sample familywise error rate. 
#' Anomalies affecting fewer than alpha components will however in all likelihood escape detection.
#' @param lambda A positive real value setting the threshold value for the familywise Type 1 error. The default value
#' is \eqn{(1.1 {\rm log}(n \times Lmax) +2 {\rm log}({\rm log}(m))) / \sqrt{{\rm log}({\rm log}(m))}}. 
#' @param Lmax A positive integer (\code{Lmax} > 0) corresponding to the maximum segment length. The default value is 10.
#' @param Lmin A positive integer (\code{Lmax} >= \code{Lmin} > 0) corresponding to the minimum segment length. The default value is 1. 
#' @param transform A function used to transform the data prior to analysis. The default value is to scale the data using the median and the median absolute deviation.
#'
#' @return An S4 object of type \code{.pass.class} containing the data \code{X}, procedure parameter values, and the results.
#' 
#' @references \insertRef{10.1093/biomet/ass059}{anomaly}
#' 
#' @examples
#' library(anomaly)
#' # generate some multivariate data
#' set.seed(0)
#' sim.data<-simulate(n=500,p=200,mu=2,locations=c(100,200,300),
#'                    duration=6,proportions=c(0.04,0.06,0.08))
#' res<-pass(sim.data)
#' summary(res)
#' plot(res,variate_names=FALSE)
#'
#' @export

pass<-function(x,alpha=2,lambda=NULL,Lmax=10,Lmin=1,transform=robustscale)
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
        stop("Lmax must be greater than Lmin")
    }
    if(!(is_whole_number(alpha) && is_non_negative(alpha)))
    {
        stop("alpha must be a non-negative whole number")
    }
    if(is.null(lambda))
    {
        n_rows<-dim(x)[1]
        n_cols<-dim(x)[2]
        lambda<-(3.0*log(n_rows*Lmax) + 2*log(log(n_cols)))/sqrt(2*log(log(n_cols)))
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
