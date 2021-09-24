#' Transforms the data X to account for autocorrelation using a moving window and a burn-in.
#'
#' @name moving_ac_corrected
#'
#' @description Transforms the data X by centring and scaling using \eqn{X_{ij}^{'} = \frac{X_{ij}-\mu_{ij}}{k_{i} \sigma_{ij}}} where \eqn{\mu_{ij}} and \eqn{\sigma_{ij}} are 
#' robust estimates for location and scale based on the median and the median absolute deviation of each variate (column) \eqn{X_{i}} of X calculated on a moving window centred at j.  
#' The scaling \eqn{k_{i} = \surd{\left( \frac{1+\phi_{i}}{1-\phi_{i}} \right)}} is a robust estimate for the autocorrelation at lag 1 calculated on an initial (burn-in) segment
#' of the data where \eqn{\phi_{i}} is calculated using a robust estimate for the autocorrelation of the burn-in segment.
#'
#' @param X A numeric matrix containing the potentially multivariate data to be transformed. Each column corresponds to a component and each row to an observation.
#' The time series data classes ts, xts, and zoo are also supported.
#' @param burnin A positive integer indicating the initial length of the data used to determine the value of \eqn{\phi_{i}}.  
#' @param window_size A positive integer indication the length of the moving window.
#'
#' @return A numeric matrix of the same dimension as X containing the transformed data.
#'
#' @export
moving_ac_corrected<-function(X,burnin,window_size)
{
    f<-function(x)
    {
       movmed<-runner(x,k=window_size,f=median)
       movmad<-runner(x,k=window_size,f=mad)
       movmad[1:2]<-movmad[3]
       burnin_phi<-covRob(matrix(c(x[2:burnin],x[1:(burnin-1)]),ncol=2),corr=TRUE,estim="mcd")$cov[1,2]
       return(sqrt((1-burnin_phi)/(1+burnin_phi))*(x-movmed)/movmad)
    }
    X %<>% tibble %>% mutate(across(.fns=f)) %>% as.matrix
    return(X)
}


