#' ac_corrected
#'
#' Transforms the data X to account for autocorrelation by centring and scaling using \eqn{X_{i}^{'} = \frac{X_{i}-\mu_{i}}{k_{i}\sigma_{i}}}. Here \eqn{\mu_{i}} and \eqn{\sigma_{i}} are robust estimates for
#' the mean and standard deviation of each variate (column) \eqn{X_{i}} of X calculated using the median and median absolute deviation and \eqn{k_{i} = \surd{\left( \frac{1+\phi_{i}}{1-\phi_{i}} \right)}} with
#' \eqn{\phi_{i}} a robust estimate for the autocorrelation. 
#' 
#' @param X A numeric matrix containing the data to be transformed.
#' 
#' @return A numeric matrix containing the transformed data. 
#' 
#' @examples
#' library(anomaly)
#' data(simulated)
#' # compare the medians of each variate and transformed variate
#' head(apply(sim.data,2,median))
#' head(apply(ac_corrected(sim.data),2,median))
#' # compare the variances of each variate and transformed variate
#' head(apply(sim.data,2,var))
#' head(apply(ac_corrected(sim.data),2,var))
#'
#' @export
ac_corrected<-function(X)
    {
        ac_corrections<-function(X)
        {
            X<-as.matrix(X)
            n<-dim(X)[1]
            m<-dim(X)[2]
            if(m == 1)
            {
                rcov<-covRob(matrix(c(X[2:n],X[1:(n-1)]),ncol=2),corr=TRUE,estim="M")
                psi<-rcov$cov[1,2]
                correction_factor<-sqrt((1-psi)/(1+psi))
                return(correction_factor)
            }
            else
            {
                return(unlist(Map(function(i) ac_corrections(as.matrix(X[,i])),1:m)))
            }
            
        }
        X<-robustscale(X)
        X<-t(ac_corrections(X)*t(X))
        return(X)
  }