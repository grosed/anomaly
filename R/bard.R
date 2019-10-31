

.bard.class<-setClass("bard.class",representation(data="array",
                                                  p_N="numeric",
                                                  p_A="numeric",
                                                  k_N="integer",
                                                  k_A="integer",
                                                  pi_N="numeric",
                                                  alpha="numeric",
                                                  paffected="numeric",
                                                  lower="numeric",
                                                  upper="numeric",
                                                  h="numeric",
                                                  Rs="list"))

bard.class<-function(data,p_N,p_A,k_N,k_A,pi_N,alpha,paffected,lower,upper,h,Rs,...)
{
    .bard.class(data=data,
                p_N=p_N,
                p_A=p_A,
                k_N=k_N,
                k_A=k_A,
                pi_N=pi_N,
                alpha=alpha,
                paffected=paffected,
                lower=lower,
                upper=upper,
                h=h,
                Rs=Rs)
}




#' Detection of multivariate anomalous segments using BARD.
#'
#' Implements the BARD (Bayesian Abnormal Region Detector) procedure of Bardwell and Fearnhead (2017). BARD is a fully Bayesian inference procedure which is able to give
#' measures of uncertainty about the number and location of abnormal regions. It uses flexible prior distributions on the lengths of normal and abnormal regions as well as
#' a prior distribution on the mean of the affected variates.
#' 
#' @param x - An $n$ by $p$ real matrix representing n observations of p variates. Each variate is scaled by BARD using the median and the median absolute deviation. This
#' can be changed using the \code{transform} parameter. 
#' @param p_N - Probability of success in each trial for the Negative Binomial distribution for the length of normal segments.
#' @param k_N - Dispersion parameter for the Negative Binomial distribution for the length of normal segments.
#' @param p_A - Probability of success in each trial for the Negative Binomial distribution for the length of abnormal segments.
#' @param k_A - Dispersion parameter for the Negative Binomial distribution for the length of abnormal segments.
#' @param pi_N - Probability that an abnormal segment is followed by a normal segment.
#' @param alpha - Threshold used to control the resampling in the approximation of the posterior distribution at each time step.
#' @param paffected - Proportion of the series believed to be affected by an abnormal segment.
#' @param lower - The lower limit of the prior uniform distribution for \eqn{mu}.
#' @param upper - The upper limit of the prior uniform distribution for \eqn{mu}.
#' @param h - The step size in the numerical integration used to find the marginal likelihood. The quadrature points are located from lower to upper in steps of h.  
#' @param transform - A function used to transform the data prior to analysis. The default value is to scale the data using the median and the median absolute deviation.
#' 
#' @return An S4 object of type \code{.bard.class} containing the data \code{x}, procedure parameter values, and the results.
#'
#' @references  \insertRef{bardwell2017}{anomaly}
#'
#' @examples
#' 
#' library(anomaly)
#' set.seed(0)
#' sim.data<-simulate(n=500,p=200,mu=2,locations=c(100,200,300),
#'                    duration=6,proportions=c(0.04,0.06,0.08))
#' # parameters
#' p_N<-0.05
#' p_A<-0.5
#' k_N<-10
#' k_A<-10
#' pi_N<-0.9
#' alpha<-0.0001
#' paffected<-10/200
#' lower<-0.5
#' upper<-1.5
#' h<-0.25
#' # run bard
#' res<-bard(sim.data,p_N,p_A,k_N,k_A,pi_N,alpha,paffected,lower,upper,h)
#' sampler(res)
#'
#' @export
bard<-function(x,p_N,p_A,k_N,k_A,pi_N,alpha,paffected,lower,upper,h,transform=robustscale)
{
    # check the data
    data<-as.array(as.matrix(x))
    if(!is_array(data))
    {
        stop("cannot convert data to an array")
    }
    if(!all(is_not_na(data)))
    {
        stop("x contains NA values")
    }
    if(!all(is_not_null(data)))
    {
        stop("x contains NULL values")
    }
    if(!is_numeric(data))
    {
        stop("x must be of type numeric")
    }

    # transform the data
    data<-transform(data)
    
    # now convert the data to a list of vectors for marshalling to Rcpp
    data<-Map(function(i) unlist(data[i,]),1:nrow(data))

    # check p_N
    if(!is_numeric(p_N))
    {
        stop("p_N must be of type numeric")
    }
    if(length(p_N) != 1)
    {
        stop("p_N must be a single numeric value")
    }
    if(p_N < 0.0 || p_N > 1)
    {
        stop("p_N must be in the range [0,1]")
    }

    # check p_A
    if(!is_numeric(p_A))
    {
        stop("p_A must be of type numeric")
    }
    if(length(p_A) != 1)
    {
        stop("p_A must be a single numeric value")
    }
    if(p_A < 0.0 || p_A > 1)
    {
        stop("p_A must be in the range [0,1]")
    }

    # check pi_N
    if(!is_numeric(pi_N))
    {
        stop("pi_N must be of type numeric")
    }
    if(length(pi_N) != 1)
    {
        stop("pi_N must be a single numeric value")
    }
    if(pi_N < 0.0 || pi_N > 1)
    {
        stop("pi_N must be in the range [0,1]")
    }

    # check alpha
    if(!is_numeric(alpha))
    {
        stop("alpha must be of type numeric")
    }
    if(length(alpha) != 1)
    {
        stop("alpha must be a single numeric value")
    }
    if(alpha < 0.0 || alpha > 1)
    {
        stop("alpha must be in the range [0,1]")
    }

    # check paffected
    if(!is_numeric(paffected))
    {
        stop("paffected must be of type numeric")
    }
    if(length(paffected) != 1)
    {
        stop("paffected must be a single numeric value")
    }
    if(paffected < 0.0 || paffected > 1)
    {
        stop("paffected must be in the range [0,1]")
    }

    # check k_A
    if(!is_numeric(k_A))
    {
        stop("k_A must be of type numeric")
    }
    if(length(k_A) != 1)
    {
        stop("k_A must be a single numeric value")
    }
    if(k_A < 0.0)
    {
        stop("k_A must be positive")
    }
    if(k_A - as.integer(k_A) > 0)
    {
        warning("k_A should be an integer - user value will rounded down")
    }

    # check k_N
    if(!is_numeric(k_N))
    {
        stop("k_N must be of type numeric")
    }
    if(length(k_N) != 1)
    {
        stop("k_N must be a single numeric value")
    }
    if(k_N < 0.0)
    {
        stop("k_N must be positive")
    }
    if(k_N - as.integer(k_N) > 0)
    {
        warning("k_N should be an integer - user value will rounded down")
    }
    
    # check lower
    if(!is_numeric(lower))
    {
        stop("lower must be of type numeric")
    }
    if(length(lower) != 1)
    {
        stop("lower must be a single numeric value")
    }

    # check upper
    if(!is_numeric(upper))
    {
        stop("upper must be of type numeric")
    }
    if(length(upper) != 1)
    {
        stop("upper must be a single numeric value")
    }

    # check relationaship between upper an lower
    if(lower > upper)
    {
        stop("value of upper should be greater than lower")
    }

    # check h
    if(!is_numeric(h))
    {
        stop("h must be of type numeric")
    }
    if(length(h) != 1)
    {
        stop("h must be a single numeric value")
    }
    # check relationship between h and upper and lower
    if(h > (upper - lower))
    {
        stop("h value to large : (h <= upper -lower)")
    }

    
    # set boost seed (used by c++) to sink with R random number generator
    seed = as.integer(runif(1,0,.Machine$integer.max))

    # dispatch    
    Rs<-marshall_bard(data,p_N,p_A,k_N,k_A,pi_N,alpha,paffected,lower,upper,h,seed)

    # put the data back into an array
    data<-t(array(unlist(data),c(length(data[[1]]),length(data))))
    
    return(bard.class(data=data,
                      p_N=p_N,
                      p_A=p_A,
                      k_N=as.integer(k_N),
                      k_A=as.integer(k_A),
                      pi_N=pi_N,
                      alpha=alpha,
                      paffected=paffected,
                      lower=lower,
                      upper=upper,
                      h=h,
                      Rs=Rs)
           )
    
}



#### helper functions for post processing

get.probvec <- function( filtering , params , num_draws=1000 )
{
  logprobs <- filtering$weights
  cpt.locs <- filtering$locations
  types <- filtering$type
  n <- length(filtering$weights)
  vec<-numeric(n)
  prob.states <- matrix(nrow=num_draws,ncol=n)
  
  for (i in 1:num_draws){
  
    f <- draw.from.post(logprobs, cpt.locs, types, params)
    segs <- f$draws
    states <- f$states
  
    # fill vec (hist of chpts)
    vec[f$draws] <- vec[f$draws] + 1
  
    # probs of being N or A
    prob.states[i,] <- get.states(segs,states,n)
    
  }

  return( apply(prob.states,2,sum)/num_draws )

}


# function to get a vector of states for a drawn value frin filtering posterior # 
# segs is vector of locations of segments
# states vector 
get.states <- function( segs , states, n){
  
  state.vec <- numeric(n)
  state.vec[ segs[1]:segs[2] ] <- states[1]
  for (i in 2:( length(segs) - 1 ) ){
    state.vec[ ( segs[i]-1 ):segs[i+1] ] <- states[i]
  }

  return(state.vec)
  
}

format_output = function(R){
  
  n = length(R)
  weights = vector("list", n) 
  location = vector("list", n)
  type = vector("list", n)
  for (i in 1:n){
    
    m = length(R[[i]])
    tmpweights = numeric(2*m)
    tmplocs = numeric(2*m)
    tmptypes = numeric(2*m)
    for (j in 1:m){
      triple = R[[i]][[j]]
      tmpweights[(2*j-1):(2*j)] = triple[1:2] 
      tmplocs[(2*j-1):(2*j)] = as.integer(triple[3])
      tmptypes[(2*j-1):(2*j)] = c(0,1) 
    }
    weights[[i]] = tmpweights[tmpweights > -Inf]
    location[[i]] = tmplocs[tmpweights > -Inf]
    type[[i]] = tmptypes[tmpweights > -Inf]
    
  }
  
  filtering = list("weights"=weights, "locations"=location, "type"=type)
  return(filtering)
  
}


# apply loss function to get a segmentation returns vector with 0's and 1's
# 0 means in a normal segment , 1 is abnormal
# apply loss with parameter gamma

loss <- function( gamma, probvec ){

  p.gamma <- 1/(1+gamma)
  probvec[probvec < p.gamma ] <- 0
  probvec[probvec>0] <- 1
  return(probvec)
  
}

# draw one sample of chpts and states from the posterior
draw.from.post <- function( logprobs , cpt.locs , types , params ){ 

  k_N = params[1]
  p_N = params[2]
  k_A = params[3]
  p_A = params[4]
  pi_N = params[5]
  pi_A = params[6]
  
  n <- length(logprobs)
  l.probs <- logprobs[[n]]
  seg.locs <- cpt.locs[[n]]
  seg.types <- types[[n]]

  ind <- 1:length(l.probs)
  a <- sample( ind , size=1 , replace = TRUE, exp( l.probs ) )

  # state amd location
  curr.state <- seg.types[a]
  t <- seg.locs[a]

  STATES <- curr.state
  DRAW <- t
  
  while (t > 1){

    if(curr.state==0){
    
      l.probs <- logprobs[[t]][ types[[t]] == 1 ] 
      i <- cpt.locs[[t]][ types[[t]] ==1 ]
      back.dens <- log(pi_N) + dnbinom(t-i-1,k_A,p_A,log=TRUE) - pnbinom(t-i-2,k_A,p_A, lower.tail=FALSE , log.p=TRUE)
      l.probs <- l.probs + back.dens  
    
      ind <- 1:length(l.probs)
      a <- sample( ind , size=1 , replace = TRUE, exp( l.probs ) )
    
      # state amd location
      curr.state <- 1
      t <- i[a]
      STATES <- c(STATES,curr.state)
      DRAW <- c(DRAW,t)
    }
    else{
    
      # currently in abnormal state poss transitions to normal or abnormal
      i.N <- cpt.locs[[t]][ types[[t]] == 0 ]
      i.A <- cpt.locs[[t]][ types[[t]] == 1 ]
    
      l.probsA <- logprobs[[t]][  types[[t]] == 1 ] 
      back.densA <- log(pi_A) + dnbinom(t-i.A-1,k_A,p_A,log=TRUE) - pnbinom(t-i.A-2,k_A,p_A, lower.tail=FALSE , log.p=TRUE)
      l.probsA <- l.probsA + back.densA
      # normal points
      l.probsN <- logprobs[[t]][  types[[t]] == 0 ] 
      back.densN <- dnbinom( t-i.N-1 , k_N , p_N , log=TRUE ) - pnbinom( t-i.N-2 , k_N , p_N , lower.tail=FALSE , log.p=TRUE )
      l.probsN <- l.probsN + back.densN
      # now normalise
      i.all <- c( i.N , i.A )
      t.all <- c( rep(0,length(i.N)) , rep(1,length(i.A)) )
      l.probs <- c( l.probsN , l.probsA )
      c <- max(l.probs)
      l.probs.norm <- l.probs - ( c + log( sum( exp( l.probs - c ) ) ) ) 
    
      ind <- 1:length(l.probs.norm)
      a <- sample( ind , size=1 , replace = TRUE, exp( l.probs.norm ) )
    
      # state amd location
      curr.state <- t.all[a]
      t <- i.all[a]
      STATES <- c(STATES,curr.state)
      DRAW <- c(DRAW,t)
      }
  
   }
  
  DRAW[DRAW==0] <- 1
  newlist <- list("draws"=c(n,DRAW),"states"=STATES)
  return(newlist)

}

## TODO - to be renamed and a S4 class returned with relevant stuff in it
#post_process_bard<-function(bard.result)
#{
#   # creating null entries for ggplot global variables so as to pass CRAN checks 
#    time<-probability<-NULL
#    R<-bard.result@Rs
#    params<-c(bard.result@k_N,bard.result@p_N,bard.result@k_A,bard.result@p_A,bard.result@pi_N,1-bard.result@pi_N)
#    filtering<-format_output(R)
#    # vector of probabilities
#    pvec<-get.probvec(filtering, params, no.draws = 1000)
#    df<-data.frame("time"=seq(1,length(pvec)),"probability"=pvec)
#    p<-ggplot(data=df,aes(x=time,y=probability))
#    p<-p+geom_line()
#    return(p)
#}




#' Post processing of BARD results.
#'
#' Determines the locations of anomalous sections from the results produced by BARD.
#'
#' @param bard_result - An instance of the S4 class \code{.bard.class} containing a result returned by the \code{bard} function. 
#' @param gamma - Parameter of loss function: cost of incorrectly assigning an abnormal point as being normal (false negative).
#' @param num_draws - Number of samples to draw from the posterior distribution. 
#' 
#' @return A dataframe containing the start, end, and log marginal likelihood for each anomalous segment. 
#'
#' @references  \insertRef{bardwell2017}{anomaly}
#'
#' @examples
#' library(anomaly)
#' set.seed(0)
#' sim.data<-simulate(n=500,p=200,mu=2,locations=c(100,200,300),
#' duration=6,proportions=c(0.04,0.06,0.08))
#' # parameters
#' p_N<-0.05
#' p_A<-0.5
#' k_N<-10
#' k_A<-10
#' pi_N<-0.9
#' alpha<-0.0001
#' paffected<-10/200
#' lower<-0.5
#' upper<-1.5
#' h<-0.25
#' # run bard
#' res<-bard(sim.data,p_N,p_A,k_N,k_A,pi_N,alpha,paffected,lower,upper,h)
#' # sample 
#' sampler(res)
#' # effect of sampling parameters
#' sampler(res,gamma=10,num_draws=1000)
#' sampler(res,gamma=10,num_draws=10)
#' 
#' @export
sampler<-function(bard_result, gamma = 1/3, num_draws = 1000)
{
    P.A <- function(data, s, t, mu_seq, p)
    {
  
        S = rbind( rep(0,dim(data)[2]) , apply(data , 2 , cumsum) )
        S_2 = cumsum( c( 0 , rowSums(data^2) ) )
        N = dim(data)[2]
    
        # prior value mu_dens
        mu_dens <- 1/( tail(mu_seq,1) - mu_seq[1] )
        
        # width of rectangle
        mu_wid <- diff(mu_seq)[1]
        vec <- numeric(length(mu_seq))
        
        # evaluate at each point of grid
        # do this as typically smaller than dimension
        # evaluating log of quantity 
        for (k in 1:length(mu_seq)){
            vec[k] <- N*log(1-p) + sum( log( 1 + exp( mu_seq[k] * ( S[(t+1),] - S[s,]  -  mu_seq[k] * (t-s+1)/2 ) + log(p) - log(1-p) ) ) )
        }
        # finding sum of logs -- for numerical instability
        cmax <- max( vec )
        marg.like <- cmax + log( sum( exp( vec - cmax ) ) ) + log(mu_dens) + log(mu_wid)  
        return(marg.like)   
    }
    
   # creating null entries for ggplot global variables so as to pass CRAN checks 
    time<-probability<-NULL
    R<-bard_result@Rs
    params<-c(bard_result@k_N,bard_result@p_N,bard_result@k_A,bard_result@p_A,bard_result@pi_N,1-bard_result@pi_N)
    filtering<-format_output(R)
    # vector of probabilities
    pvec<-get.probvec(filtering, params, num_draws = num_draws)
    # get segmentation using loss function
    segmentation <- loss(gamma, pvec)
    # stuff to get marginal likelihood
    lower = bard_result@lower
    upper = bard_result@upper
    h = bard_result@h
    mu_seq = seq(lower, upper, by = h)
    paffected = bard_result@paffected
    data = bard_result@data
    # put into data frame
    df = data.frame("start"=NA, "end"=NA, "LogMargLike"=NA)
    welem = which(segmentation == 1)
    wdiff = c(0, which(diff(welem) != 1), length(welem))
    for (i in 1:(length(wdiff)-1) ){
      start = welem[(wdiff[i] + 1)]
      end = welem[wdiff[(i+1)]]
      ml = P.A(data, start, end, mu_seq, paffected)
      tempdf = data.frame("start"=start,
                          "end"=end,
                          "LogMargLike"=ml)
      df = rbind(df, tempdf)
    }
    df = df[-1,]
    df = df[order(df$LogMargLike, decreasing = T), ]                                                                                                                              
    return(df)
    #df<-data.frame("time"=seq(1,length(pvec)),"probability"=pvec)
    #p<-ggplot(data=df,aes(x=time,y=probability))
    #p<-p+geom_line()
    #return(p)
}






##### BARD example to use in documentation

#library(anomaly)
#data(simulated)
#
## parameters
#p_N<-0.05
#p_A<-0.5
#k_N<-10
#k_A<-10
#pi_N<-0.9
#alpha<-0.0001
#paffected<-10/200
#lower<-0.5
#upper<-1.5
#h<-0.25
#
## run bard
#res<-bard(sim.data,p_N,p_A,k_N,k_A,pi_N,alpha,paffected,lower,upper,h)
#p<-post_process_bard(res)
