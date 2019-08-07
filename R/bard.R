

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




bard<-function(data,p_N,p_A,k_N,k_A,pi_N,alpha,paffected,lower,upper,h)
{
    # check the data
    data<-as.array(as.matrix(data))
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
