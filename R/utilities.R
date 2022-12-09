## These are common function used in the call to capa, bard, pass etc bbuut not exported

## Utility function to coerce data to a matrix and check it has only finite values
to_array<-function(X)
{
  if(is.ts(X) || is.xts(X) || is.zoo(X)){ X<-unclass(X) }## TODO - check constant timestamps

  if(is.data.frame(X)){ X<-data.matrix(X) }

  if(is.vector(X)){ X<-as.matrix(X) }

  if(!is.matrix(X)){ stop("Cannot convert input to a matrix") }

  if(!all(is.finite(X))){ stop("All values must be numeric and finite") }

  return(X)
}

## check single integer in range
int_in_range <- function(x,lwr=-Inf,upr=Inf,lbl="x"){
    x <- as.integer(x)
    
    if( length(x)!=1 | !is.finite(x) ){
        stop( paste(lbl,"must be a single finite integer") )
    }
    if( x<lwr | x>upr ){
        stop( paste(lbl,"must be in (",lwr,",",upr,")") )
    }
    return(x)
}


# utility function to process commen features of summary and show
summary_show_common<-function(object,epoch=nrow(object@data))
{
    if(epoch < 0)
    {
        stop("epoch should be a positive integer")
    }
    if(epoch > nrow(object@data))
    {
        stop("epoch cannot be greater than the number of observations in the data")
    }
    if(dim(object@data)[2] == 1)
    {
       cat("Univariate ",sep="")
    }
    else
    {
       cat("Multivariate ",sep="")	
    }
    cat("CAPA detecting changes in ",sep="")
    if(object@type == "meanvar")
    {
        cat("mean and variance.","\n",sep="") 
    }
    if(object@type %in% c("mean","robustmean"))
    {
        cat("mean.","\n",sep="") 
    }
    cat("observations = ",dim(object@data)[1],sep="")
    cat("\n",sep="")
    if(dim(object@data)[2] != 1)
    {
       cat("variates = ",dim(object@data)[2],"\n",sep="")
    }
    cat("minimum segment length = ",object@min_seg_len,'\n',sep="")
    cat("maximum segment length = ",object@max_seg_len,'\n',sep="")
    if(dim(object@data)[2] != 1)
    {
       cat("maximum lag = ",object@max_lag[1],'\n',sep="")
    }
    if(epoch != nrow(object@data))
    {
       cat("epoch = ",epoch,"\n",sep="")
    }
}


