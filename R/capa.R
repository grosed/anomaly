
.capa.class<-setClass("capa.class",representation(data="array",beta="array",beta_tilde="vector",min_seg_len="integer",max_seg_len="integer",max_lag="integer",type="character",
                                                  transform="function",anomaly_types="vector",anomaly_positions="vector",components="array",start_lags="array",end_lags="array"))

capa.class<-function(data,beta,beta_tilde,min_seg_len,max_seg_len,max_lag,type,
                     transform,anomaly_types,anomaly_positions,components,start_lags,end_lags,...)
{
    .capa.class(data=data,beta=beta,beta_tilde=beta_tilde,min_seg_len=min_seg_len,max_seg_len=max_seg_len,max_lag=max_lag,type=type,
                transform=transform,anomaly_types=anomaly_types,anomaly_positions=anomaly_positions,components=components,start_lags=start_lags,end_lags=end_lags,...)
}



.capa.uv.class<-setClass("capa.uv.class",contains="capa.class",representation())


capa.uv.class<-function(data,beta,beta_tilde,min_seg_len,max_seg_len,max_lag,type,
                     transform,anomaly_types,anomaly_positions,components,start_lags,end_lags,...)
{
.capa.uv.class(capa.class(data=data,beta=beta,beta_tilde=beta_tilde,min_seg_len=min_seg_len,max_seg_len=max_seg_len,max_lag=max_lag,type=type,
			transform=transform,anomaly_types=anomaly_types,anomaly_positions=anomaly_positions,components=components,start_lags=start_lags,end_lags=end_lags)
			,...)
}

.capa.mv.class<-setClass("capa.mv.class",contains="capa.class",representation())


capa.mv.class<-function(data,beta,beta_tilde,min_seg_len,max_seg_len,max_lag,type,
                     transform,anomaly_types,anomaly_positions,components,start_lags,end_lags,...)
{
.capa.mv.class(capa.class(data=data,beta=beta,beta_tilde=beta_tilde,min_seg_len=min_seg_len,max_seg_len=max_seg_len,max_lag=max_lag,type=type,
			transform=transform,anomaly_types=anomaly_types,anomaly_positions=anomaly_positions,components=components,start_lags=start_lags,end_lags=end_lags)
			,...)
}


#' Point anomaly location and strength.
#'
#' @name point_anomalies
#'
#' @description Creates a data frame containing point anomaly locations and strengths as detected by \code{\link{capa}}, \code{\link{capa.uv}} and \code{\link{capa.mv}}. 
#'
#' For an object produced by \code{\link{capa}}, \code{point_anomalies} returns a data frame with columns containing the position, variate and
#' strength of the anomaly, for observations up to and including the value of \code{epoch}.
#'
#' For an object produced by \code{\link{capa.mv}}, \code{point_anomalies} returns a data frame with columns containing the position, variate and
#' strength of the anomaly. 
#'
#' For an object produced by \code{\link{capa.uv}}, the output is a data frame  with columns containing the position and
#' strength of the anomaly. 
#' 
#' @docType methods
#'
#' @param object An S4 class produced by \code{\link{capa}}, \code{\link{capa.uv}} or \code{\link{capa.mv}}.
#' @param epoch Numerical value. Since \code{\link{capa}}, \code{\link{capa.uv}} and \code{\link{capa.mv}} are sequential algorithms, it is possible to process a subset of the data
#' up to and including a given epoch. The default value for \code{epoch}is the length of the data series.
#' 
#' @rdname point-anomaly-methods
#'
#' @aliases point-anomaly,capa.class,ANY-method
#' 
#' @seealso \code{\link{capa}},\code{\link{capa.uv}},\code{\link{capa.mv}}.
#'
#' @return A data frame. 
#' 
#' @export
if(!isGeneric("point_anomalies")) {setGeneric("point_anomalies",function(object,...) {standardGeneric("point_anomalies")})}
setMethod("point_anomalies",signature=list("capa.class"),
          function(object,epoch=dim(object@data)[1])
          {
              # get the anomalies
              anoms<-anomalies(object,epoch)
              # transform data
              data_dash<-object@transform(object@data)
              p_anoms<-Map(function(x) x[1],Filter(function(x) x[1] == x[2],anoms))
              if(length(p_anoms) == 0)
              {
                  return(data.frame("location"=NA,"variate"=NA,"strength"=NA))
              }
              return(
                  Reduce(rbind,
                         Map(
                             function(p_anom)
                             {
                                 variates<-seq(1,ncol(data_dash))[as.logical(object@components[p_anom[1],])]
                                 location<-rep(p_anom[1],length(variates))
                                 strength<-abs(data_dash[p_anom[1],variates])
                                 return(
                                     data.frame("location"=location,
                                                "variate"=variates,
                                                "strength"=strength)
                                 )
                             },
                             p_anoms)
                         )
              )
          }
          )

#' @name point_anomalies
#'
#' @docType methods
#'
#' @rdname point_anomaly-methods
#'
#' @aliases point_anomaly,capa.uv.class-method
#'
#' @export
setMethod("point_anomalies",signature=list("capa.uv.class"),
          function(object)
          {
              return(callNextMethod(object)[,c(1,3)])
          })


#' @name point_anomalies
#'
#' @docType methods
#'
#' @rdname point_anomaly-methods
#'
#' @aliases point_anomaly,capa.mv.class-method
#'
#' @export
setMethod("point_anomalies",signature=list("capa.mv.class"),
          function(object)
          {
              return(callNextMethod(object))
          })

#' Collective anomaly location, lag and mean/variance change.
#'
#' @name collective_anomalies
#'
#' @description Creates a data frame containing collective anomaly locations, lags and changes in mean and variance  as detected by \code{\link{capa}}, \code{\link{capa.uv}}
#' and \code{\link{capa.mv}}. 
#'
#' For an object produced by \code{\link{capa}}, \code{collective_anomalies} returns a data frame with columns containing the start and end position of the anomaly, the start
#' and end lag of the anomaly, the variates affected by the anomaly, and the change in mean and variance due to the anomaly. The results are calculated using data up to and including the
#' value of \code{epoch}.
#'
#' For an object produced by \code{\link{capa.mv}}, \code{collective_anomalies} returns a data frame with columns containing the start and end position of the anomaly, the start
#' and end lag of the anomaly, the variates affected by the anomaly, and the change in mean and variance due to the anomaly.
#'
#' For an object produced by \code{\link{capa.uv}}, \code{collective_anomalies} returns a data frame with columns containing the start and end position of the anomaly, the start
#' and end lag of the anomaly and the change in mean and variance due to the anomaly.
#' 
#' @docType methods
#'
#' @param object An S4 class produced by \code{\link{capa}}, \code{\link{capa.uv}} or \code{\link{capa.mv}}.
#' @param epoch Numerical value. Since \code{\link{capa}}, \code{\link{capa.uv}} and \code{\link{capa.mv}} are sequential algorithms, it is possible to process a subset of the data
#' up to and including a given epoch. The default value for \code{epoch} is the length of the data series.
#'
#' @return A data frame.
#' 
#' @rdname collective_anomalies-methods
#'
#' @aliases collective_anomalies,capa.class,ANY-method
#' 
#' @seealso \code{\link{capa}},\code{\link{capa.uv}},\code{\link{capa.mv}}. 
#'
#' @export
if(!isGeneric("collective_anomalies")) {setGeneric("collective_anomalies",function(object,...) {standardGeneric("collective_anomalies")})}
setMethod("collective_anomalies",signature=list("capa.class"),
          function(object,epoch=dim(object@data)[1])
          {
              # get the anomalies
              anoms<-anomalies(object,epoch)
              # transform data
              data_dash<-object@transform(object@data)
              c_anoms<-Filter(function(x) x[1] != x[2],anoms)
              if(length(c_anoms) == 0)
              {                      
                  return(data.frame("start"=NA,"end"=NA,"variate"=NA,"start.lag"=NA,"end.lag"=NA,"mean.change"=NA,"variance.change" = NA))
              }
              res<-Reduce(rbind,
                          Map(
                              function(c_anom)
                              {
                                  variates<-seq(1,ncol(data_dash))[as.logical(object@components[c_anom[2],])]
                                  start<-rep(c_anom[1],length(variates))
                                  end<-rep(c_anom[2],length(variates))
                                  start_lags<-object@start_lags[c_anom[2],variates]
                                  end_lags<-object@end_lags[c_anom[2],variates]
                                  return(
                                      data.frame("start"=start,
                                                 "end"=end,
                                                 "variate"=variates,
                                                 "start.lag"=start_lags,
                                                 "end.lag"=end_lags)
                                  )
                              },
                              c_anoms)
                          )
              if(object@type == "meanvar")
              {
                  changes<-data.frame(
                      Reduce(rbind,
                             Map(
                                 function(variate,start,end,start_lag,end_lag)
                                 {
                                     variance<-var(data_dash[(start+start_lag):(end-end_lag),variate])
                                     variance_change<-sqrt(variance)+1/sqrt(variance)-2
                                     mean_change<-mean(data_dash[(start+start_lag):(end-end_lag),variate])^2/sqrt(variance)
                                     return(array(c(mean_change,variance_change),c(1,2)))    
                                 },
                                 res$variate,
                                 res$start,
                                 res$end,
                                 res$start.lag,
                                 res$end.lag)
                             ),row.names=NULL
                  )
                  names(changes)<-c("mean.change","variance.change")
              }
              else if(object@type == "mean")
              {
                  changes<-data.frame(
                      Reduce(rbind,
                             Map(
                                 function(variate,start,end,start_lag,end_lag)
                                 {
                                     mean_change<-mean(data_dash[(start+start_lag):(end-end_lag),variate])^2
                                     variance_change<-mean_change*((end-end_lag)-(start+start_lag)+1) 
                                     return(array(c(mean_change,variance_change),c(1,2)))    
                                 },
                                 res$variate,
                                 res$start,
                                 res$end,
                                 res$start.lag,
                                 res$end.lag)
                             ),row.names=NULL
                  )
                  names(changes)<-c("mean.change","test.statistic")
              }
              else
              {
                  # default - no summary information produced
                  return(res)
              }
              res<-cbind(res,changes);
              return(res)
          }
          )



#' @name collective_anomalies
#'
#' @docType methods
#'
#' @rdname collective_anomalies-methods
#'
#' @aliases collective_anomalies,capa.uv.class-method
#'
#' @export
setMethod("collective_anomalies",signature=list("capa.uv.class"),
          function(object)
          {
              return(callNextMethod(object)[,c(1:2,6:7)])
          })

#' @name collective_anomalies
#'
#' @docType methods
#'
#' @rdname collective_anomalies-methods
#'
#' @aliases collective_anomalies,capa.mv.class-method
#'
#' @export
setMethod("collective_anomalies",signature=list("capa.mv.class"),
          function(object)
          {
              return(callNextMethod(object))
          })



#' Summary of collective and point anomalies.
#'
#' @name summary
#'
#' @description Summary methods for S4 objects returned by \code{\link{capa}}, \code{\link{capa.uv}}, \code{\link{capa.mv}} and \code{\link{pass}}.  The output displayed depends on
#' the type of S4 object passed to summary. For all types, the output indicates whether the data is univariate or multivariate, the number of observations in the data, and the type of change
#' being detected.
#'
#' For an object produced by \code{\link{capa}}, \code{summary} displays the value of \code{epoch} and two tables showing
#' the location of the collective and point anomalies and the variates that they affect. The table of point anomalies contains three columns showing the location, variate
#' and strength for each point anomaly. The table of collective anomalies contains seven columns showing the start and end position of the anomaly, the start and end lag for
#' each anomaly, the variates affected by each anomaly, and the change in mean and variance for the anomaly.
#'
#' For an object produced by \code{\link{capa.mv}} the output of \code{summary} is the same as for an object produced by \code{\link{capa}} except that the epoch is not displayed.
#'
#' For an object produced by \code{\link{capa.uv}} the output of \code{summary} is the same as for an object produced by \code{\link{capa.mv}} except that the tables do not have a variate
#' column (there is only one), or start and end lags.
#'
#' For an object produced by \code{\link{pass}} the output of \code{summary} is TODO 
#'
#' @docType methods
#'
#' @param object An S4 class produced by \code{\link{capa}}, \code{\link{capa.uv}}, \code{\link{capa.mv}} or \code{\link{pass}}.
#' 
#'@param epoch Numerical value. Since \code{\link{capa}}, \code{\link{capa.uv}} and \code{\link{capa.mv}} are sequential algorithms, it is possible to process a subset of the data
#' up to and including a given epoch. The default value for \code{epoch}is the length of the data series.
#'
#' @rdname summary-methods
#'
#' @aliases summary,capa.class,ANY-method
#' 
#' @seealso \code{\link{capa}},\code{\link{capa.uv}},\code{\link{capa.mv}},\code{\link{pass}}. 
#'
#' @export
setMethod("summary",signature=list("capa.class"),function(object,epoch)
{
    if(missing(epoch))
    {
        epoch=dim(object@data)[1]
    }
    cat("CAPA detecting changes in ",sep="")
    if(object@type == "meanvar")
    {
        cat("mean and variance.","\n",sep="") 
    }
    if(object@type == "mean")
    {
        cat("mean.","\n",sep="") 
    }
    cat("observations = ",dim(object@data)[1],sep="")
    if(epoch != dim(object@data)[1])
    {
	cat(" : Epoch = ",epoch,"\n",sep="")
    }
    else
    {
        cat("\n",sep="")
    }
    cat("variates = ",dim(object@data)[2],"\n",sep="")	
    p_anoms<-point_anomalies(object,epoch)
    c_anoms<-collective_anomalies(object,epoch)
    if(!Reduce("|",is.na(p_anoms)))
    {
        cat("Point anomalies detected : ",nrow(p_anoms),"\n",sep="")
        print(p_anoms)
    }
    if(!Reduce("|",is.na(c_anoms)))
    {
        cat("Collective anomalies detected : ",length(unique(Map(function(a,b) c(a,b),c_anoms[,1],c_anoms[,2]))),"\n",sep="")
        print(c_anoms)
    }
    invisible()
})


#' @name summary
#'
#' @docType methods
#'
#' @rdname summary-methods
#'
#' @aliases summary,capa.uv.class-method
#'
#' @export
setMethod("summary",signature=list("capa.uv.class"),function(object)
{
    cat("Univariate ",sep="")
    cat("CAPA detecting changes in ",sep="")
    if(object@type == "meanvar")
    {
        cat("mean and variance.","\n",sep="") 
    }
    if(object@type == "mean")
    {
        cat("mean.","\n",sep="") 
    }
    cat("observations = ",dim(object@data)[1],'\n',sep="")
    cat("variates = ",dim(object@data)[2],'\n',sep="")	
    cat("minimum segment length = ",object@min_seg_len,'\n',sep="")
    cat("maximum segment length = ",object@max_seg_len,'\n',sep="")
    p_anoms<-point_anomalies(object)
    c_anoms<-collective_anomalies(object)
    if(!Reduce("|",is.na(p_anoms)))
    {
        cat("Point anomalies detected : ",nrow(p_anoms),"\n",sep="")
        print(p_anoms)
    }
    if(!Reduce("|",is.na(c_anoms)))
    {
        cat("Collective anomalies detected : ",length(unique(Map(function(a,b) c(a,b),c_anoms[,1],c_anoms[,2]))),"\n",sep="")
        print(c_anoms)
    }
    invisible()
})


#' @name summary
#'
#' @docType methods
#'
#' @rdname summary-methods
#'
#' @aliases summary,capa.mv.class-method
#' 
#' @export
setMethod("summary",signature=list("capa.mv.class"),function(object)
{
    cat("Multivariate ",sep="")
    cat("CAPA detecting changes in ",sep="")
    if(object@type == "meanvar")
    {
        cat("mean and variance.","\n",sep="") 
    }
    if(object@type == "mean")
    {
        cat("mean.","\n",sep="") 
    }
    cat("observations = ",dim(object@data)[1],'\n',sep="")
    cat("variates = ",dim(object@data)[2],'\n',sep="")	
    cat("minimum segment length = ",object@min_seg_len,'\n',sep="")
    cat("maximum segment length = ",object@max_seg_len,'\n',sep="")
    cat("maximum lag = ",object@max_lag[1],'\n',sep="")
    p_anoms<-point_anomalies(object)
    c_anoms<-collective_anomalies(object)
    if(!Reduce("|",is.na(p_anoms)))
    {
        cat("Point anomalies detected : ",nrow(p_anoms),"\n",sep="")
        print(p_anoms)
    }
    if(!Reduce("|",is.na(c_anoms)))
    {
        cat("Collective anomalies detected : ",length(unique(Map(function(a,b) c(a,b),c_anoms[,1],c_anoms[,2]))),"\n",sep="")
        print(c_anoms)
    }
    invisible()
})



setMethod("show",signature=list("capa.class"),function(object)
{
    summary(object)
})



anomalies<-function(x,epoch=NULL)
{
    anomaly_types<-x@anomaly_types
    anomaly_positions<-x@anomaly_positions
    if(is.null(epoch))
    {
        epoch<-length(anomaly_types) 
    }
    if(epoch < 0 || epoch > length(anomaly_types))
    {
	stop("epoch must be a positive integer <= number of observations in data")
    }
    anomaly_types<-anomaly_types[1:epoch]
    anomaly_positions<-anomaly_positions[1:epoch]
    anom<-list()
    cursor<-length(anomaly_positions)
    count<-0
    while(cursor > 1)
    {
        if(anomaly_types[cursor] == 1)
        {
            count<-count+1
            anom[[count]]<-c(cursor,cursor)
        }
        if(anomaly_types[cursor] == 2)
        {
            count<-count+1
            anom[[count]]<-c(anomaly_positions[cursor]+1,cursor)
        }
        cursor<-anomaly_positions[cursor]
    }
    return(rev(anom))
}


  

capa.uv_call<-function(x,beta=NULL,beta_tilde=NULL,type="meanvar",min_seg_len=10,max_seg_len=Inf,transform=robustscale)
{
    
    # error trapping
    x_dash<-x
    if(!is.null(transform))
    {
        x_dash = transform(x)
        # error trap x_dash
    }
    # convert to vector structure
    if(is.array(x_dash))
    {
        x_dash<-as.vector(x_dash)
    }
    # configure defaults as required
    marshaller = marshall_MeanVarAnomaly
    if(type == "mean")
    {
        marshaller = marshall_MeanAnomaly
    }
    else if(type == "poisson")
    {
        marshaller = marshall_PoissonAnomaly
    }
    else
    {
        # error - invalid type 
    }
    if(max_seg_len == Inf)
    {
        max_seg_len = length(x_dash)
    }
    if(is.null(beta))
    {
        if(type == "mean" || type == "poisson")
        {
            beta = 3*log(length(x_dash))
        }
        else if(type == "meanvar")
        {
            beta = 4*log(length(x_dash))
        }
        else
        {
            # error - invalid type
        }
    }
    if(length(beta) == 1)
    {
        beta = rep(beta,length(x_dash))
    }
    if(is.null(beta_tilde))
    {
        beta_tilde = 3*log(length(x_dash))
    }
    
    # call the underlying method
    S<-marshaller(x_dash,
                  as.integer(length(x_dash)),
                  as.integer(min_seg_len),
                  as.integer(max_seg_len),
                  beta,
                  beta_tilde,
                  as.integer(1))
		     blob<-list(x_dash,
               as.integer(length(x_dash)),
               as.integer(min_seg_len),
               as.integer(max_seg_len),
               beta,
               beta_tilde,
               as.integer(1),
	       S)
    # construct the S4 capa class instance
    return(
        capa.uv.class(array(x,c(length(x_dash),1)), # array(x_dash,c(length(x_dash),1)),
		     array(beta,c(length(beta),1)),
                     array(beta_tilde,c(1,1)),
                     as.integer(min_seg_len),
                     as.integer(max_seg_len),
                     integer(),
                     type,
                     transform,
                     S[seq(1,length(S),2)],
                     S[seq(2,length(S),2)],
                     array(1,c(length(x),1)), #array(1,c(length(x_dash),1)),
                     array(0,c(length(x),1)), #array(0,c(length(x_dash),1)),
                     array(0,c(length(x),1))) # array(0,c(length(x_dash),1)))
        )
}




capa.mv_call<-function(x,beta=NULL,beta_tilde=NULL,type="meanvar",min_seg_len=10,max_seg_len=Inf,max_lag=0,transform=robustscale)
{
    # error trapping
    x_dash<-x
    if(!is.null(transform))
    {
        x_dash = transform(x)
        # error trap x_dash
    }
    # configure defaults as required
    marshaller = marshall_MeanVarAnomalyMV
    if(type == "mean")
    {
        marshaller = marshall_MeanAnomalyMV
    }
    else
    {
        # error - invalid type 
    }
    if(max_seg_len == Inf)
    {
        max_seg_len = nrow(x_dash)
    }
    # process beta
    n = nrow(x)
    p = ncol(x)
    s = log(n)
    if (is.null(beta))
    {            
        if(type == "mean")
        {
            if (max_lag == 0)
            {
                to_be_tried = 1:p
                a_vector = qchisq(seq(p-1,0)/p, 1)
        
                penalty_1 = 2*s + 2*to_be_tried*log(p)
                penalty_2 = rep(p + 2*s + 2*sqrt(p*s),p)
        
                penalty_3    = 2*(s+log(p)) + 1:p + 2*p*a_vector*dchisq(a_vector, 1) + 2*sqrt((1:p+2*p*a_vector*dchisq(a_vector, 1))*(s+log(p)))
                penalty_3[p] = 2*(s+log(p)) + p + 2*sqrt(p*(s+log(p)))
        
                beta = diff( c(0,pmin(penalty_1,penalty_2,penalty_3)))   
            }
      
            if (max_lag > 0)
            {
                beta = rep(2*log(p*(max_lag+1)),p)
                beta[1] = beta[1] + 2*s
            }
        }
        if(type == "meanvar")#
        {
            beta = rep(4*log(p*(max_lag+1)),p)
            beta[1] = beta[1] + 4*s         
        }
    }
    else if(length(beta) == 1)
    {
        beta = rep(beta,p)
    }
    if(is.null(beta_tilde))
    {
        beta_tilde = 3*log(length(x_dash))
    }
    
    # call the underlying method
    S<-marshaller(as.vector(x_dash),
                  as.integer(nrow(x_dash)),
                  as.integer(ncol(x_dash)),
                  as.integer(max_lag),
                  as.integer(min_seg_len),
                  beta,
                  beta_tilde,
                  as.integer(max_seg_len),
                  as.integer(1))
    # construct the S4 capa class instance
    S = matrix(S,nrow(x_dash),byrow=T)
    p = ncol(x_dash)
    return(
        capa.class(x, #x_dash,
                   array(beta,c(length(beta),1)),
                   array(beta_tilde,c(1,1)),
                   as.integer(min_seg_len),
                   as.integer(max_seg_len),
                   integer(max_lag),
                   type,
                   transform,
                   S[,1], 
                   S[,2],
                   matrix(S[,2 + 0*p + 1:p],ncol=p),
                   matrix(S[,2 + 1*p + 1:p],ncol=p),  
                   matrix(S[,2 + 2*p + 1:p],ncol=p))
        )
}

#' capa 
#'
#' A technique for detecting anomalous segments based on CAPA (Collective And Point Anomalies) by Fisch et al.
#' 
#' @param x A numeric matrix with n rows and p columns containing the data which is to be inspected.
#' @param beta A numeric vector of length p, giving the marginal penalties. If p > 1, type ="meanvar" or type = "mean" and max_lag > 0 it defaults to the penalty regime 2' described in 
#' Fisch, Eckley and Fearnhead (2018). If p > 1, type = "mean" and max_lag = 0 it defaults to the pointwise minimum of the penalty regimes 1, 2, and 3 in Fisch, Eckley and Fearnhead (2018).
#' @param beta_tilde A numeric constant indicating the penalty for adding an additional point anomaly. It defaults to a BIC style penalty if no argument is provided.
#' @param type A string indicating which type of deviations from the baseline are considered. Can be "meanvar" for collective anomalies characterised by joint changes in mean and
#' variance (the default), "mean" for collective anomalies characterised by changes in mean only, and "poisson" if the collective anomalies are characterised by an a-typical poisson parameter.
#' @param min_seg_len An integer indicating the minimum length of epidemic changes. It must be at least 2 and defaults to 10.
#' @param max_seg_len An integer indicating the maximum length of epidemic changes. It must be at least min_seg_len and defaults to Inf.
#' @param max_lag A non-negative integer indicating the maximum start or end lag. Default value is 0.
#' @param transform A function used to transform the data prior to analysis by \code{\link{capa}}. This can, for example, be used to compensate for the effects of autocorrelation in the data. Importantly, the
#' untransformed data remains available for post processing results obtained using \code{\link{capa}}. The package includes several methods that are commonly used for
#' the transform, (see \code{\link{robustscale}} and \code{\link{ac_corrected}}), but a user defined function can be specified. The default values is \code{transform=robust_scale}. 
#' 
#' @return An S4 class of type capa.class. 
#'
#' @references \insertRef{2018arXiv180601947F}{anomaly}
#' 
#' @examples
#' library(anomaly)
#' data(acgh)
#' res<-capa(acgh,type="mean",max_lag=5,transform=ac_corrected)
#' plot(res,tile_plot=FALSE,subset=1:20)
#'
#' @export
capa<-function(x,beta=NULL,beta_tilde=NULL,type="meanvar",min_seg_len=10,max_seg_len=NULL,max_lag=0,transform=robustscale)
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
    # check dimensions
    if(length(dim(x)) != 2)
    {
        stop("cannot convert x to a two dimensional array")
    }
    if(dim(x)[1] == 1)
    {
        x<-t(x)
    }
    # try transforming the data
    if(!is_function(transform))
    {
        stop("transform must be a function")
    }
    x<-transform(x)
    # and check the transformed data
    if(!is_array(x))
    {
        stop("cannot convert the transformed x to an array - check the transform function")
    }
    if(!all(is_not_na(x)))
    {
        stop("the transformed x contains NA values - check the transform function")
    }
    if(!all(is_not_null(x)))
    {
        stop("the transformed x contains NULL values - check the transform function")
    }
    if(!is_numeric(x))
    {
        stop("the transformed x must be of type numeric - check the transform function")
    }
    if(length(dim(x)) != 2)
    {
        stop("cannot convert transformed x to a two dimensional array - check the transform function")
    }

    # check the type 
    types=list("mean","meanvar")
    if(!(type %in% types))
    {
        stop("type has to be one of mean or meanvar")
    }

    # set analysis type
    univariate<-FALSE
    if(dim(x)[2] == 1)
    {
        univariate<-TRUE
    }

    # check max_lag
    if(!is_numeric(max_lag))
    {
        stop("max_lag must be numeric")
    }
    if(max_lag < 0)
    {
        stop("max_lag must be positive or zero")
    }
    if(univariate && max_lag != 0)
    {
        warning("max_lag != 0 redundant for a univariate analysis")
    }

    # check min_seg_len
    if(!is_numeric(min_seg_len))
    {
        stop("min_seg_len must be numeric")
    }
    if(type=="mean" && min_seg_len < 1)
    {
        stop("when type=mean, min_seg_len must be greater than zero")
    }
    if(type=="meanvar" && min_seg_len < 2)
    {
        stop("when type=meanvar, min_seg_len must be greater than 1")
    }
    if(min_seg_len < 1)
    {
        stop("min_seg_len must be greater than zero")
    }
    if(min_seg_len > dim(x)[1])
    {
        stop("min_seg_len must be less tha the number of observations in x")
    }

    # check max_seg_len
    if(is.null(max_seg_len))
    {
        max_seg_len=dim(x)[1]
    }
    if(!is_numeric(max_seg_len))
    {
        stop("max_seg_len must be numeric")
    }
    if(max_seg_len < 1)
    {
        stop("max_seg_len must be greater than zero")
    }
    # check relative values of min and max segment length
    if(max_seg_len < min_seg_len)
    {
        stop("max_seg_len must be greater than min_seg_len")
    }
    # check beta_tilde
    if(!is.null(beta_tilde))
        {
            if(!is_numeric(beta_tilde))
            {
                stop("beta_tilde must be numeric")
            }
            if(length(beta_tilde) != 1)
            {
                stop("beta_tilde must be a single scalar value")
            }
            if(beta_tilde < 0)
            {
                stop("beta_tilde must be >= 0")
            }
        }
    # check beta
    if(!is.null(beta))
    {
        if(!is_numeric(beta))
        {
            stop("beta must be numeric")
        }
        if(length(beta) != 1 && length(beta) != dim(x)[2])
        {
            stop(paste("beta must be a single scalar value or a vector of length ",dim(x)[2],'\n',sep=""))
        }
        if(!(all(beta_tilde >= 0)))
        {
                stop("beta values must be >= 0")
        }
    }

    # call appropriate method
    if(univariate)
    {
        return(capa.uv_call(x,beta,beta_tilde,type,min_seg_len,max_seg_len,transform))
    }
    else
    {
        return(capa.mv_call(x,beta,beta_tilde,type,min_seg_len,max_seg_len,max_lag,transform))
    }
    
}


#' capa.uv 
#'
#' A technique for detecting anomalous segments based on CAPA (Collective And Point Anomalies) by Fisch et al.
#' 
#' @param x A numeric vector containing the data which is to be inspected.
#' @param beta A numeric constant indicating the penalty for adding an additional epidemic changepoint. It defaults to a BIC style penalty if no argument is provided.
#' @param beta_tilde A numeric constant indicating the penalty for adding an additional point anomaly. It defaults to a BIC style penalty if no argument is provided.
#' @param type A string indicating which type of deviations from the baseline are considered. Can be "meanvar" for collective anomalies characterised by joint changes in mean and
#' variance (the default), "mean" for collective anomalies characterised by changes in mean only, and "poisson" if the collective anomalies are characterised by an a-typical poisson parameter.
#' @param min_seg_len An integer indicating the minimum length of epidemic changes. It must be at least 2 and defaults to 10.
#' @param max_seg_len An integer indicating the maximum length of epidemic changes. It must be at least the min_seg_len and defaults to Inf.
#'
#' @return An S4 class derived from type capa.class. 
#'
#' @references \insertRef{2018arXiv180601947F}{anomaly}
#' 
#' @examples
#' library(anomaly)
#' # Simulated data example
#' set.seed(2018)
#' # Generate data typically following a normal distribution with mean 0 and variance 1.
#' # Then introduce 3 anomaly windows and 4 point outliers.
#' x = rnorm(5000)
#' x[401:500] = rnorm(100,4,1)
#' x[1601:1800] = rnorm(200,0,0.01)
#' x[3201:3500] = rnorm(300,0,10)
#' x[c(1000,2000,3000,4000)] = rnorm(4,0,100)
#' res<-capa.uv(x)
#' res # print a summary of the results
#' plot(res) # visulaize the results
#' 
#' 
#' @export
capa.uv<-function(x,beta=NULL,beta_tilde=NULL,type="meanvar",min_seg_len=10,max_seg_len=Inf)
{
    res<-capa.uv_call(x,beta,beta_tilde,type,min_seg_len,max_seg_len,robustscale)
    return(res)
    return(
    capa.uv.class(data=res@data,
                 beta=res@beta,
	         beta_tilde=res@beta_tilde,
	         min_seg_len=res@min_seg_len,
	         max_seg_len=res@max_seg_len,
	         max_lag=res@max_lag,
	         type=res@type,
                 transform=robustscale,
                 anomaly_types=res@anomaly_types,
	         anomaly_positions=res@anomaly_positions,
	         components=res@components,
	         start_lags=res@start_lags,
	         end_lags=res@end_lags)
           )
}





#' capa.mv 
#'
#' A technique for detecting anomalous segments based on CAPA (Collective And Point Anomalies) by Fisch et al.
#' 
#' @param x A numeric matrix with n rows and p columns containing the data which is to be inspected.
#' @param beta A numeric vector of length p, giving the marginal penalties. If type ="meanvar" or if type = "mean" and maxlag > 0 it defaults to the penalty regime 2' described in 
#' Fisch, Eckley and Fearnhead (2018). If type = "mean" and maxlag = 0 it defaults to the pointwise minimum of the penalty regimes 1, 2, and 3 in Fisch, Eckley and Fearnhead (2018).
#' @param beta_tilde A numeric constant indicating the penalty for adding an additional point anomaly. It defaults to a BIC style penalty if no argument is provided.
#' @param type A string indicating which type of deviations from the baseline are considered. Can be "meanvar" for collective anomalies characterised by joint changes in mean and
#' variance (the default), "mean" for collective anomalies characterised by changes in mean only, and "poisson" if the collective anomalies are characterised by an a-typical poisson parameter.
#' @param min_seg_len An integer indicating the minimum length of epidemic changes. It must be at least 2 and defaults to 10.
#' @param max_seg_len An integer indicating the maximum length of epidemic changes. It must be at least the min_seg_len and defaults to Inf.
#' @param max_lag A non-negative integer indicating the maximum start or end lag. Default value is 0.
#' @return An S4 class derived from type capa.class. 
#'
#' @references \insertRef{2018arXiv180601947F}{anomaly}
#'
#' @examples
#' library(anomaly)
#' data(simulated)
#' res<-capa(sim.data,type="mean",min_seg_len=2,max_lag=5)
#' collective_anomalies(res)
#'
#' @export
capa.mv<-function(x,beta=NULL,beta_tilde=NULL,type="meanvar",min_seg_len=10,max_seg_len=Inf,max_lag=0)
{
    res<-capa.mv_call(x,beta,beta_tilde,type,min_seg_len,max_seg_len,max_lag,robustscale)
    return(
    capa.mv.class(data=res@data,
                 beta=res@beta,
	         beta_tilde=res@beta_tilde,
	         min_seg_len=res@min_seg_len,
	         max_seg_len=res@max_seg_len,
	         max_lag=as.integer(max_lag),
	         type=res@type,
                 transform=robustscale,
                 anomaly_types=res@anomaly_types,
	         anomaly_positions=res@anomaly_positions,
	         components=res@components,
	         start_lags=res@start_lags,
	         end_lags=res@end_lags)
           )
}


capa_line_plot<-function(object,epoch=dim(object@data)[1],subset=1:ncol(object@data),variate_names=TRUE)
{
    # creating null entries for ggplot global variables so as to pass CRAN checks
    x<-value<-ymin<-ymax<-x1<-x2<-y1<-y2<-x1<-x2<-y1<-y2<-NULL
    data_df<-as.data.frame(object@data)
    names<-paste("y",1:ncol(object@data),sep="")
    colnames(data_df)<-names
    data_df<-as.data.frame(data_df[,subset,drop=FALSE])
    n<-nrow(data_df)
    p<-ncol(data_df)
    data_df<-cbind(data.frame("x"=1:n),data_df)
    data_df<-melt(data_df,id="x")
    out<-ggplot(data=data_df)
    out<-out+aes(x=x,y=value)
    out<-out+geom_point()
    # highlight the collective anomalies
    c_anoms<-collective_anomalies(object,epoch)
    c_anoms<-c_anoms[c_anoms$variate %in% subset,]
    if(!any(is.na(c_anoms)) & nrow(c_anoms) > 0)
    {
        c_anoms_data_df<-c_anoms[,1:3]
        names(c_anoms_data_df)<-c(names(c_anoms_data_df)[1:2],"variable")
        c_anoms_data_df$variable<-names[c_anoms_data_df$variable]
        c_anoms_data_df$ymin<--Inf
        c_anoms_data_df$ymax<-Inf
        out<-out+geom_rect(data=c_anoms_data_df,inherit.aes = F,mapping=aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),fill="blue",alpha=0.2)
        c_anoms_data_df$start<-c_anoms_data_df$start+c_anoms$start.lag
        c_anoms_data_df$end<-c_anoms_data_df$end-c_anoms$end.lag
        out<-out+geom_rect(data=c_anoms_data_df,inherit.aes = F,mapping=aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),fill="blue",alpha=0.5)
    }
    # out<-out+facet_grid(variable~.,scales="free_y")
    # highlight the point anomalies
    p_anoms<-point_anomalies(object,epoch)
    p_anoms<-p_anoms[p_anoms$variate %in% subset,]
    if(!any(is.na(p_anoms)) & nrow(p_anoms) > 0)
        {
            p_anoms_data_df<-Reduce(rbind,Map(function(a,b) data_df[data_df$variable==names[a] & data_df$x==b,],p_anoms$variate,p_anoms$location))
            out<-out+geom_point(data=p_anoms_data_df,colour="red", size=1.5)
        }
    out<-out+facet_grid(factor(variable,levels=rev(names)) ~ .,scales="free_y")
    # grey out the data after epoch
    if(epoch != nrow(object@data))
        {
            d<-data.frame(x1=epoch,x2=n,y1=-Inf,y2=Inf)
            out<-out+geom_rect(data=d,inherit.aes=F,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),fill="yellow",alpha=0.2)
        }
    if(variate_names==FALSE)
        {
            out<-out+theme(strip.text.y=element_blank())
        }
    # change background
    out<-out+theme(
                   # Hide panel borders and remove grid lines
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   # Change axis line
                   axis.line = element_line(colour = "black")
                 )
    return(out)
}


capa_tile_plot<-function(object,variate_names=FALSE,epoch=dim(object@data)[1],subset=1:ncol(object@data))
{
    # nulling out variables used in ggplot to get the package past CRAN checks
    x1<-y1<-x2<-y2<-variable<-value<-NULL
    df<-as.data.frame(object@data)
    df<-as.data.frame(df[,subset,drop=FALSE])
    # normalise data
    for(i in 1:ncol(df))
    {
        df[,i]<-(df[,i]-min(df[,i]))/(max(df[,i])-min(df[,i]))
    }
    n<-data.frame("n"=seq(1,nrow(df)))
    molten.data<-melt(cbind(n,df),id="n")
    out<-ggplot(molten.data, aes(n,variable))
    out<-out+geom_tile(aes(fill=value))
    # get any collective anomalies
    c_anoms<-collective_anomalies(object,epoch=epoch)
    c_anoms<-c_anoms[c_anoms$variate %in% subset,]
    c_anoms<-unique(c_anoms[,1:2])
    if(!any(is.na(c_anoms)) & nrow(c_anoms) > 0)
    {
        ymin<-0
        ymax<-ncol(df)
        out<-out+annotate("rect",xmin=c_anoms$start,xmax=c_anoms$end,ymin=ymin,ymax=ymax+1,alpha=0.0,color="red",fill="yellow")
    }
    if(epoch != nrow(object@data))
        {
            d<-data.frame(x1=epoch,x2=nrow(object@data),y1=-Inf,y2=Inf)
            out<-out+geom_rect(data=d,inherit.aes=F,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),fill="yellow",alpha=0.2)
        }
    if(variate_names==FALSE)
    {
        out<-out+theme(axis.text.y=element_blank(),axis.title=element_blank())
    }
    out<-out+theme(
                 # Hide panel borders and remove grid lines
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 # Change axis line
                 axis.line = element_line(colour = "black")
                 )
    return(out)
}




#' Visualisation of data, collective and point anomalies.
#'
#' @name plot
#'
#' @description Plot methods for S4 objects returned by \code{\link{capa}}, \code{\link{capa.uv}}, \code{\link{capa.mv}} and \code{\link{pass}}. 
#'
#' The plot can either be a line plot or a tileplot, the type produced depending on the options provided to the \code{plot} function and/or the dimensions of the
#' data associated with the S4 object.
#'
#' @docType methods
#'
#' @param x An S4 class produced by \code{\link{capa}}, \code{\link{capa.uv}}, \code{\link{capa.mv}} or \code{\link{capa.mv}}.
#' @param subset A numeric vector specifying a subset of the variates to be displayed. Default value is all of the variates present in the data.
#' @param variate_names Logical value indicating if variate names should be displayed on the plot. This is useful when a large number of variates are being displayed
#' as it makes the visualisation easier to interpret. Default value is TRUE.
#' @param epoch Numerical value. Since \code{\link{capa}}, \code{\link{capa.uv}} and \code{\link{capa.mv}} are sequential algorithms, it is possible to process a subset of the data
#' up to and including a given epoch. The default value for \code{epoch}is the length of the data series.
#' @param tile_plot Logical value. If TRUE then a tile plot of the data is produced. The data displayed in the tile plot is normalised to values in [0,1] for each variate.
#' This type of plot is useful when the data contains are large number of variates. The defaut value is TRUE if the number of variates is greater than 20.
#' 
#' @return A ggplot object.
#'
#' @rdname plot-methods
#'
#' @aliases plot,capa.class,ANY-method
#' 
#' @seealso \code{\link{capa}},\code{\link{capa.uv}},\code{\link{capa.mv}},\code{\link{pass}}.
#'
#' @export
setMethod("plot",signature=list("capa.class"),function(x,subset,variate_names,epoch,tile_plot)
{
    if(missing(subset))
    {
        subset<-1:ncol(x@data)
    }
    if(missing(variate_names))
    {
        variate_names<-NULL
    }
    if(missing(epoch))
    {
        epoch<-nrow(x@data)
    }
    if(missing(tile_plot))
    {
        tile_plot<-NULL
    }
    subset<-sort(unique(subset))
    if(!is.logical(tile_plot))
    {
        if(is.null(tile_plot))
        {
            tile_plot<-FALSE
            if(length(subset) > 20)
            {
                tile_plot<-TRUE
            }
        }
        else
        {
            stop("tile_plot must be of type logical or NULL")
        }
    }
    if(!is.logical(variate_names))
    {
        if(is.null(variate_names))
        {
            variate_names<-TRUE
            if(tile_plot==TRUE)
            {
                variate_names<-FALSE
            }
        }
        else
        {
            stop("variable_names must be of type logical or NULL")
        }
    }
    if(tile_plot)
    {
        return(capa_tile_plot(x,variate_names=variate_names,epoch=epoch,subset=subset))
    }
    else
    {
        return(capa_line_plot(x,variate_names=variate_names,epoch=epoch,subset=subset))
    }
})


#' @name plot
#'
#' @docType methods
#'
#' @param variate_name Logical value indicating if the variate name should be displayed. Defualt value is \code{variate.name=TRUE}.
#' 
#' @rdname plot-methods
#'
#' @aliases plot,capa.uv.class,ANY-method
#'
#' @export
setMethod("plot",signature=list("capa.uv.class"),function(x,variate_name)
{
    if(missing(variate_name))
    {
        variate_name<-NULL
    }
    return(plot(as(x,"capa.class"),variate_names=variate_name))
})


#' @name plot
#'
#' @docType methods
#'
#' @rdname plot-methods
#'
#' @aliases plot,capa.mv.class,ANY-method
#'
#' @export
setMethod("plot",signature=list("capa.mv.class"),function(x,subset,variate_names,tile_plot)
{
    if(missing(subset))
    {
        subset<-1:ncol(x@data)
    }
    if(missing(variate_names))
    {
        variate_names<-NULL
    }
    if(missing(tile_plot))
    {
        tile_plot<-NULL
    }
    return(plot(as(x,"capa.class"),subset=subset,variate_names=variate_names,tile_plot=tile_plot))
})



