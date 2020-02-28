
setClass("anomaly_series")

#' A technique for detecting anomalous segments based on CAPA.
#'
#' @name anomaly_series
#' 
#' @description Method for calling \code{\link{capa.uv}} in a manner consistent with older versions of the anomaly package. 
#'
#' @param x A numeric vector containing the data which is to be inspected.
#' @param penaltywindow A numeric constant indicating the penalty for adding an additional epidemic changepoint. It defaults to a BIC style penalty if no argument is provided.
#' @param penaltyanomaly A numeric constant indicating the penalty for adding an additional point anomaly. It defaults to a BIC style penalty if no argument is provided.
#' @param minimumsegmentlength An integer indicating the minimum length of epidemic changes. It must be at least 2 and defaults to 10.
#' @param warnings A logical determining whether the warnings should be displayed. It defaults to TRUE.
#' @param method A string indicating which type of deviations from the baseline are considered. Can be "meanvar" for collective anomalies characterised by joint changes in mean and
#' variance (the default) and "mean" for collective anomalies characterised by changes in mean only.
#'
#' @return An anomaly_series object.
#'
#' @examples
#' library(anomaly)
#' set.seed(2018)
#' # Generate data typically following a normal distribution with mean 0 and variance 1. 
#' # Then introduce 3 anomaly windows and 4 point outliers.
#' x  = rnorm(5000)
#' x[401:500]   = rnorm(100,4,1)
#' x[1601:1800] = rnorm(200,0,0.01)
#' x[3201:3500] = rnorm(300,0,10)
#' x[c(1000,2000,3000,4000)] = rnorm(4,0,100)
#' inferred_anomalies = anomaly_series(x)
#' plot(inferred_anomalies)
#' summary(inferred_anomalies)
#' print(inferred_anomalies)
#'
#' ### Repeat with other method
#'
#' inferred_anomalies = anomaly_series(x,method="mean")
#' plot(inferred_anomalies)
#' summary(inferred_anomalies)
#' print(inferred_anomalies)
#' 
#' ### Real data example: 
#' 
#' library(anomaly)
#' data(Lightcurves)
#' ### Plot the data for Kepler 10965588: No transit apparent
#' plot(Lightcurves$Kepler10965588$Day,Lightcurves$Kepler10965588$Brightness,xlab = "Day",pch=".")
#' ### Examine a period of 62.9 days for Kepler 10965588
#' binned_data = period_average(Lightcurves$Kepler10965588,62.9)
#' inferred_anomalies = anomaly_series(binned_data)
#' plot(inferred_anomalies,xlab="Bin")
#' # We found a planet!
#'
#' @references \insertRef{2018arXiv180601947F}{anomaly}
#' 
#' @export
anomaly_series<-function(x,penaltywindow=NULL,penaltyanomaly=NULL,minimumsegmentlength=10,warnings=TRUE,method="meanvar")
{
    res<-capa.uv(x=x,beta=penaltywindow,beta_tilde=penaltyanomaly,min_seg_len=minimumsegmentlength,type=method,max_seg_len=Inf,transform=robustscale)
    res@type="meanvar" # mutate the object so as to always calculate a variance change
    canoms<-collective_anomalies(res)
    panoms<-point_anomalies(res)
    res<-list("x"=x,
              "anomalies_strength"=data.frame("variance_change"=canoms$variance.change,"mean_change"=canoms$mean.change),
              "pointanomalies_strength"=panoms$strength,
              "pointanomalies"=panoms$location, 
              "anomalywindows"=canoms[,1:2])
    class(res)<-"anomaly_series"
    return(res)
}



plot.anomaly_series = function(x,xlab="",ylab="",...){
 anomaly_object = x
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  tmpdaf = data.frame(x = 1:length(anomaly_object[["x"]]),y = anomaly_object[["x"]])
  
  output = ggplot(tmpdaf,aes_string(x="x",y="y"))
  
  if (length(anomaly_object[["pointanomalies"]])>0.5){
    pointanomalydaf = data.frame(x=anomaly_object[["pointanomalies"]],y=anomaly_object[["x"]][anomaly_object[["pointanomalies"]]])
    output = output + geom_point(data = pointanomalydaf,aes_string(x="x",y="y"), colour="red", size=2)
  }
  
  if (nrow(anomaly_object[["anomalywindows"]])>0){
    collectiveanomalydaf = anomaly_object[["anomalywindows"]]
    collectiveanomalydaf$ymax =  Inf
    collectiveanomalydaf$ymin = -Inf
    output = output + geom_rect(data = collectiveanomalydaf,inherit.aes = F,mapping = aes_string(xmin="start",xmax="end",ymin="ymin",ymax="ymax"),fill="red",alpha=0.5)
  }
  
  output = output+geom_point()+ labs(x=xlab,y=ylab)
  
  return(output)
  
}


print.anomaly_series = function(x,...){
  
  anomaly_object = x
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",paste(unexpectedarguments,", "),"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}
  
  cat("Point anomalies detected:\t")
  cat(length(anomaly_object[["pointanomalies"]]))
  cat("\n")
  cat("location\t")
  cat("strength\t")
  cat("\n")
  if (length(anomaly_object[["pointanomalies"]]) > 0){
    for (ii in 1:length(anomaly_object[["pointanomalies"]]) ){
      cat(anomaly_object[["pointanomalies"]][ii])
      cat("\t")
      cat("\t")
      cat(anomaly_object[["pointanomalies_strength"]][ii])
      cat("\n")
    }
  }
  cat("\n")
  cat("Anomalous segments detected:\t")
  cat(nrow(anomaly_object[["anomalywindows"]]))
  cat("\n")
  cat("start\t")
  cat("end\t")
  cat("mean change\t")
  cat("variance change\t")
  cat("\n")
  if (nrow(anomaly_object[["anomalywindows"]]) > 0){
    for (ii in 1:nrow(anomaly_object[["anomalywindows"]]) ){
      cat(anomaly_object[["anomalywindows"]][ii,"start"])
      cat("\t")
      cat(anomaly_object[["anomalywindows"]][ii,"end"])
      cat("\t")
      cat(anomaly_object[["anomalies_strength"]][ii,"mean_change"])
      cat("\t")
      cat(anomaly_object[["anomalies_strength"]][ii,"variance_change"])
      cat("\n")
    }
  }
  
}


summary.anomaly_series = function(object,...){
  
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  anomaly_object = object
  
  cat("Point anomalies detected:\t")
  cat(length(anomaly_object[["pointanomalies"]]))
  cat("\n")
  cat("Anomalous segments detected:\t")
  cat(nrow(anomaly_object[["anomalywindows"]]))
  
}


#' @name plot
#'
#' @docType methods
#'
#' @param xlab Character string containing label for the x-axis.
#' @param ylab Character string containing label for the y-axis.
#' @param ... Other parameters to be passed to plotting methods.
#' 
#' @rdname plot-methods
#'
#' @aliases plot,anomaly_series-method
#'
#' @export
setMethod("plot",signature=list("anomaly_series"),function(x,xlab="",ylab="",...)
{
    return(plot.anomaly_series(x,xlab,ylab))
})


#' @name summary
#'
#' @docType methods
#'
#' @param ... Other parameters to be passed to summary methods.
#' 
#' @rdname summary-methods
#'
#' @aliases summary,anomaly_series-method
#'
#' @export
setMethod("summary",signature=list("anomaly_series"),function(object,...)
{
    return(summary.anomaly_series(object))
})

#' @name show
#'
#' @docType methods
#' 
#' @rdname show-methods
#'
#' @aliases show,anomaly_series-method
#'
#' @export
setMethod("show",signature=list("anomaly_series"),function(object)
{
    return(print.anomaly_series(object))
})









