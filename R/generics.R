## Generic s4 class functions

#' Point anomaly location and strength.
#'
#' @name point_anomalies
#'
#' @description Creates a data frame containing point anomaly locations and strengths as detected by \code{\link{capa}}.
#'  
#' Returns a data frame with columns containing the position, strength, and (for multivariate data) the variate number.
#'
#' @param ... TODO
#' @docType methods
#'
#' @rdname point_anomaly-methods
#'
#' @seealso \code{\link{capa}}. 
setGeneric("point_anomalies",function(object,...) standardGeneric("point_anomalies") )

#' Collective anomaly location, lags, and mean/variance changes.
#'
#' @name collective_anomalies
#'
#' @description Creates a data frame containing collective anomaly locations, lags and changes in mean and variance as detected by \code{\link{capa}}, \code{\link{pass}}, and \code{\link{sampler}}. 
#'
#' For an object created by \code{\link{capa}} returns a data frame with columns containing the start and end position of the anomaly, the change in mean
#' due to the anomaly. For multivariate data a data frame with columns containing the start and end position of the anomaly, the variates 
#' affected by the anomaly, as well as their the start and end lags. When \code{type="mean"/"robustmean"} only the change in mean is reported. When \code{type="meanvar"} both the change in mean and
#' change in variance are included. If \code{merged=FALSE} (the default), then all the collective anomalies are processed individually even if they are common across multiple variates.
#' If \code{merged=TRUE}, then the collective anomalies are grouped together across all variates that they appear in.
#'
#' For an object produced by \code{\link{pass}} or \code{sampler} returns a data frame containing the start, end and strength of the collective anomalies.
#'
#' @param ... TODO
#' 
#' @docType methods
#'
#' @rdname collective_anomalies-methods
setGeneric("collective_anomalies",function(object,...) standardGeneric("collective_anomalies") )
