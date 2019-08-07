#' Simulated data.
#'
#' @description Simulated data for 200 variates with 500 observations containing three recurrent variants located at [100: 105], [200, 205], and [300:305]
#' with carrier proportions 0.04, 0.06, and 0.08 respectively. (add reference here)
#' 
#' @name sim.data
#' 
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(simulated)
#'
#' @rdname simulated-data
#'
#' @format A matrix with 500 rows and 200 columns.
#'
#' @references \insertRef{10.1093/biomet/ass059}{anomaly}
#' 
NULL



#' Machine temperature.
#'
#' @description Temperature sensor data of an internal component of a large, industrial machine. The data contains three known anomalies.
#' The first anomaly is a planned shutdown of the machine. The second anomaly is difficult to detect and directly led to the third anomaly, a catastrophic failure of the machine.
#' The data consists of 22695 observations of machine temperature recorded at 5 minute intervals along with the date and time of the measurement. The data was obtained from the Numenta
#' Anomaly Benchmark \insertCite{AHMAD2017134}{anomaly}.
#' 
#' @name machinetemp
#' 
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(machinetemp)
#'
#' @rdname machine-temperature-data
#'
#' @format A dataframe with 22695 rows and 2 columns. The first column contains the date and time of the temperature measurement. The second column contains the
#' machine temperature.
#'
#' @references \insertRef{AHMAD2017134}{anomaly}
#' 
NULL

#' Microarray data
#' @description Microarray data for 41 individuals with a bladder tumour. The data was obtained from the \pkg{ecp} package \insertRef{ecp-package}{anomaly} and standardised using
#' robust measures of mean and variance. 
#'
#' @name acgh
#' 
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(acgh)
#'
#' @rdname acgh-data
#'
#' @format A dataframe with 2215 rows and 41 columns. Each column contains data for one individual.
#'
#' @references \insertRef{ecp-package}{anomaly}
#' 
NULL