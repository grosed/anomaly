
#' Machine temperature data.
#'
#' @description Temperature sensor data of an internal component of a large, industrial machine. The data contains three known anomalies.
#' The first anomaly is a planned shutdown of the machine. The second anomaly is difficult to detect and directly led to the third anomaly, a catastrophic failure of the machine.
#' The data consists of 22695 observations of machine temperature recorded at 5 minute intervals along with the date and time of the measurement. The data was obtained from the Numenta
#' Anomaly Benchmark \insertCite{AHMAD2017134}{anomaly}, which can be found at https://github.com/numenta/NAB.
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

#' Simulated data.
#'
#' @description A simulated data set for use in the examples and vignettes. The data consists of 500 observations on 20 variates drawn from the standard normal distribution.
#' Within the data there are three multivariate anomalies of length 15 located at t=100, t=200, and t=300 for which the mean changes from 0 to 2. The anomalies affect
#' variates 1 to 8, 1 to 12 and 1 to 16 respectively.
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
#' @format A matrix with 500 rows and 40 columns.
#' 
NULL



