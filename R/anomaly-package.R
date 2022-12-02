#' Another package for anomoly detection
#'
#' TODO - write this bit
#' 
#' @name anomaly-package
#' @docType package
#' @useDynLib anomaly, .registration = TRUE
#' @importFrom methods new as
#' @importFrom stats dnbinom pnbinom runif dchisq end is.ts qchisq start var
#' @importFrom utils tail
#' @importFrom zoo is.zoo
#' @importFrom dplyr group_by mutate
#' @importFrom Rdpack reprompt
## TODO - be more selective from the following packages
#' @import Rcpp
#' @import ggplot2
#' @import tidyr
#' @import assertive
#' @import xts
NULL
