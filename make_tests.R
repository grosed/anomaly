## making tests from current CRAN version of anomaly
## designed to be run in root directory of package
## CHECK original anomaly version is the one loaded...

rm(list=ls())
graphics.off()

library(anomaly)
packageVersion("anomaly") ## should be 4.0.2

load("data/simulated.RData")

X <- sim.data
X[100,1] <- 45



## #################################################################
## test output for capa
## #################################################################
out <- list()

## ##################################################
## single series
res <- capa(X[,1,drop=F],
            beta = NULL,
            beta_tilde = NULL,
            type = "meanvar",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 0,
            transform = robustscale
            )
out$single_meanvar = list( point = point_anomalies(res),
                          collective = collective_anomalies(res) )

res <- capa(X[,1,drop=F],
            beta = NULL,
            beta_tilde = NULL,
            type = "mean",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 0,
            transform = robustscale
            )
out$single_mean = list( point = point_anomalies(res),
                       collective = collective_anomalies(res) )

res <- capa(X[,1,drop=F],
            beta = NULL,
            beta_tilde = NULL,
            type = "robustmean",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 0,
            transform = robustscale
            )
out$single_robustmean = list( point = NA, ## This fails in current package.... point_anomalies(res),
                             collective = collective_anomalies(res) )

## ###################################
## multivariate no lag
res <- capa(X,
            beta = NULL,
            beta_tilde = NULL,
            type = "meanvar",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 0,
            transform = robustscale
            )
out$multi_meanvar = list( point = point_anomalies(res),
                          collective = collective_anomalies(res) )

res <- capa(X,
            beta = NULL,
            beta_tilde = NULL,
            type = "mean",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 0,
            transform = robustscale
            )
out$multi_mean = list( point = point_anomalies(res),
                       collective = collective_anomalies(res) )

res <- capa(X,
            beta = NULL,
            beta_tilde = NULL,
            type = "robustmean",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 0,
            transform = robustscale
            )
out$multi_robustmean = list( point = NA, 
                             collective = collective_anomalies(res) )

## #####################################################
## multivariate with lag
res <- capa(X,
            beta = NULL,
            beta_tilde = NULL,
            type = "meanvar",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 10,
            transform = robustscale
            )
out$multi_10_meanvar = list( point = point_anomalies(res),
                          collective = collective_anomalies(res) )

res <- capa(X,
            beta = NULL,
            beta_tilde = NULL,
            type = "mean",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 10,
            transform = robustscale
            )
out$multi_10_mean = list( point = point_anomalies(res),
                       collective = collective_anomalies(res) )

res <- capa(X,
            beta = NULL,
            beta_tilde = NULL,
            type = "robustmean",
            min_seg_len = 2,
            max_seg_len = Inf,
            max_lag = 10,
            transform = robustscale
            )
out$multi_10_robustmean = list( point = NA, ## This fails in current package.... point_anomalies(res),
                             collective = collective_anomalies(res) )


saveRDS(out,"inst/tinytest/capa_results402_v2.rds")

## #################################################################
## test output for bard
## #################################################################
out <- list()

## #################################################################
## test output for pass
## #################################################################
out <- list()





