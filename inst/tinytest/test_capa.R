## tests for the capa class objects

## basic tests to check things run
data(simulated)
## univariate
expect_silent({ res<-capa(sim.data[,1,drop=F],type="meanvar",min_seg_len=2) })
expect_silent({ res<-capa(sim.data[,1,drop=F],type="robustmean",min_seg_len=2) })
expect_silent({ res<-capa(sim.data[,1,drop=F],type="mean",min_seg_len=2) })
## multivariate
expect_silent({ res<-capa(sim.data,type="meanvar",min_seg_len=2,max_lag=5) })
expect_silent({ res<-capa(sim.data,type="robustmean",min_seg_len=2,max_lag=5) }) ## TODO - this seems very slow
expect_silent({ res<-capa(sim.data,type="mean",min_seg_len=2,max_lag=5) })

expect_silent({ summary(res) })
expect_silent({ show(res) })
expect_silent({ collective_anomalies(res) })
expect_silent({ point_anomalies(res) })
expect_silent({ plot(res,variate_names=TRUE) }) ## TODO - capture both types of plotting

