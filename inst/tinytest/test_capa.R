## tests for the capa class objects

## basic tests to check things run
data(simulated)
expect_silent({ res<-capa(sim.data,type="mean",min_seg_len=2,max_lag=5) })
expect_silent({ summary(res) })
expect_silent({ show(res) })
expect_silent({ collective_anomalies(res) })
expect_silent({ point_anomalies(res) })
expect_silent({ plot(res,variate_names=TRUE) }) ## TODO - capture both types of plotting

