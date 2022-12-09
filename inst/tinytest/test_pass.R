## tests for pass class objects

## basic tests to check things run
data(simulated)
expect_silent({ res<-pass(sim.data)})
expect_silent({ summary(res) })
expect_silent({ show(res) })
expect_silent({ collective_anomalies(res) })
expect_silent({ plot(res,variate_names=TRUE) }) ## TODO - capture both types of plotting


