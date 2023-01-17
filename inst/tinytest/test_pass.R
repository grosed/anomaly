## tests for pass class objects

## basic tests to check things run - low dimension data
## TODO - pass doesn't allow for univariate data at the moment - fails with alpha error why?
data(simulated)
expect_silent({ res<-pass(sim.data[,1:3])})
expect_silent({ summary(res) })
expect_silent({ show(res) })
expect_silent({ collective_anomalies(res) })
expect_silent({ plot(res,variate_names=TRUE) }) ## TODO - capture both types of plotting


## basic tests to check things run - multivariate data
data(simulated)
expect_silent({ res<-pass(sim.data)})
expect_silent({ summary(res) })
expect_silent({ show(res) })
expect_silent({ collective_anomalies(res) })
expect_silent({ plot(res,variate_names=TRUE) }) ## TODO - capture both types of plotting
