## tests for the bard and bard.sampler class objects

## basic tests to check things run - bard class
data(simulated)
expect_silent({ bard.res<-bard(sim.data, alpha = 1e-3, h = 0.5) })
expect_silent({ show(bard.res) })

## basic tests to check things run - bard.sampler class
expect_silent({ sampler.res<-sampler(bard.res) })
expect_silent({ show(sampler.res) })
expect_silent({ summary(sampler.res) })
expect_silent({ collective_anomalies(sampler.res) })
expect_silent({ plot(sampler.res,marginals=TRUE) }) ## TODO - capture both types of plotting

