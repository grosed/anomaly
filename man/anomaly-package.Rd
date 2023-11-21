  
\name{anomaly-package}
\alias{anomaly-package}
\alias{anomaly}
\docType{package}
\title{
Detecting Anomalies in Data
}

\description{
The \pkg{anomaly} package provides methods for detecting collective and point anomalies in both univariate and multivariate settings.
}

\section{Introduction}{
The \pkg{anomaly} package implements a number of recently proposed methods for anomaly detection. For univariate data there is the Collective And Point Anomaly (CAPA)
method of Fisch, Eckley and Fearnhead (2022a), which can detect both collective and point anomalies. For multivariate data there are three methods, the multivariate extension of CAPA
of Fisch, Eckley and Fearnhead (2022b), the Proportion Adaptive Segment
Selection (PASS) method of Jeng, Cai and Li, and a Bayesian approach, Bayesian Abnormal
Region Detector, of Bardwell and Fearnhead.

The multivariate CAPA method and PASS are similar in that, for a given segment they use a likelihood-based approach to measure the evidence that it is anomalous for each component of the
multivariate data stream, and then merge this evidence across
components. They differ in how they merge this evidence, with PASS using
higher criticism (Donoho and Jin) and CAPA using
a penalised
likelihood approach. One disadvantage of the higher criticism approach for merging evidence is that it can lose power when only one or a very small number of components are anomalous. Furthermore,
CAPA also allows for point anomalies in otherwise normal segments of data, and can be more robust to detecting collective anomalies when there are point anomalies in the data. CAPA can also allow for
the anomalies segments to be slightly mis-aligned across different components. 

The BARD method considers a similar model to that of CAPA or PASS, but is Bayesian and so its basic output are samples from the posterior distribution for where the collective anomalies are, and which
components are anomalous. It does not allow for point anomalies. As with any Bayesian method, it requires the user to specify suitable priors, but the output is more flexible, and can more directly
allow for quantifying uncertainty about the anomalies.
}


\references{
\insertRef{2018arXiv180601947F_a}{anomaly}
  
\insertRef{2019MVCAPA_b}{anomaly}

\insertRef{10.1093/biomet/ass059}{anomaly}

\insertRef{bardwell2017}{anomaly}

\insertRef{donoho2004}{anomaly}
}


\examples{
# Use univariate CAPA to analyse simulated data
library("anomaly")
set.seed(0)
x <- rnorm(5000)
x[401:500] <- rnorm(100, 4, 1)
x[1601:1800] <- rnorm(200, 0, 0.01)
x[3201:3500] <- rnorm(300, 0, 10)
x[c(1000, 2000, 3000, 4000)] <- rnorm(4, 0, 100)
x <- (x - median(x)) / mad(x)
res <- capa(x)
# view results
summary(res)
# visualise results
plot(res)

# Use multivariate CAPA to analyse simulated data
library("anomaly")
data("simulated")
# set penalties
beta <- 2 * log(ncol(sim.data):1)
beta[1] <- beta[1] + 3 * log(nrow(sim.data))
res <- capa(sim.data, type= "mean", min_seg_len = 2,beta = beta)
# view results
summary(res)
# visualise results
plot(res, subset = 1:20)

# Use PASS to analyse simulated mutivariate data
library("anomaly")
data("simulated")
res <- pass(sim.data, max_seg_len = 20, alpha = 3)
# view results
collective_anomalies(res)
# visualise results
plot(res)
\donttest{
# Use BARD to analyse simulated mutivariate data
library("anomaly")
data("simulated")
bard.res <- bard(sim.data)
# sample from the BARD result
sampler.res <- sampler(bard.res, gamma = 1/3, num_draws = 1000)
# view results
show(sampler.res)
# visualise results
plot(sampler.res, marginals = TRUE)
}
}





