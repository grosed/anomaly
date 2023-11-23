#  **News**

Latest version on CRAN is 4.3.2. This version is also
available [here](https://github.com/grosed/anomaly/tree/4.3).

## anomaly 4.3

- Updated documentation
- Replaced maggritr style %>% with base |>
- Removed dependency on magrittr

## anomaly 4.2.0

- Removed vignettes due to CRAN package size issues.
- Fixed issues regarding tests and CRAN win-builder
- Decreased transparency of point anomalies in plots.
- Resolved conflicts between Rcpp and .Call mechanisms which were causing problems with the clang compiler.
- Added gamma correction to multivariate methods

## anomaly 4.1.0

- Amalgamated capa.uv, capa.mv, scapa.uv, and scapa.mv into a single capa method.
- Removed the transform parameter from capa, pass, and bard.
- Added a summary function for bard.class type.
- Corrected mistake in pass documentation.
- Added more details to the capa documentation.
- Removed default NULL argument values for beta and beta_tilde.
- Added tests based on examples in vignettes.
- Added JSS paper draft as vignettes.
- Added missing gamma correction factor from mean/variance point anomaly cost function.
- Changed transparency level of various plot elements.
- Updated references.











