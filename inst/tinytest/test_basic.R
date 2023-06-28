## basic tests of the correspondance to the theory in the univariate case

## testing point anomalies
expect_silent({
    x <- seq(0,5,length=1000)
    ## manual evaluation of the solution
    idx <- (x^2) > log( exp(-(1+2)) + x^2 ) + 1 + 2
    ans <- which(idx)
    ## various capa forma
    mv <- capa(x,beta=1e300,beta_tilde=2,type="meanvar")
    m <- capa(x,beta=1e300,beta_tilde=2,type="mean")
    rm <- capa(x,beta=1e300,beta_tilde=2,type="robustmean")
})
expect_equal( point_anomalies(mv)$location, ans )
expect_equal( point_anomalies(m)$location, ans )
expect_equal( point_anomalies(rm)$location, ans )


## test for detection of collective anomalies with mean
expect_silent({
    n <- 20
    beta <- 2
    x <- rep(0,n) ## should be the same length as min_seg_len
    z <- c(x,x+sqrt(beta/n))
    res <- capa(z,beta=beta,beta_tilde=1e300,type="mean")
})
expect_equal( nrow(collective_anomalies(res)), integer(1) )
expect_silent({
    z <- c(x,x+sqrt(beta+1e-200/n))
    res <- capa(z,beta=beta,beta_tilde=1e300,type="mean")
    res <- collective_anomalies(res)
})
expect_equal( c(res$start,res$end), c(21,40) )

## TODO test for collective anomaly detection for meanvar and robustmean
