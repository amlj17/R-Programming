set.seed(254)
nsim <- 50
S0 <- 100
mu <- 0.15
sigma <- 0.30
t = 365
gbm<- matrix(ncol = nsim, nrow = t)

for (simu in 1:nsim) {
  for (day in 2:t) {
    epsilon <- rnorm(t)
    dt = 1 / t
    gbm[1, simu] <- S0
    gbm[day, simu] <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon[day] * sqrt(dt))
  }
}
gbm <- apply(gbm, 2, cumprod)

ts.plot(gbm, gpars = list(col=rainbow(10)))