# All calculations, simulations, tables and figures in the main
# article may be reproduced by running the following code in R.




lam0 <- (210 / 470) * 13
lam1 <- (260 / 470) * 13
cbind(lam0, lam1)
logLRobs <- 2 * (dpois(13, 13, log = TRUE) -
              (dpois(13, lam0, log = TRUE) + dpois(0, lam1, log = TRUE)))
logLRobs
1 - pchisq(logLRobs, df = 1)
set.seed(20180226)
logLRsim <- 0
for (i in 1:20000000){
  x <- rpois(1, lam0)
  y <- rpois(1, lam1)
  lam0sim <- (x + y) / (210 + 260)
  logL0 <- dpois(x, lam0sim * 210, log = TRUE) + dpois(y, lam0sim * 260, log = TRUE)
  logL1 <- dpois(x, x, log = TRUE) + dpois(y, y, log = TRUE)
  logLRsim[i] <- 2 * (logL1 - logL0)
}
no_exceeding <- sum(logLRsim >= logLRobs)
no_exceeding
mu0 <- (14 / (210 + 260)) * 210
mu1 <- (14 / (210 + 260)) * 260
cbind(mu0,mu1)

logLRperturb <- 2 * ((dpois(13, 13, log = TRUE) + dpois(1, 1, log = TRUE))-
                  (dpois(13, mu0, log = TRUE) + dpois(1, mu1, log = TRUE)))
logLRperturb

1 - pchisq(logLRperturb, df = 1)

mon <- c(9, 1, 6, 8, 10, 12, 9, 8, 8, 10, 3, 1, 4)
yr <- c(1981, 1984, 1987, 1987, 1987, 1987, 1988, 1990, 1991, 1992, 1993, 1996, 1996)
months <- 12 *(yr - 1979) + mon
rbind(mon, yr, months)
scan_stat <- function(months, window){
  sum_window <- 0
  for (j in 1:(210 - window + 1)){
    sum_window[j] = sum((j <= months) & (months < j + window))
  }
  max(sum_window)
}
n_sim <- 10000
max_window <- 18
max_stat_mat <- matrix(0, n_sim, max_window)
for (j in 1:n_sim){
  N <- rpois(1, 13)
  months_sim <- sort(sample(1:210, size=N, repl=FALSE))
  for (i in 1:max_window){
    max_stat_mat[j,i] <- scan_stat(months_sim, i)
  }
}
p_vals <- 0
stat_obs <- 0
window <- 1:max_window
for (k in window) {
  stat_obs[k] <- scan_stat(months, k)
  p_vals[k] <- mean(max_stat_mat[, k] >= stat_obs[k])
}
cbind(window, stat_obs, p_vals)
unadj_pval <- min(p_vals)
unadj_pval
M_sim <- 10000
pvals_sim <- 0
min_pval_sim <- 0
for (a in 1:M_sim){
  N <- rpois(1, 13)
  months_sim <- sort(sample(1:210, size = N,repl = FALSE))
  stat_obs_sim <- 0
  for (b in window){
    stat_obs_sim[b] <- scan_stat(months_sim,b)
    pvals_sim[b] <- mean(max_stat_mat[,b] >= stat_obs_sim[b])
  }
  min_pval_sim[a] <- min(pvals_sim)
}
adj_pval <- mean(min_pval_sim <= unadj_pval)
adj_pval
tabl <- cbind(window, stat_obs, p_vals)
tabl
hist(min_pval_sim, pr = TRUE, breaks = (0:20) / 20,
     main="Simulated minimum p-values (observed in marked in red)")
abline(v = unadj_pval, col = "red")
