library(pscl)
data(AustralianElectionPolling)
lattice::xyplot(ALP ~ startDate | org,
                data=AustralianElectionPolling,
                layout=c(1,5),
                type="b",
                xlab="Start Date",
                ylab="ALP")
## test for house effects
y <- AustralianElectionPolling$ALP/100
v <- y*(1-y)/AustralianElectionPolling$sampleSize
w <- 1/v
m1 <- mgcv::gam(y ~ s(as.numeric(startDate)),
                weight=w,
                data=AustralianElectionPolling)
m2 <- update(m1, ~ . + org)
anova(m1,m2, test = "Chisq")
summary(m2)
