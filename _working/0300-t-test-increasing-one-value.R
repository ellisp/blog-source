
x2 <- x1 <- c(1,2,3,4)
x2[1] <- 10
t.test(x1)$p.value
t.test(x2)$p.value

