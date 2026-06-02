
# Generate one family and return which one is a boy
# Could do this much faster by just using a geometric distribution,
# but that is kind of assuming the result we are trying to demonstrate!
one_fam <- function(n = 8, prob_boy = 0.512){
  fam <- sample(c("b", "g"), replace = TRUE, n, prob = c(prob_boy, 1 - prob_boy))
  first_boy <- grep("b", fam)[1]
  if(is.na(first_boy)){first_boy <- Inf}
  return(first_boy)
}

n <- 8
system.time({
  x <- replicate(1e7, one_fam(n = n))
})
 
tab <- table(x)

data.frame(girls = 0:n, 
           boys = c(rep(1, n), 0),
           n = as.numeric(tab))

boys <- sum(tab[-length(tab)])
girls <- sum(0:n * tab)
boys / (boys + girls) * 100


# more efficient versiuon
x <- rgeom(1e7, 0.512) # number of girls before first boy
x[x > 7] <-  8
tab <- table(x)
number_girls <- as.numeric(names(tab))
data.frame(girls = number_girls, 
           boys = c(rep(1, length(number_girls) - 1), 0),
           n = as.numeric(tab))

boys <- sum(tab[-length(tab)])
girls <- sum(number_girls * tab)
boys / (boys + girls) * 100
