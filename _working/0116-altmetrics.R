# Idea is to explore rankings as per https://twitter.com/altmetric/status/956736960923516933
# AS David Friggens said, 2 problems - top 100 rank of about 2m articles is going to be very 
# random even if altmetric and WoS highly correlated.  And 2017 is too recent for articles
# to have had many citations.

# about 2m articles written per year probably
n <- 2000000
x <- rnorm(n)
y <- scale(x +rnorm(n))
cor(x, y)

top_x <- which(rank(x) <= 100)
top_y <- which(rank(y) <= 100)
sum(top_x %in% top_y)
