
# inspired by Richard McElreath tweet (reposted on Bluesky at
# https://bsky.app/profile/rmcelreath.bsky.social/post/3lvx73m6jc22o)
#
# This is all about the fact that p-values under a null hypothesis don't
# have a uniform distribution when the test under consideration has some 
# strong discrete element.


library(tidyverse)
library(corpora)
library(GGally)
library(glue)

#' Version of fisher.pval that won't break if k1 and k2 both 0
tough_fisher <- function(k1, n1, k2, n2, set_both_zero = 1, ...){
  problems <- k1 + k2 == 0
  k1[problems] <- 1
  temp <- fisher.pval(k1, n1, k2, n2)
  temp[problems] <- set_both_zero
  return(temp)
}
# note need a decision on what to do when k1 and k2 are both zero. I think
# p value is 1 here, as p is the "probability of seeing data at least as extreme
# as this if the null hypothesis of no difference is true". Not sure why Fisher's
# exact test returns an error, worth looking into that. My 'hand' method below
# gives NaN because the estimated variance is 0 andyou have 0/0, probably similar
# problem to that.



# Takes about 30 seconds with 100,000 reps, but a long time with 10 million reps
st <- system.time({
  for(reps in c(1e3, 1e5, 1e7)){
    set.seed(42)
    
    d <- expand_grid(
      prob = rep(c(0.2, 0.5, 0.8), each = reps),
      size = c(10, 100, 1000, NA)
    ) |> 
      mutate(size_lab = ifelse(is.na(size), "n=Pois(100)", glue("n={size}")),
             prob_lab = glue("Prob={prob}")) |> 
      mutate(size = ifelse(is.na(size), rpois(n(), 100), size)) |> 
      mutate(x1 = rbinom(n(), size = size, prob = prob),
            x2 = rbinom(n(), size = size, prob = prob),
            # observed proportions p1 and p2 from the two populations
            p1 = x1 / size,
            p2 = x2 / size,
            # under null hypothesis, the equal probability of both pops:
            pmid = (p1 + p2) / 2,
            # observed difference of the two proportions:
            delt = abs(p1 - p2),
            # variance of each of p1 and p2: p(1-p)/n
            s1 = pmid * (1 - pmid) / size,
            # standard deviation of the sum of two of those variances
            sddelt = sqrt(s1 + s1)) |> 
      # calculate p values
      mutate(pval_hand = 2 * (1 - pnorm(delt / sddelt)),
             pval_fisher = tough_fisher(x1, size, x2, size),
             pval_proptest = NA)
  
    if(reps < 10000){
      # when the number of reps is fairly small, I draw a pairs plot just to
      # compare the different ways of calculating p values:
      # - prop.test (out of the box R method). I couldn't find an easy way to vectorize this, 
      #   is why it is only done here, via a loop. when reps is small
      # - pval_hand (my home made approximate method)
      # - pval_fisher (Fisher exact test, but toughened up as above to give 1 when both k1 and k2 are 0)
      # The main conclusion from this is that my by-hand aproximation isn't great!
      system.time({
        for (i in 1:nrow(d)){
          x <- d[i, ]
          d[i, "pval_proptest"] <- prop.test(c(x$x1, x$x2), c(x$size, x$size))$p.value
        }
      })
      # 5 seconds for reps=1000
      
      plot1 <- function(){
        d |> 
          select(pval_hand:pval_proptest, size_lab) |> 
          ggpairs() |> 
          print()
      }
      
      svg_png(plot1, glue("../img/0299-pairs-{reps}"), w = 10, h = 8)      
    }
  
  
    plot2 <- d |> 
      ggplot(aes(x = pval_fisher)) +
      facet_grid(size_lab  ~ prob_lab, scales = "free_y") +
      geom_histogram(fill = "steelblue") +
      scale_y_continuous(label = comma) +
      labs(title = "Distribution of all p values when a null hypothesis is true",
          subtitle = "Equal size binomial samples drawn from two populations with same underying probability",
          x = "P value from Fisher's exact test
    (when zero positive cases in both samples, p value is set to one)",
          y = glue("Count of simulations (out of {comma(reps)})"))
  
    plot3 <- d |>
    filter(pval_fisher < 0.05) |> 
    ggplot(aes(x = pval_fisher)) +
    facet_grid(size_lab  ~ prob_lab, scales = "free_y") +
    geom_histogram(fill = "steelblue") +
    scale_y_continuous(label = comma) +
    labs(title = "Distribution of significant (<0.05) p values when a null hypothesis is true",
         subtitle = "Equal size binomial samples drawn from two populations with same underying probability",
         x = "P value from Fisher's exact test
  (when zero positive cases in both samples, p value is set to one)",
         y = glue("Count of simulations (out of {comma(reps)})"))
  
      
    svg_png(plot2, glue("../img/0299-histogram-{reps}"), w = 9, h = 6)     
    svg_png(plot3, glue("../img/0299-histogram-sig-only-{reps}"), w = 9, h = 6)      
  
  }
})

print(st)

d |> 
  group_by(prob_lab, size_lab) |> 
  summarise(number_p_values = length(unique(pval_fisher))) |> 
  spread(prob_lab, number_p_values)



