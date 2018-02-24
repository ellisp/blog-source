---
layout: post
title: State-space modelling of the Australian 2007 federal election
date: 2017-06-24
tag: 
   - VotingBehaviour
   - R
   - Timeseries
   - Reproducibility
description: As part of familiarising myself with the Stan probabilistic programming language, I replicate Simon Jackman's state space modelling with house effects of the 2007 Australian federal election.
image: /img/0102-all-polls.svg
socialimage: http://ellisp.github.io/img/0102-all-polls.png
category: R
---

## Pooling the polls with Bayesian statistics

In [an important 2005 article in the Australian Journal of Political Science](http://eppsac.utdallas.edu/files/jackman/CAJP%2040-4%20Jackman.pdf), Simon Jackman set out a statistically-based approach to pooling polls in an election campaign.  He describes the sensible intuitive approach of modelling a latent, unobserved voting intention (unobserved except on the day of the actual election) and treats each poll as a random observation based on that latent state space.   Uncertainty associated with each measurement comes from sample size and bias coming from the average effect of the firm conducting the poll, as well as of course uncertainty about the state of the unobserved voting intention.  This approach allows house effects and the latent state space to be estimated simultaneously, quantifies the uncertainty associated with both, and in general gives a much more satisfying method of pooling polls than any kind of weighted average.

Jackman gives a worked example of the approach in his excellent book [Bayesian Analysis for the Social Sciences](http://au.wiley.com/WileyCDA/WileyTitle/productCd-0470011548.html), using voting intention for the Australian Labor Party (ALP) in the 2007 Australian federal election for data.  He provides `JAGS` code for fitting the model, but notes that with over 1,000 parameters to estimate (most of those parameters are the estimated voting intention for each day between the 2004 and 2007 elections) it is painfully slow to fit in general purpose MCMC-based Bayesian tools such as `WinBUGS` or `JAGS` - several days of CPU time on a fast computer in 2009.  Jackman estimated his model with Gibbs sampling implemented directly in R.

Down the track, I want to implement Jackman's method of polling aggregation myself, to estimate latent voting intention for New Zealand to provide an alternative method for my [election forecasts](/elections/elections.html).  I set myself the familiarisation task of reproducing his results for the Australian 2007 election.  New Zealand's elections are a little complex to model because of the multiple parties in the proportional representation system, so I wanted to use a general Bayesian tool for the purpose to simplify my model specification when I came to it.  I use [Stan](http://mc-stan.org/) because its [Hamiltonian Monte Carlo](https://en.wikipedia.org/wiki/Hybrid_Monte_Carlo) method of exploring the parameter space works well when there are many parameters - as in this case, with well over 1,000 parameters to estimate.

Stan describes itself as "a state-of-the-art platform for statistical modeling and high-performance statistical computation. Thousands of users rely on Stan for statistical modeling, data analysis, and prediction in the social, biological, and physical sciences, engineering, and business."  It lets the programmer specify a complex statistical model, and given a set of data will return a range of parameter estimates that were most likely to produce the observed data.  Stan isn't something you use as an end-to-end workbench - it's assumed that data manipulation and presentation is done with another tool such as R, Matlab or Python.  Stan focuses on doing one thing well - using Hamiltonian Monte Carlo to estimate complex statistical models, potentially with many thousands of hierarchical parameters, with arbitrarily set prior distributions.

*Caveat! - I'm fairly new to Stan and I'm pretty sure my Stan programs that follow aren't best practice, even though I am confident they work.  Use at your own risk!*

## Basic approach - estimated voting intention in the absence of polls

I approached the problem in stages, gradually making my model more realistic.  First, I set myself the task of modelling latent first-preference support for the ALP in the absence of polling data.  If all we had were the 2004 and 2007 election results, where might we have thought ALP support went between those two points?  Here's my results:

<img src="/img/0102-no-polls.svg" width = "100%">

For this first analysis, I specified that support for the ALP had to be a random walk that changed by a normally distributed variable with standard deviation of 0.25 percentage points for each daily change.  Why 0.25?  Just because Jim Savage used it in his [rough application of this approach](http://andrewgelman.com/2016/08/06/state-space-poll-averaging-model/) to the US Presidential election in 2016.  I'll be relaxing this assumption later.


Here's the R code that sets up the session, brings in the data from Jackman's `pscl` R package, and defines a graphics function that I'll be using for each model I create.

{% highlight R %}
library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = 7)

#=========2004 election to 2007 election==============
data(AustralianElectionPolling)
data(AustralianElections)

days_between_elections <- as.integer(diff(as.Date(c("2004-10-09", "2007-11-24")))) + 1

#' Function to plot time series extracted from a stan fit of latent state space model of 2007 Australian election
plot_results <- function(stan_m){
   if(class(stan_m) != "stanfit"){
      stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")
   }
   ex <- as.data.frame(rstan::extract(stan_m, "mu"))
   names(ex) <- 1:d1$n_days
   
   p <- ex %>%
      gather(day, value) %>%
      mutate(day = as.numeric(day),
             day = as.Date(day, origin = "2004-10-08")) %>%
      group_by(day) %>%
      summarise(mean = mean(value),
                upper = quantile(value, 0.975),
                lower = quantile(value, 0.025)) %>%
      ggplot(aes(x = day)) +
      labs(x = "Shaded region shows a pointwise 95% credible interval.", 
           y = "Voting intention for the ALP (%)",
           caption = "Source: Jackman's pscl R package; analysis at https://ellisp.github.io") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line(aes(y = mean)) +
      scale_y_continuous(breaks = 31:54, sec.axis = dup_axis(name = "")) +
      theme(panel.grid.minor = element_blank())
   
   return(p)
}
{% endhighlight %}

Here's the Stan program that specifies this super simple model of changing ALP support from 2004 to 2007:

{% highlight stan %}
// oz-polls-1.stan

data {
  int<lower=1> n_days;           // number of days
  real mu_start;                 // value at starting election
  real mu_finish;                // value at final election
}
parameters {
  real<lower=0,upper=100> mu[n_days];               // underlying state of vote intention
}

model {
  
  // state model
  mu[1] ~ normal(mu_start, 0.01);
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], 0.25);
      
  // measurement model
  // 1. Election result
  mu[n_days] ~ normal(mu_finish, 0.01);
  
}
{% endhighlight %}

And here's the R code that calls that Stan program and draws the resulting summary graphic.  Stan works by compiling a program in C++ that is based on the statistical model specified in the `*.stan` file.  Then the C++ program zooms around the high-dimensional parameter space, moving slower around the combinations of parameters that seem more likely given the data and the specified prior distributions.  It can use multiple processors on your machine and works super fast given the complexity of what it's doing.

{% highlight R %}
#----------------no polls inbetween the elections------------
d1 <- list(mu_start = 37.64, mu_finish = 43.38, n_days = days_between_elections)

# returns some warnings first time it compiles; see
# http://mc-stan.org/misc/warnings.html suggests most compiler
# warnings can be just ignored.
system.time({
  stan_mod1 <- stan(file = 'oz-polls-1.stan', data = d1,
  control = list(max_treedepth = 20))
  }) # 1800 seconds

plot_results(stan_mod1) +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Latent variable estimated with no use of polling data")
{% endhighlight %}

## Adding in one polling firm

Next I wanted to add a single polling firm.  I chose Nielsen's 42 polls because Jackman found they had a fairly low bias, which removed one complication for me as I built up my familiarity with the approach.  Here's the result:

<img src="/img/0102-one-poll.svg" width = "100%">

That model was specified in Stan as set out below.  The Stan program is more complex now; I've had to specify how many polls I have (`y_n`), the values for each poll (`y_values`), and the days since the last election each poll was taken (`y_days`).  This way I only have to specify 42 measurement errors as part of the probability model - other implementations I've seen of this approach ask for an estimate of measurement error for each poll on each day, treating the days with no polls as missing values to be estimated.  That obviously adds a huge computational load I wanted to avoid.

In this program, I haven't yet added in the notion of a house effect for Nielsen.  Each measurement Nielsen made is assumed to have been an unbiased one.  Again, I'll be relaxing this later.  The state model is also the same as before ie standard deviation of the day to day innovations is still hard coded as 0.25 percentage points.

{% highlight stan %}
// oz-polls-2.stan

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election
  real mu_finish;                 // value at final election
  int<lower=1> y_n;               // number of polls
  real y_values[y_n];             // actual values in polls
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  real y_se[y_n];
}
parameters {
  real<lower=0,upper=100> mu[n_days];               // underlying state of vote intention
}

model {
  
  // state model
  mu[1] ~ normal(mu_start, 0.01);
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], 0.25);
      
  // measurement model
  // 1. Election result
  mu[n_days] ~ normal(mu_finish, 0.01);
  
  // 2. Polls
  for(t in 1:y_n)
      y_values[t] ~ normal(mu[y_days[t]], y_se[t]);
  
}
{% endhighlight %}

Here's the R code to prepare the data and pass it to Stan.  Interestingly, fitting this model is noticeably faster than the one with no polling data at all.  My intuition for this is that now the state space is constrained to being reasonably close to some actually observed measurements, it's an easier job for Stan to know where is good to explore.

{% highlight R %}
#--------------------AC Nielson-------------------
ac <- AustralianElectionPolling %>%
  filter(org == "Nielsen") %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-08")),  # ie number of days since first election; last election (9 October 2004) is day 1
         p = ALP / 100,
         se_alp = sqrt(p * (1- p) / sampleSize) * 100)

d2 <- list(
  mu_start = 37.64,
  mu_finish = 43.38,
  n_days = days_between_elections,
  y_values = ac$ALP,
  y_days = ac$MidDateNum,
  y_n = nrow(ac),
  y_se = ac$se_alp
)

system.time({
  stan_mod2 <- stan(file = 'oz-polls-2.stan', data = d2,
                   control = list(max_treedepth = 20))
}) # 510 seconds

plot_results(stan_mod2) +
   geom_point(data = ac, aes(x = MidDate, y = ALP)) +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Latent variable estimated with use of just one firm's polling data (Nielsen)")
{% endhighlight %}

## Including all five polling houses

Finally, the complete model replicating Jackman's work:

<img src="/img/0102-all-polls.svg" width = "100%">

As well as adding the other four sets of polls, I've introduced five house effects that need to be estimated (ie the bias for each polling firm/mode); and I've told Stan to estimate the standard deviation of the day-to-day innovations in the latent support for ALP rather than hard-coding it as 0.25.  Jackman specified a uniform prior on `[0, 1]` for that parameter, but I found this led to lots of estimation problems for Stan.  The Stan developers give some [great practical advice](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations) on this sort of issue and I adapted some of that to specify the prior distribution for the standard deviation of day to day innovation as `N(0.5, 0.5)`, constrained to be positive. 

Here's the Stan program:

{% highlight stan %}
// oz-polls-3.stan

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election
  real mu_finish;                 // value at final election
  
  // change the below into 5 matrixes with 3 columns each for values, days, standard error
  int y1_n;                     // number of polls
  int y2_n;
  int y3_n;
  int y4_n;
  int y5_n;
  real y1_values[y1_n];       // actual values in polls
  real y2_values[y2_n];       
  real y3_values[y3_n];       
  real y4_values[y4_n];       
  real y5_values[y5_n];       
  int y1_days[y1_n];          // the number of days since starting election each poll was taken
  int y2_days[y2_n]; 
  int y3_days[y3_n]; 
  int y4_days[y4_n]; 
  int y5_days[y5_n]; 
  real y1_se[y1_n];             // the standard errors of the polls
  real y2_se[y2_n];           
  real y3_se[y3_n];           
  real y4_se[y4_n];           
  real y5_se[y5_n];           
}
parameters {
  real<lower=0,upper=100> mu[n_days];               // underlying state of vote intention
  real d[5];                                        // polling effects
  real<lower=0> sigma;                              // sd of innovations
}

model {
  
  // state model
  mu[1] ~ normal(mu_start, 0.01); // starting point
  
  // Jackman used uniform(0, 1) for sigma, but this seems to be the cause
  // of a lot of problems with the estimation process.
  // https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  // recommends not using a uniform, but constraining sigma to be positive
  // and using an open ended prior instead.  So:
  sigma ~ normal(0.5, 0.5);              // prior for innovation sd.  
  
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], sigma);
      
  // measurement model
  // 1. Election result
  mu[n_days] ~ normal(mu_finish, 0.01);
  
  // 2. Polls
  d ~ normal(0, 7.5); // ie a fairly loose prior for house effects
  
  for(t in 1:y1_n)
      y1_values[t] ~ normal(mu[y1_days[t]] + d[1], y1_se[t]);
  for(t in 1:y2_n)
      y2_values[t] ~ normal(mu[y2_days[t]] + d[2], y2_se[t]);
  for(t in 1:y3_n)
      y3_values[t] ~ normal(mu[y3_days[t]] + d[3], y3_se[t]);
  for(t in 1:y4_n)
      y4_values[t] ~ normal(mu[y4_days[t]] + d[4], y4_se[t]);
  for(t in 1:y5_n)
      y5_values[t] ~ normal(mu[y5_days[t]] + d[5], y5_se[t]);
}
{% endhighlight %}

Building the fact there are 5 polling firms (or firm-mode combinations, as Morgan is in there twice) directly into the program must be bad practice, but seeing as there are different numbers of polls taken by each firm and on different days I couldn't work out a better way to do it.  Stan doesn't support ragged arrays, or objects like R's lists, or (I think) convenient subsetting of tables, which would be the three ways I'd normally try to do that in another language.  So I settled for the approach above, even though it has some ugly bits of repetition.

Here's the R code that sorts the data and passes it to Stan

{% highlight R %}
#-------------------all 5 polls--------------------
all_polls <- AustralianElectionPolling %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-08")),  # ie number of days since starting election
         p = ALP / 100,
         se_alp = sqrt(p * (1- p) / sampleSize) * 100,
         org = fct_reorder(org, ALP))


poll_orgs <- as.character(unique(all_polls$org))

p1 <- filter(all_polls, org == poll_orgs[[1]])
p2 <- filter(all_polls, org == poll_orgs[[2]])
p3 <- filter(all_polls, org == poll_orgs[[3]])
p4 <- filter(all_polls, org == poll_orgs[[4]])
p5 <- filter(all_polls, org == poll_orgs[[5]])


d3 <- list(
  mu_start = 37.64,
  mu_finish = 43.38,
  n_days = days_between_elections,
  y1_values = p1$ALP,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_alp,
  y2_values = p2$ALP,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_alp,
  y3_values = p3$ALP,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_alp,
  y4_values = p4$ALP,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_alp,
  y5_values = p5$ALP,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se_alp
)


system.time({
  stan_mod3 <- stan(file = 'oz-polls-3.stan', data = d3,
                    control = list(max_treedepth = 15,
                                   adapt_delta = 0.8),
                    iter = 4000)
}) # about 600 seconds

plot_results(stan_mod3) +
   geom_point(data = all_polls, aes(x = MidDate, y = ALP, colour = org), size = 2) +
   geom_line(aes(y = mean)) +
   labs(colour = "") +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Latent variable estimated with use of all major firms' polling data")
{% endhighlight %}

## Estimates of polling house effects

Here's the house effects estimated by me with Stan, compared to those in Jackman's 2009 book:

<img src="/img/0102-compare-house-effects.svg" width = "100%">

Basically we got the same results - certainly close enough anyway.  Jackman writes:

> "The largest effect is for the face-to-face polls conducted by Morgan; the point estimate of the house effect is 2.7 percentage points, which is very large relative to the classical sampling error accompanhying these polls."

Interestingly, Morgan's phone polls did much better.

Here's the code that did that comparison:

{% highlight R %}
house_effects <- summary(stan_mod3, pars = "d")$summary %>%
  as.data.frame() %>%
  round(2) %>%
  mutate(org = poll_orgs,
         source = "Stan") %>%
  dplyr::select(org, mean, `2.5%`, `97.5%`, source)

jackman <- data_frame(
   org = c("Galaxy", "Morgan, F2F", "Newspoll", "Nielsen", "Morgan, Phone"),
   mean = c(-1.2, 2.7, 1.2, 0.9, 0.8),
   `2.5%` = c(-3.1, 1, -0.5, -0.8, -1),
   `97.5%` = c(0.6, 4.3, 2.8, 2.5, 2.3),
   source = "Jackman"
)

d <- rbind(house_effects, jackman) %>%
   mutate(org = fct_reorder(org, mean),
          ypos = as.numeric(org) + 0.1 - 0.2 * (source == "Stan")) 

d %>%
   ggplot(aes(y = ypos, colour = source)) +
   geom_segment(aes(yend = ypos, x = `2.5%`, xend = `97.5%`)) +
   geom_point(aes(x = mean)) +
   scale_y_continuous(breaks = 1:5, labels = levels(d$org),
                      minor_breaks = NULL) +
   theme(panel.grid.major.y = element_blank(),
         legend.position = "right") +
   labs(x = "House effect for polling firms, 95% credibility intervals\n(percentage points over-estimate of ALP vote)",
        y = "",
        colour = "",
        caption = "Source: Jackman's pscl R package; analysis at https://ellisp.github.io")  +
   ggtitle("Polling 'house effects' in the leadup to the 2007 Australian election",
           "Basically the same results in new analysis with Stan as in the original Jackman (2009)")
{% endhighlight %}

So there we go - state space modelling of voting intention, with variable house effects, in the Australian 2007 federal election.