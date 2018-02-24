---
layout: post
title: Improving state-space modelling of the Australian 2007 federal election
date: 2017-07-09
tag: 
   - VotingBehaviour
   - R
   - RobustMethods
description: I revisit the state space model of Labor party vote leading up to the 2007 Australian election; and a re-think about total survey error in the context of polling data leads to a more stable, less wiggly underlying state of voting intention.  Also, vectorization in Stan leads to much faster estimation.
image: /img/0102a-all-polls-inflator-2.svg
socialimage: http://ellisp.github.io/img/0102a-all-polls-inflator-2.png
category: R
---

After I wrote a couple of weeks back about [state-space modelling of the Australian 2007 federal election](/blog/2017/06/24/oz-polls-statespace), I received some very helpful feedback from [Bob Carpenter](http://datascience.columbia.edu/bob-carpenter), a Columbia University research scientist and one of the core developers of the Stan probabilistic language.  Some of the amendments I made in response to his comments and my evolving thinking are interesting enough (to me anyway) to blog about.

To refresh memories or introduce new readers, this exercise is all about estimating an unobserved, latent voting intention for the Australian Labor Party (ALP) between the 2004 and 2007 federal elections; using the election results at each end of that period as high quality precise measurements of that voting intention, and all the individual polls from the five main firms (in fact, four firms, with one firm publishing both face to face and by phone) between the two elections as lower quality, less precise measurements.  We include in the model an allowance for five different "house effects" - effectively, biases of the different polling efforts.

By the end of today's post, my best model of ALP voting intention over this time period looks like this:

<img src='/img/0102a-all-polls-inflator-2.svg' width='100%'>

At the end of last fortnight's blog, it looked like this, which was basically identical to the Simon Jackman model it was replicating:
<img src='/img/0102-all-polls.svg' width='100%'>

Most of the changes were under the hood to get performance improvements, or things I explored that made little difference whether I accepted them or not.  One important change in assumptions about total survey error led to the smoother latent voting curve in the eventual graphic.

So here's what I did.  Caveat - I'm marginally more competent with Stan than a fortnight ago, but still at the very early stages of the learning curve; so treat all the below with caution!

## Vectorise probability statements in Stan wherever possible

The big eye-opener for me, at this stage early in my experience with Stan, was the huge performance boost from fitting a vector of random variables in a single statement rather than one at a time with a `for` loop.  As most of my programming is in R, this should come naturally to me, but I'd absorbed the message that because Stan programs are compiled in C++, loops aren't slow in Stan the way they are in R.  It turns out this is correct, but a helpful footnote on page 123 of the helpful [user documentation](https://github.com/stan-dev/stan/releases/download/v2.16.0/stan-reference-2.16.0.pdf) explains further:

> Unlike in Python and R, which are interpreted, Stan is translated to C++ and compiled, so loops and
assignment statements are fast. Vectorized code is faster in Stan because (a) the expression tree used to
compute derivatives can be simplified, leading to fewer virtual function calls, and (b) computations that
would be repeated in the looping version, such as log(sigma) in the above model, will be computed once
and reused.

In my first version I had Stan code that looked like

{% highlight stan %}
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], 0.0025);
{% endhighlight %}

Changing this to

{% highlight stan %}
  mu[2:n_days] ~ normal(mu[1:(n_days - 1)], 0.0025);
{% endhighlight %}

and explicitly specifying `mu` as a vector in the parameter block sped up everything remarkably - for my simplest program (without any polling data and hence not a realistic exercise, but still), from 30 minutes to 4 minutes.  Similar gains came when I respecified the measurement error part of the model in vectorised terms, once I got to the models incorporating the polls.

## Don't give a parameter two priors

In my original version, I'd forced the final (2007 election day) value of the latent voting intention `mu` to match the actual 2007 election result with this code (where `n_days` is the number of days from the 2004 election to the 2007 election and `mu_finish` is the 2007 election result):

{% highlight stan %}
mu[n_days] ~ normal(mu_finish, 0.0001);
{% endhighlight %}

Dr Carpenter pointed out that this meant I declared the probability distribution of the final value of `mu` twice - once in the above, and once earlier when I'd specified it as equalling the latent voting intention the day before the election plus or minus a small random change.  In fact, I'd noticed this myself and had been surprised that it works (it does).  But it's much clearer to change from specifying the distribution of `mu[n_days]` at this point, to specifying the distribution of the election result `mu_finish`:

{% highlight stan %}
mu_finish ~ normal(mu[n_days], 0.0001);
{% endhighlight %}

This doesn't seem to make any difference to performance or the results but it makes more sense.  I hadn't done this originally because I was stuck thinking that the election result `mu_finish` was somehow particularly real and definitive, whereas latent voting intention `mu` was a random variable.  In fact, it makes perfect sense to think of (in this context) the election result as a random variable, very closely tied to the latent support for the ALP, but still imperfectly measured.  So moving it to the left of the `~` in the probability declaration statement above makes sense.

## Consider longer-tailed distributions

Dr Carpenter pointed out that my original model still showed a lot of short term up and down of latent support for the ALP.  Given there seemed to be a few periods of quite rapid change, he suggested changing the model for the daily change in support from Normal to something like Student's t distribution with four degrees of freedom, which has longer tails allowing those occasional big days of change.  Although it nearly doubles the estimation time, I think this is a good idea and I've now implemented it this way.  It doesn't seem to make any noticeable difference.

## Don't forget total survey error

However, I agreed with the suggestion that the latent voting intention in my original post (which of course was a reproduction of work published elsewhere) was too wiggly for plausibility.  In thinking this through I decided that the problem was that the probabilistic model is taking into account only a simple estimate of sampling error for individual polls (plus the house effects of bias for or against the ALP).  We know beyond reasonable doubt that there's more volatility in voting intention polls than actually in the variable we're trying to define and measure.  For example, [Trump's Up 3! Clinton's Up 9!](http://www.slate.com/articles/news_and_politics/politics/2016/08/don_t_be_fooled_by_clinton_trump_polling_bounces.html) by Andrew Gelman and David Rothschild convincingly shows how *two thirds* of apparent swings are explained by variations in response rates.  Using compelling data from the time of the 2012 US Presidential Election, they show:

> When there was good news for Mitt Romney, more Republicans opted to respond to the poll; when Obama was riding high, Democrats were more likely to respond. 

In the case of the 2007 Australian election, it seems likely (for example) that there was a surge of enthusiasm when <del>Mark Latham</del>Kim Beazley was replaced with Kevin Rudd as leader of the ALP; and that this would have translated into over-estimates of people's likelihood of actually voting for  the ALP, with ALP voters perhaps relishing the chance of telling a pollster what they think.  

Voter turnout isn't as the source of randomness in Australia (which has compulsory voting) as other jurisdictions, but it is easy to see many sources of survey error other than the simple sampling error usually reported.  This problem is simply ignored in my model of a fortnight ago - neither the systematic bias of the house effects nor the sampling errors from the standard way of calculating them from polls leaves any way for this extra variance to feature in the model.

It's difficult to know what to do about this issue.  I think the actual uncertainty from these surveys is some multiple (greater than 1) of the sampling error, but it's difficult to quantify (noting for example that the level of this bonus randomness will vary in different circumstances).  Based on my own gut feel and experience, and the scary estimate by Gelman and Rothschild quoted above, I decided to double the variance of polls from the simple estimate (this equates to multiplying confidence intervals by the square root of 2, or around 1.4).

Implementing this has the effect of giving individual polls less pull than in the original model, and we see a smoother point estimate of latent voting intention:

<img src="/img/0102a-all-polls-inflator-2.svg" width = "100%">

Now, there's quite a literature on [total survey error](https://scholar.google.co.nz/scholar?q=total+survey+error&hl=en&as_sdt=0&as_vis=1&oi=scholart&sa=X&ved=0ahUKEwiHkI-nx-fUAhXKoJQKHVVQC3wQgQMIJTAA) filled with detailed advice on all the things that go wrong, how to estimate their impact and how to control for it.  One thing I'm pretty sure it never says is "just multiply the variances by 2 and you should be right."  So few people would recommend this procedure.  But it's surely better than multiplying them by 1; few experienced researchers in this space would think that the best multiplier is closer to 1 than to 2.  Yet by reporting so widely the margin of error from simple sampling error (usually ignoring even design effects from weighting) that's what is done thousands of times every day.

In fact, further thinking leads me to the idea that a better way to do this is to *add* some variance (rather than multiply the existing variance calculated on purely sampling grounds); because it's unlikely this extra variance reduces as sample size increases, which is implied by my use of a multiplier.  This needs to be a subject of future thinking.

In case anyone is wondering, I did try turning this variance multiplier into a parameter and estimating it from the data, but this simply makes too many things for the model to estimate.  I had to impose such a tight prior on that multiplier parameter that I might as well have just specified it arbitrarily as some number (eg 2) anyway.

As well as smoothing out the state space of intended ALP vote, this had the interesting effect of leading to slightly higher estimated house effects than the original model.

## Estimate the sampling error from the underlying parameter, not just a single poll

A thing at the back of my mind as a potential weakness was that I was estimating variance for the measurement error of each poll based on its sample size and the poll's own estimate of the proportion voting for labour ie 

$$\sigma^2_\hat{\mu} = \frac{\hat{\mu}(1-\hat{\mu})}{n}$$

The *real* sampling variance of those estimates is of course:

$$\sigma^2_\hat{\mu} = \frac{\mu(1-\mu)}{n}$$

...the difference being the use of the correct value of \\(\mu\\) rather than the estimate \\(\hat{\mu}\\).  In fact, using \\(\hat{\mu}\\) from an individual poll is precisely how people usually estimate standard error from a single poll because it is the best estimate available of \\(\mu\\).  But in my case, I have a good estimate of \\(\mu\\) for every single day - a better estimate than given by any single poll.  So it makes sense to use the proportion from my latent state model and re-estimate the standard errors as those expected proportions value change through the estimation process.

Unfortunately this comes with two costs:

* it adds materially to the computation cost, complicating the convergence process
* it makes the estimation process vulnerable to going hay-wire.  When Stan is exploring values of \\(\hat{\mu}\\) that are very close to 0 or 1 it leads to estimates of polling error being numerically zero, which understandably confuses things.  Particularly when I started modelling the underlying state space on the logit scale rather than the original [0,1] interval (see next sub-section), this led to big problems and the estimation process usually failed to converge to sensible values.

In the end, I stuck with estimating the standard errors of the polls just once, outside of the actual model fitting, to avoid these problems.  I am confident the loss of accuracy from doing this is pretty minimal.

## Avoid hard boundaries for parameters (if you can...)

In my original analysis I specified the state space parameter with 

{% highlight stan %}
parameters {
  real<lower=0, upper=1> mu[n_days];  // underlying state of vote intention as a proportion
}
{% endhighlight %}

Dr Carpenter pointed out it wasn't essential to have the `<lower=0, upper=1>` constraint and that this can cause problems if there is any probability mass at all near the boundaries.

I tried removing this constraint; re-parameterising so the latent state I was modelling was the logarithm of the odds of ALP voting intention (and hence no need to be bound); with-and-without also reparameterising so the innovation is a random variable centered on zero and `mu` (other than its starting value) the cumulative sum of those innovations.  But I couldn't get the fitting process to work satisfactorally in any combination of these changes.   The problem definitely lies with me but I'll have to come back to it another time.

## Final model

Here's the minimal version of the R code that does the data preparation, passes it to Stan to fit the model, and presents the results:

{% highlight R %}
## Changes compared to previous version:
# vectorize mu
# vectorize the polls
# double the variance of the polls to account for total survey error - because worried about http://www.slate.com/articles/news_and_politics/politics/2016/08/don_t_be_fooled_by_clinton_trump_polling_bounces.html
# change the innovations to be based on student_t(4, mu, sigma) rather than normal(mu, sigma)

## Changes that were tried and either couldn't get to work or decided otherwise not to use:
# calculate the poll standard errors from mu, not the poll
# putting mu on a logit scale 
# re-parameterise so an innovation "epsilon" with mean zero is the parameter and mu is a transformed paraeter

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
#' Assumes parameter mu is the state space intent to vote ALP, on logit scale
plot_results <- function(stan_m){
   if(class(stan_m) != "stanfit"){
      stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")
   }
   ex <- as.data.frame(rstan::extract(stan_m, "mu"))
   names(ex) <- 1:d3$n_days
   
   p <- ex %>%
      gather(day, value) %>%
      mutate(day = as.numeric(day),
             day = as.Date(day, origin = "2004-10-08"),
             value = value * 100) %>%
      group_by(day) %>%
      summarise(middle = mean(value),
                upper = quantile(value, 0.975),
                lower = quantile(value, 0.025)) %>%
      ggplot(aes(x = day)) +
      labs(x = "Shaded region shows a pointwise 95% credible interval.", 
           y = "Voting intention for the ALP (%)",
           caption = "Source: Jackman's pscl R package; analysis at https://ellisp.github.io") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line(aes(y = middle)) +
      scale_y_continuous(breaks = 31:54, sec.axis = dup_axis(name = "")) +
      theme(panel.grid.minor = element_blank())
   
   return(p)
}

#-------------------all 5 polls--------------------
all_polls <- AustralianElectionPolling %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-08")),  # ie number of days since first election
         p = ALP / 100,
         org = fct_reorder(org, ALP),
         se = sqrt(p * (1 - p) / sampleSize))


poll_orgs <- as.character(unique(all_polls$org))

p1 <- filter(all_polls, org == poll_orgs[[1]])
p2 <- filter(all_polls, org == poll_orgs[[2]])
p3 <- filter(all_polls, org == poll_orgs[[3]])
p4 <- filter(all_polls, org == poll_orgs[[4]])
p5 <- filter(all_polls, org == poll_orgs[[5]])


d3 <- list(
  mu_start = 0.3764,
  mu_finish = 0.4338,
  n_days = days_between_elections,
  inflator =sqrt(2),
  y1_values = p1$p,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se,
  y2_values = p2$p,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se,
  y3_values = p3$p,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se,
  y4_values = p4$p,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se,
  y5_values = p5$p,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se
)

stan_mod3 <- stan(file = 'oz-polls-3a.stan', data = d3, chains = 4, 
				control = list(max_treedepth = 15),
				iter = 4000)

plot_results(stan_mod3) +
   geom_point(data = all_polls, aes(x = MidDate, y = ALP, colour = org), size = 2) +
   geom_line(aes(y = middle)) +
   labs(colour = "") +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Daily innovations with a Student's t distribution with 4 degrees of freedom; 
total survey variance inflated 2x usual sampling error.")
{% endhighlight %}

And here's the final Stan model, to be saved on `oz-polls-3a.stan` in the same directory as R is working from:

{% highlight stan %}
// oz-polls-3a.stan

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election, on [0,1] scale
  real mu_finish;                 // value at final election, on [0,1] scale
  real inflator;                  // amount by which to inflate the standard error of polls
  
  // information on five polls from different houses
  int y1_n;                     // number of polls by house 1
  int y2_n;
  int y3_n;
  int y4_n;
  int y5_n;
  vector[y1_n] y1_values;       // actual values in polls, in 0-1 scale
  vector[y2_n] y2_values;       
  vector[y3_n] y3_values;       
  vector[y4_n] y4_values;       
  vector[y5_n] y5_values;       
  int y1_days[y1_n];          // the number of days since starting election each poll was taken
  int y2_days[y2_n]; 
  int y3_days[y3_n]; 
  int y4_days[y4_n]; 
  int y5_days[y5_n]; 
  vector[y1_n] y1_se;             // the sample errors of the polls
  vector[y2_n] y2_se;           
  vector[y3_n] y3_se;           
  vector[y4_n] y4_se;           
  vector[y5_n] y5_se;           
}
parameters {
  vector<lower=0,upper=1>[n_days] mu;         // 
  real d[5];                      // polling effects
  real<lower=0> sigma;            // sd of innovations
}


model {
  // priors 
  sigma ~ normal(0.001, 0.001);     // prior for innovation sd.  
  mu[1] ~ beta(2, 2);               // starting state space
  d ~ normal(0, 0.05); // ie a fairly loose prior for house effects (on scale of [0,1])
  
  
  // state model
  mu[2:n_days] ~ student_t(4, mu[1:(n_days - 1)], sigma);

  // measurement model
  // 1. Election results
  mu_start ~ normal(mu[1], 0.0001);
  mu_finish ~ normal(mu[n_days], 0.0001);
  
  // 2. Polls
  y1_values ~ normal(mu[y1_days] + d[1], y1_se * inflator);
  y2_values ~ normal(mu[y2_days] + d[2], y2_se * inflator);
  y3_values ~ normal(mu[y3_days] + d[3], y3_se * inflator);
  y4_values ~ normal(mu[y4_days] + d[4], y4_se * inflator);
  y5_values ~ normal(mu[y5_days] + d[5], y5_se * inflator);

}
{% endhighlight %}

