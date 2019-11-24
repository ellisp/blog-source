---
layout: post
title: Cost-benefit analysis in R
date: 2019-11-24
tag: 
   - Simulations
   - Tools
   - R
description: A small random sample will give better results than a much larger non-random sample, under certain conditions; but more importantly, it is reliable and controls for risk.
image: /img/0160-million.svg
socialimage: http://freerangestats.info/img/0160-million.svg
category: R
---

One type of analysis that is commonly undertaken in spreadsheets is various types of economic evaluative analysis, such as cost-benefit analysis and cost effectiveness analysis. While there is software out there - see the book [Bayesian Cost-Effectiveness Analysis with the R package BCEA](https://www.springer.com/gp/book/9783319557168), for example - that can preovide more analytic grunt, particularly when adding probabilistic elements and estimates of uncertainty to such analyses, I think it is fair to say that Excel still rules in this world. Which I find odd, given how a tool like R is much easier to use, share and debug when the analysis starts getting more complex.

In this post I'll put forward a simple method of doing the number-crunching part of [cost-benefit analysis](https://en.wikipedia.org/wiki/Cost%E2%80%93benefit_analysis)(CBA) in R. The *hard* part of CBA is data collection and the valuing of costs and benefits - I'm not going to touch on that here, just on the turning the data you've generated into estimates of the standard outputs of CBA, with appropriate sesnitity and uncertainty bounds that are awkward to produce in Excel.

Note that CBA is not the same as cost-effectiveness analysis as delivered in the BCEA R package reference above. Simply put, CBA requires the analyst to measure both benefits and costs on the same metric (invariably monetary), whereas cost-effectiveness analysis  EITHER allows the benefits to be taken as given and compares the costs of methods of achieving those benefits OR, if allowing different levels of benefit to come into the equation, measures them with a different metric to the costs (for example, if costs are in dollars, benefits might be measured in Quality-Adjusted Life Years - it's still necessary in this case to put all the benefits on a common measurement basis, it just doesn't have to be the same metric as costs).

My approach to implementing CBA is to build in uncertainty from the ground up; allowing each parameter to be treated as a random variable. All the key analytical tasks will be separated into functions, which is good for maintainability as well as efficient adaption to other cases.

Two common key metrics in CBA are:

* Net Present Value - the real value of costs in today's dollars minus the real value of benefits (where the "real" value means future values are discounted by some arbitrarily chosen discount rate - the choice of which makes a big difference to how much the analysis values either the short term or the long term)
* Internal Rate of Return - the discount rate at which benefits break even with costs

I greatly prefer the Internal Rate of Return as a measure because it avoids the omnipresent arguments about which discount rate to choose. You calculate the discount rate at which you would break even, and if that is higher than your cost of capital, and higher than the rate of return of other uses of capital, then it is worth investing in. However, it's "harder" to calculate and perhaps to explain so is perhaps less commonly presented than Net Present Value (casual observation only, I haven't collected data on this). You can in fact use Excel's solver functionality to estimate Internal Rate of Return, but it's a step up in sophistication, liable to human error, and (in my opinion) is going against the grain of how spreadsheets work, by reducing the instant flexibility of calculations you usually get.

## Calculating net present value

Let's start with a generic function to calculate the net present value of a stream of costs and benefits, given an arbitrary discount rate. Discount rates are normally described as "percents". 7% is a commonly chosen one, recommended by the Australian Treasury for instance - although why this should be the case given years of inflation and interest rates well below that level I'm not sure. See this interesting (and in my view odd) [guidance from the New Zealand Treasury](https://treasury.govt.nz/information-and-services/state-sector-leadership/guidance/financial-reporting-policies-and-guidance/discount-rates), specifying 6.0% as the default, 4.0% to be used for office and accommodation buildings, and 7.0% for ICT-related investments.

So here's an R function for estimating the net present value of an investment, given two equally-lengthed vectors of annual costs and benefits (in current prices) and a discount rate:

{% highlight R lineanchors %}
#' Net present value
#' 
#' @param discount Discount rate, as a percentage (eg "7" means 7%)
#' @param cost Vector of annual costs
#' @param benefit Vector of annual benefits
#' @param return_abs Whether or not to return the absolute value of the net present value (only likely to be
#' used if you are using this function as part of an internal rate of return calculation)
#' @details for a given stream of costs, benefits, number of years and discount rate, return the net
#' present value (discounted benefits minus discounted costs)
net_present_value <- function(discount, cost, benefit, return_abs = FALSE){
  if (length(discount) != 1 | class(discount) != "numeric"){
    stop("discount should be a single number")
  }
  
  if(length(benefit) != length(cost)){
    stop("cost and benefit should be equally-lengthed numeric vectors")
  }
  
  # convert discount percentage into a multiplier
  discount_m <- 1 - discount / 100
  
  # number of years, other than year zero, which get discounted
  m <- length(benefit) - 1
  
  npv <- sum(benefit  * discount_m ^ (0:m)) - sum(cost  * discount_m ^ (0:m))
  if(return_abs){
    npv <- abs(npv)
  }
  return(npv)
}
{% endhighlight %}

In action, here we can use it to calculate the net present value of an investment that costs $1,000,000 in its first year and $100 per year subsequently, and returns a benefit stream valued at $70,000 per year. I've done this with two different discount rates:

```
> costs <- c(1000000, rep(100, 29))
> benefits <- rep(70000, 30)
> net_present_value(discount = 7, cost = costs, benefit  = benefits)
[1] -114534.1

> net_present_value(discount = 4, cost = costs, benefit  = benefits)
[1] 234083.8
```

So we see that depending on the discount rate, this same set of costs and benefits could be either minus $115k or positive $234k. If we followed New Zealand Treasury guidance, this investment would be worthwhile if it for an office building (discount rate of 4%) but not if it were in ICT (discount rate of 7%).

One of the key reasons for using R (and in particular, functions) for this sort of analysis is how easy it is to explore assumptions. For example, it is common in Excel to laboriously run CBA for several different scenarios - in ways that are as tedious and error prone as you can imagine, and which are difficult to generalise to work for thousands of scenarios rather than just one or two. So instead of saying that we're sure the ongoing costs will be $100 per year and the benefits $70,000 per year, let's make them both uncertain estimates with Gamma distributions (hence guaranteed to be positive) with those means. Maybe the benefits will be as low as $10,000 per year, maybe as high as $200,000; and similar for the costs. If those ranges seem, high, you should probably look at the assumptions and sketchy data that is usually needed to estimate both sides of the equation in a CBA. So here's a little simulation, still only using our one function so far

{% highlight R lineanchors %}
#----------with uncertain assumptions----------
nsim <- 10000
set.seed(123)
sims <- data.frame(
  benefit_rand = rgamma(nsim, 7, 0.0001),
  cost_rand = rgamma(nsim, 10, 0.1),
  npv = NA
)
for(i in 1:nsim){
  sims[i, "npv"] <- net_present_value(discount = 4, 
                                      cost = c(1000000, rep(sims[i, "cost_rand"], 29)),
                                      benefit = rep(sims[i, "benefit_rand"], 30))
}

sims %>%
  ggplot(aes(x = npv)) +
  geom_density(fill = "steelblue", alpha = 0.1) +
  scale_x_continuous(label = dollar) +
  labs(x = "Net present value",
       title = "Impact of randomness on net present value",
       subtitle = "Investment with initial cost of $1m, mean annual costs of around $100, and mean annual benefits of around $70,000\nAnnual costs and benefits are Gamma-distributed random variables")
{% endhighlight %}

<object type="image/svg+xml" data='/img/0161-varying-npvs.svg' width='100%'><img src='/img/0161-varying-npvs.png'></object>

The 80% credibility interval for the net present value in this case is that it is somewhere between minus $320k and positive $825k, but probably above zero.

## Relationship of discount rate to internal rate of return

This functional approach is also useful for visualising the relationship between discount rate and internal rate of return. Let's go back to a fixed (non-random) value for the costs and benefits, and calculate the net present value for a whole range of discount rates:

<object type="image/svg+xml" data='/img/0161-discount-rates.svg' width='100%'><img src='/img/0161-discount-rates.png'></object>

You pays your money, and you takes your choice. In this case we can see that the net present value line hits the negative area with a discount value of around 6%. If your discount rate or your cost of capital is higher than that, that's a "no" for the project; if it's lower, then it makes sense to undertake the project.

Code for that chart:

{% highlight R lineanchors %}
discounts <- seq(from = 0, to = 10, length.out = 100)
npvs <- sapply(discounts, net_present_value, cost = costs, benefit = benefits)

tibble(npv = npvs,
       discount = discounts) %>%
  ggplot(aes(x = discount, y = npvs)) +
  annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymax = 0, ymin = -Inf, fill = "red", alpha = 0.2) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  labs(x = "Discount rate",
       y = "Net Present Value",
       title = "Impact of discount rate on net present value",
       subtitle = "Investment with initial cost of $1m, annual costs of $100, and annual benefits of $70,000")
{% endhighlight %}

We can use the R `optimise()` function just like the Excel "solver" to calculate the actual value at which the net present value hits zero. For convenience (remember, this is all about why R is easier than Excel for this sort of thing), I'm going to wrap this in my own `internal_rate_return()` function:

```
internal_rate_return <- function(cost, benefit, interval = c(-25, 25)){
  opt <- optimise(net_present_value,
                 interval = interval, 
                 cost = cost,
                 benefit = benefit,
                 return_abs = TRUE)
  return(opt$minimum)
}

internal_rate_return(costs, benefits)
```

So our internal rate of return turns out to be 5.84. Because we've made a function out of this internal rate of return calculation, it's just as easy to calculate this for our 10,000 simulations as it was the NPV. Try doing that in Excel (no, not really don't - I'm sure it can be done, my point is just that it would be very fiddly).

<object type="image/svg+xml" data='/img/0161-varying-irr.svg' width='100%'><img src='/img/0161-varying-irr'></object>

{% highlight R lineanchors %}
#-------------IRR with random costs and benefits--------

for(i in 1:nsim){
  sims[i, "irr"] <- internal_rate_return(cost = c(1000000, rep(sims[i, "cost_rand"], 29)),
                                         benefit = rep(sims[i, "benefit_rand"], 30))
}

sims %>%
  ggplot(aes(x = irr / 100)) +
  geom_density(fill = "steelblue", alpha = 0.1) +
  scale_x_continuous(label = percent) +
  labs(x = "Internal rate of return",
       title = "Impact of randomness on internal rate of return",
       subtitle = "Investment with initial cost of $1m, mean annual costs of around $100, and mean annual benefits of around $70,000\nAnnual costs and benefits are Gamma-distributed random variables")
{% endhighlight %}
	   



{% highlight R lineanchors %}

{% endhighlight %}
	   



<object type="image/svg+xml" data='/img/0160-million-abs.svg' width='100%'><img src='/img/0160-million-abs.png'></object>


	   

{% highlight R lineanchors %}

{% endhighlight %}
	   
