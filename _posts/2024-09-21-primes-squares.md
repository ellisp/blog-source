---
layout: post
title: Prime numbers as sums of three squares.
date: 2024-09-21
description: I explore the number of ways to make a prime number as the sum of squares of three positive integers.
image: /img/0280-primes-squares-k100.svg
socialimage: https:/freerangestats.info/img/0280-primes-squares-k100.png
category: R
---
I was interested by a [LinkedIn post about the number 397](https://www.linkedin.com/posts/fermatslibrary_397-is-conjectured-to-be-the-largest-prime-activity-7242947116719915008-BRz7?utm_source=share&utm_medium=member_desktop):

> "397 is conjectured to be the largest prime that can be represented uniquely as the sum of three positive squares"

That is, 3^2 + 8^2 + 18^2 = 397

This led to some confusion in the comments as people found other prime numbers that can be created as the sum of three squares. But the wording is sloppy; better wording would be:

> "397 is conjectured to be the largest prime that can be represented as the sum of three positive squares of integers in exactly one way"

Let's confirm that. The only method I know for something like this is brute force. I can make a data frame with three columns, each with all the squares of positive integers up to some maximum point - so the data frame has every combination of those, discarding duplicate combinations by making the order of the columns strictly non-decreasing. Then we sum those three squares, and store the results:

{% highlight R lineanchors %}
library(primes)
library(tidyverse)
library(glue)

k <- 30
squares <- (1:k) ^ 2
primes <- tibble(p = generate_primes(max = k^2))

# This is the part that gets slower with larger k as you make k^3 combinations
s3s <- expand_grid(s1 = squares, s2 = squares, s3 = squares) |>
  filter(s2 >= s1 & s3 >= s2) |> 
  mutate(sum_3_sq = s1 + s2 + s3)

# example:
filter(s3s, sum_3_sq == 397)
{% endhighlight %}

This gets us our one combination that adds up to 397:
```
     s1    s2    s3 sum_3_sq
  <dbl> <dbl> <dbl>    <dbl>
1     9    64   324      397
```

Next step is to count the number of times each result appears.

{% highlight R lineanchors %}
s3s_sum <- s3s |>
  group_by(sum_3_sq) |>
  summarise(number_3_square_sums = n())

# example:
s3s_sum |>
  filter(number_3_square_sums == 3) |>
  slice(1) |>
  left_join(s3s, by = "sum_3_sq")
{% endhighlight %}

So for example we see that 54 (which of course is not a prime - we haven't yet filtered to primes) can be made 3 ways: as the sum of the squares of 1, 2, 7; of 2, 5, 5; and 3, 3, 6:

```
  sum_3_sq number_3_square_sums    s1    s2    s3
     <dbl>                <int> <dbl> <dbl> <dbl>
1       54                    3     1     4    49
2       54                    3     4    25    25
3       54                    3     9     9    36
```

Then it's a simple matter of joining that summary (of counts of the number of ways to get a given total of three squares) to a data frame of the prime numbers, and drawing a plot of the results:
{% highlight R lineanchors %}
res <- primes |>
  left_join(s3s_sum, by = c("p" = "sum_3_sq")) |>
  mutate(number_3_square_sums = replace_na(number_3_square_sums, 0))

ggplot(res, aes (x = p, y = number_3_square_sums)) +
  geom_point() +
  annotate("point", colour = "red", shape = 1, size = 4, x = 397, y = 1) +
  annotate("text", colour = "red", label = "397", x = 500, y = 1, hjust = 0) +
  scale_x_continuous(label = comma) +
  labs(x = "Prime number",
       y = "Number of ways",
       title = "Number of ways to make a prime number as sum of three positive squares of integers",
       subtitle = glue("397 (circled) is the largest with exactly one way, of primes up to {comma(k^2)}."))

if(max(res$number_3_square_sums) < 11){
  p <- p + 
    scale_y_continuous(breaks = 0:10) +
    theme(panel.grid.minor.y = element_blank())
}
{% endhighlight %}

There's a bit of fiddling there to make the charts look nice for low values of k (where k is the maximum number I square). In my real life code the above is surrounded by a loop of different values of k, with the results saved as SVG and PNG images for use in this blog. All of which gets me this result:

<object type="image/svg+xml" data='/img/0280-primes-squares-k30.svg' width='100%'><img src='/img/0280-primes-squares-k30.png' width='100%'></object>

And here we see for some larger results of k:
<object type="image/svg+xml" data='/img/0280-primes-squares-k100.svg' width='100%'><img src='/img/0280-primes-squares-k100.png' width='100%'></object>

As k gets bigger of course the program gets slower to run. This method doesn't scale well; for primes above about 250,000 the step of making a data frame of all the combinations of three squares starts taking too long. If I wanted to extend this further I'd have to find some ways to do this more efficiently, or put that step into a database that can handle bigger-than-memory data objects.

I'm sure there's some interesting maths behind why this is just a "conjecture" and no-one has been able to prove it!

<object type="image/svg+xml" data='/img/0280-primes-squares-k300.svg' width='100%'><img src='/img/0280-primes-squares-k300.png' width='100%'></object>

That's all for today.
