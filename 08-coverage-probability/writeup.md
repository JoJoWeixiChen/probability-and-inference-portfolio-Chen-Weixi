writeup
================

# Coverage probability

Definition of coverage probability: In statistics, the coverage
probability of a technique for calculating a confidence interval is the
proportion of the time that the interval contains the true value of
interest.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(stats4)
```

## step 1

Generate a single sample from a standard normal distribution of size N =
201. Explain to the reader how you use MLE to estimate the distribution.

``` r
# I plan to generate a sample of size 201 from the standard normal distribution and use the MLE method to estimate the distribution.
set.seed(111)
single_sample = rnorm(201, 0, 1)
fun <- function(a, b)
  sum(-log(dnorm(single_sample, a, b)))
z <- mle(minuslogl = fun, start = list(a = 0, b = 1))
mean = coef(z)[1]
sd = coef(z)[2]
mean
```

    ##           a 
    ## -0.01625532

``` r
sd
```

    ##        b 
    ## 1.037693

## step 2

Show the reader how you approximate the sampling distribution of the
median, conditional on the estimate of the distribution in the previous
step.

``` r
# I plan to generate sample based on the parameter estimations I got in the previous step and use the median value of the group of samples to obtain the sampling distribution of the median.
median_sample <- c()
for (i in 1:100) {
  x = median(rnorm(1000, mean, sd))
  median_sample <- c(median_sample, x)
}
hist(median_sample, main = "The sampling distribution of the median")
```

![](writeup_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## step 3

Describe how you calculate a 95% confidence interval from the
approximated sampling distribution.

``` r
# I use the quantile function to calculate the 2.5% and 97.5% quantile of data, which represents the middle 95% of data, and therefore is the 95% confidence interval of the median value.
ci = quantile(median_sample, probs = c(0.025, 0.975))
ci
```

    ##        2.5%       97.5% 
    ## -0.09332827  0.06888463

## step 4

Explain the concept of coverage probability. Explain your code for
calculating the coverage probability.

Coverage probability: In statistics, the coverage probability of a
technique for calculating a confidence interval is the proportion of the
time that the interval contains the true value of interest.

``` r
# Use MLE method to calculate the estimated value of mean and stand deviation, and then calculate the 95% confidence interval based on that. And check if the true value is located in the interval. Repeat the previous steps and calculate the probability that the true value falls into the interval.
set.seed(1)
coverage <- c()
for (i in 1:1000) {
  data <- rnorm(201, 0, 1)
  
  mean = mean(data)
  sd = sqrt(length(data-1)/length(data)*var(data))
  
  median_sample <- c()
  for (i in 1:100) {
  x = median(rnorm(201, mean, sd))
  median_sample <- c(median_sample, x)
  }
  
  ci = quantile(median_sample, probs = c(0.025, 0.975))
  
  coverage <- c(coverage, ci[1] <= 0 & ci[2] >= 0)
}

mean(coverage)
```

    ## [1] 0.973

``` r
# Use the same methods and steps to calculate the coverage probability. But in this situation, I try to use the bootstrap method to calculate the coverage probability, which means I obtain the estimated mean and standard deviation value based on the sample data collected from the origin sample data with a replaced sampling method.
set.seed(2)
coverage <- c()
for (i in 1:1000) {
  data <- rnorm(201, 0, 1)
  
  median_sample <- c()
  for (i in 1:100) {
  x = median(sample(data, size = 201, replace = TRUE))
  median_sample <- c(median_sample, x)
  }
  
  ci = quantile(median_sample, probs = c(0.025, 0.975))
  
  coverage <- c(coverage, ci[1] <= 0 & ci[2] >= 0)
}

mean(coverage)
```

    ## [1] 0.935

# Step 5

Perform the simulation and report the results.

In the code below, I run the simulation and find that the coverage
probabilities are 97.3% and 93.5%, respectively. So I can conclude that
the method I used to construct the 95% confidence interval of median
value holds a probability around 95% to capture the real value of the
parameters we are interesed in. Also, we can find that when we use
bootstrap method, the coverage probability tends to be lower, so it will
be better to use sampling data if we can get it.

# Step 6

Describe how you might change the simulation to learn more about the
operating characteristics of your chosen method for constructing the 95%
confidence interval.

I want to change the sampling amount to learn more about the chosen
method for constructing the 95% confidence interval.

``` r
# The bootstrap method
set.seed(3)
coverage <- c()
for (i in 1:1000) {
  data <- rnorm(301, 0, 1)
  
  median_sample <- c()
  for (i in 1:100) {
  x = median(sample(data, size = 301, replace = TRUE))
  median_sample <- c(median_sample, x)
  }
  
  ci = quantile(median_sample, probs = c(0.025, 0.975))
  
  coverage <- c(coverage, ci[1] <= 0 & ci[2] >= 0)
}

mean(coverage)
```

    ## [1] 0.936

``` r
# Obtain data from the rnorm function
set.seed(4)
coverage <- c()
for (i in 1:1000) {
  data <- rnorm(301, 0, 1)
  
  mean = mean(data)
  sd = sqrt(length(data-1)/length(data)*var(data))
  
  median_sample <- c()
  for (i in 1:100) {
  x = median(rnorm(301, mean, sd))
  median_sample <- c(median_sample, x)
  }
  
  ci = quantile(median_sample, probs = c(0.025, 0.975))
  
  coverage <- c(coverage, ci[1] <= 0 & ci[2] >= 0)
}

mean(coverage)
```

    ## [1] 0.969

From the code above, we can see that the method we used to construct the
confidence interval still holds a very high coverage probability when we
use change the amount of sample. So we can conclude that the method we
used to calculate the confidence interval is effective.
