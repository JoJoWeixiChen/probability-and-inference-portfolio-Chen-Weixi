writeup
================

# 11-correlation-power

``` r
library(ggplot2)
power_correlation <- function(N, rho){
  set.seed(20394)
suppressPackageStartupMessages(require(mvtnorm))
null_correlation <- 0.8
R <- 5000

sigma <- array(c(1,rho,rho,1), c(2,2))
mu <- c(0,0)

detect <- rep(NA, R)
for(i in 1:R){
  data <- rmvnorm(N, mean = mu, sigma = sigma)
  results <- cor.test(x = data[,1], y = data[,2], alternative = "greater")
  detect[i] <- results$conf.int[1] > null_correlation
}
power <- mean(detect)
}
```

``` r
N <- c(25, 50, 75, 100)
correlation <- seq(0.8,0.96,0.02)
data <- expand.grid(N = N, correlation = correlation, power = NA)
for (i in 1:length(data[,1])) {
  data$power[i] = power_correlation(data[i,1], data[i,2])
}
```

``` r
label <- data.frame(correlation = c(0.88, 0.88, 0.88, 0.88),
                    power = c(0.3922, 0.6210, 0.7806, 0.8636),
                    label = c("25", "50", "75", "N=100"))
cols <- c("25" = "black", "50" = "red", "75" = "green", "100" = "blue")
ggplot(data, aes(x = correlation, y = power, group = N, colour = factor(N), fill = factor(N))) +
  geom_line() +
  geom_label(data = label, aes(label = label)) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = seq(0.80, 0.96, 0.02)) +
  scale_y_continuous(breaks = seq(0.0, 1.0, 0.2)) +
  xlab("Correlation") +
  ylab("Power") +
  theme_bw() +
  theme(legend.position = "None",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
```

![](writeup_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
