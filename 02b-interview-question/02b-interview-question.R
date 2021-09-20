library(gmodels)
library(tidyverse)

#Define the function "one_game" to simulate the process of one selection and four flips,
#and store the results of flips into "flip". In the function, the coin1 represents the 
#normal coin and the coin2 represents the biased coin.
one_game <- function() {
  coin <- sample(c("coin1","coin1","coin1","coin1","coin2"), size = 1)
  prob <- 0.5 + 0.25*(coin == "coin2")
  flip <- rbinom(4,1,prob) %>%
          paste0(collapse = "")
  c(coin = coin, flip = flip)
}

set.seed(1)
#Replicate the game for 100000 times and store the results into a data.frame called "table".
table <- data.frame(t(replicate(1e6, one_game()))) %>%
         mutate(HHHH = 1*(flip == "1111")) 

#Create the cross table using the data simulated above.
CrossTable(table$coin, table$HHHH)
