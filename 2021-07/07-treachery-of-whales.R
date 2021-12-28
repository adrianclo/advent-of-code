library(tidyverse)

# setup ----
d7 <- scan("2021-07/input.txt", sep = ",")
head(d7)

# part 1 ----
abs(d7 - median(d7)) %>% sum()

# part 2 ----
moves <- abs(d7 - floor(mean(d7)))

fuel <- numeric(length(moves))

for(ii in 1:length(moves)) {
    fuel[ii] <- seq(0, moves[ii]) %>% sum()
}
fuel %>% sum()
