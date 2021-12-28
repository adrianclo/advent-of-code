library(tidyverse)
library(zoo)

## setup ----
d1 <- scan("2021-01/input.txt")
head(d1)

## part 1 ----
which(diff(d1) > 0) %>% length()

## part 2 ----
d1_window3 <- zoo::rollapplyr(d1, width = 3, by = 1, FUN = sum, fill = NA)
head(d1_window3)

which(diff(d2_window3) > 0) %>% length()