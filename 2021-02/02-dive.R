library(tidyverse)

## setup ----
d2 <- read_delim("2021-02/input.txt", delim = " ", col_names = c("direction", "units"), col_types = c(col_character(), col_double()))
head(d2)

## part 1 ----
## horizontal
h <- d2 %>% filter(direction == "forward") %>% pull(units) %>% sum()

## depth
d <- d2 %>% 
  filter(direction != "forward") %>% 
  mutate(units = if_else(direction == "up", 
                         paste0("-", units) %>% as.numeric(), 
                         units)) %>% 
  pull(units) %>% sum()

h * d

## part 2 ----
d2 <- d2 %>% 
  mutate(id = 1:n()) %>% 
  mutate(units = if_else(direction == "up", 
                         paste0("-", units) %>% as.numeric(), 
                         units)) %>% 
  select(id, everything())

## horizontal
h <- d2 %>% 
  filter(direction == "forward") %>% 
  pull(units) %>% sum()

## depth
d <- d2 %>% 
  left_join(
    d2 %>% 
      filter(direction %in% c("up","down")) %>% 
      mutate(aim = cumsum(units)),
    by = c("id","direction","units")
  ) %>% 
  fill(aim, .direction = "down") %>% 
  mutate(aim = if_else(is.na(aim), 0, aim)) %>% 
  mutate(depth = case_when(direction == "forward" ~ units * aim)) %>% 
  pull(depth) %>% sum(na.rm = T)

h * d