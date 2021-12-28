library(tidyverse)

# setup ----
d5 <- readLines("2021-05/input.txt") %>% 
  unname() %>% unlist() %>% str_trim() %>% as_tibble()
head(d5)

## horizontal and vertical lines
d5_clean1 <- d5 %>% 
  separate(value, into = c("first", "second"), sep = " -> ") %>% 
  separate(first, into = c("x1","y1"), sep = ",") %>% 
  separate(second, into = c("x2","y2"), sep = ",") %>% 
  mutate(across(x1:y2, ~as.numeric(.x))) %>% 
  filter(x1 == x2 | y1 == y2) %>% 
  select(starts_with("x"), starts_with("y"))
head(d5_clean1)

## diagonal lines
d5_clean2 <- d5 %>% 
  separate(value, into = c("first", "second"), sep = " -> ") %>% 
  separate(first, into = c("x1","y1"), sep = ",") %>% 
  separate(second, into = c("x2","y2"), sep = ",") %>% 
  mutate(across(x1:y2, ~as.numeric(.x))) %>% 
  select(starts_with("x"), starts_with("y")) %>% 
  anti_join(d5_clean1,
            by = c("x1", "y1", "x2", "y2")) %>% 
  mutate(angle = (y2 - y1) / (x2 - x1)) %>% 
  filter(abs(angle) == 1)

space <- matrix(numeric(1000 * 1000), ncol = 1000)

# part 1 ----
for(ii in 1:nrow(d5_clean1)) {
  print(ii)
  coordinates <- d5_clean1[ii,] %>% unname() %>% unlist()
  space[coordinates[1]:coordinates[3], coordinates[2]:coordinates[4]] <- 
    space[coordinates[1]:coordinates[3], coordinates[2]:coordinates[4]] + 1
}

which(space >= 2) %>% length()

# part 2 ----
for(jj in 1:nrow(d5_clean2)) {
  coordinates <- d5_clean2[jj,] %>% select(-angle) %>% unname() %>% unlist()
  
  xs <- seq(coordinates[1], coordinates[3])
  ys <- seq(coordinates[2], coordinates[4])
  
  kk <- 1
  for(kk in 1:length(xs)) {
    space[xs[kk],ys[kk]] <- space[xs[kk],ys[kk]] + 1  
  }
}

which(space >= 2) %>% length()