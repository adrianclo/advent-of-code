library(tidyverse)

# setup ----
numbers2judge <- readLines("2021-04/input.txt", n = 1) %>% # read only first line
  str_split(",") %>%
  unlist()
head(numbers2judge)

bingo_sheets <- readLines("2021-04/input.txt")[-c(1)] %>% # skip the first line 
  str_trim()

bingo_data <- tibble(
  values = bingo_sheets[which(bingo_sheets != "")]
) %>%
  separate(values, into = paste0("col_", 1:5), sep = "\\s{1,2}") %>%
  mutate(across(col_1:col_5, ~as.numeric(.x))) %>% 
  as.matrix()
head(bingo_data)

# part 1 ----
lucky_number <- NA
row_test <- NA
col_test <- NA
col_matrix <- NA

for (ii in 1:length(numbers2judge)) {
  print(ii)
  ww <- which(bingo_data == numbers2judge[ii])
  bingo_data[ww] <- 100
  
  # check for row bingo
  row_test <- which(apply(bingo_data, 1, sum) == 500)
  if(length(row_test) > 0) { 
    lucky_number = numbers2judge[ii] %>% as.numeric()
    break() 
  }
  # check for column bingo
  col_matrix <- zoo::rollapplyr(bingo_data, width = 5, by = 6, FUN = sum, fill = NA)
  col_test <- which(col_matrix == 500)
  if(length(col_test) > 0) { 
    lucky_number = numbers2judge[ii] %>% as.numeric()
    break()
  }
}

if(length(row_test) == 1) { 
  sheet2check <- (row_test / 5) %>% ceiling() 
}
if(length(col_test) == 1) {
  sheet2check <- (which(col_matrix == 500, arr.ind = T)[1] / 5) %>% ceiling()
}

bingo_data[((sheet2check*5)-4):(sheet2check*5),] %>% 
  as_tibble() %>%
  pivot_longer(cols = everything()) %>% 
  filter(value != 100) %>% 
  pull(value) %>% sum() * lucky_number

# part 2 ----