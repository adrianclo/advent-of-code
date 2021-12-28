library(tidyverse)

## setup ---- 
d3 <- scan("2021-03/input.txt", what = character())
head(d3)

d3 <- d3 %>% 
  str_split(pattern = "") %>% unlist() %>% as.numeric() %>% 
  matrix(ncol = nchar(d3[1]), byrow = T)
head(d3)

## part 1 ----
## positions were proportion of 1's is more than 0's
ww <- which(apply(d3, 2, sum) / nrow(d3) > .5)

## epsilon rate
epsilon_output <- rep("0", ncol(d3))
epsilon_output[ww] <- "1"
epsilon_output <- epsilon_output %>% 
  paste(., collapse = "") %>% 
  strtoi(epsilon_output, base = 2) # to convert from binary to decimal 

## gamma rate
gamma_output <- rep("1", ncol(d3))
gamma_output[ww] <- "0"
gamma_output <- gamma_output %>% 
  paste(., collapse = "") %>% 
  strtoi(gamma_output, base = 2)

epsilon_output * gamma_output

## part 2 ----
## oxygen generator rating
oxygen <- d3
while(!is.null(nrow(oxygen))) {
  ## proportion of 1's in each position
  thresh <- (apply(oxygen, 2, sum) / nrow(oxygen))[ii]
  if(thresh == .5) {
    most_common_bit <- 1
  } else {
    most_common_bit <- round(thresh) # less than .50 = 0, more than .50 = 1
  }

  oxygen[oxygen[,ii] == most_common_bit,]
  oxygen <- oxygen[oxygen[,ii] == most_common_bit,]
  
  ii <- ii + 1
}

oxygen <- oxygen %>% 
  paste(., collapse = "") %>% 
  strtoi(base = 2)

## CO2 scrubber rating
co2 <- d3
ii <- 1
while(!is.null(nrow(co2))) {
  ## proportion of 1's in each position
  thresh <- (apply(co2, 2, sum) / nrow(co2))[ii]
  if(thresh == .5) {
    least_common_bit <- 0
  } else {
    least_common_bit <- abs(1 - round(thresh))
  }
  
  co2[co2[,ii] == least_common_bit,]
  co2 <- co2[co2[,ii] == least_common_bit,]
  
  ii <- ii + 1
}

co2 <- co2 %>% 
  paste(., collapse = "") %>% 
  strtoi(co2, base = 2)

oxygen * co2