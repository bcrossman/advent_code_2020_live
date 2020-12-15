library(tidyverse)
start <- as.numeric(read_lines("day15.txt") %>% str_split(",", simplify = T))

# start <- as.numeric(c("0,3,6") %>% str_split(",", simplify = T))

for(i in (length(start)+1):2020){
  #i <- 8
  last_number <- start[i-1]
  places_spoken <- which(start[1:(i-2)]==last_number)
  number_spoken <- ifelse(length(places_spoken)==0,0,(i-1)-max(places_spoken))
  start <- c(start, number_spoken)
  print(number_spoken)
}

number_spoken