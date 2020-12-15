library(tidyverse)
start <- as.numeric(read_lines("day15.txt") %>% str_split(",", simplify = T))

# start <- as.numeric(c("0,3,6") %>% str_split(",", simplify = T))

n <- 3e+07
# n <- 2020


complete_possible_set <- data.frame("running_numbers" = (0:n))
running_df <- data.frame("running_numbers" = start, "when_stated" = seq(1:length(start)))

complete_possible_set <- 
  complete_possible_set %>% 
  left_join(running_df )

complete_possible_set$when_stated[19] <- NA

currnum <- start[length(start)]

##Initially did as a dataframe, very slow....

complete_possible_set_m <- as.matrix(complete_possible_set)

for (i in (length(start)):(n-1)) {
  if(is.na(complete_possible_set_m[currnum+1,2])){new_num <- 0}else{
    new_num <-  i - complete_possible_set_m[currnum + 1,2]} 
  complete_possible_set_m[currnum + 1,2] <- i
  currnum <- new_num
  if(i%%3e+05==0){print(i)}
}

currnum