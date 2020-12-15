library(tidyverse)
start <- as.numeric(read_lines("day15.txt") %>% str_split(",", simplify = T))

# start <- as.numeric(c("0,3,6") %>% str_split(",", simplify = T))

running_df <- data.frame("running_numbers" = start, "last_stated" = seq(1:length(start)))
running_df$last_last_stated <- c(running_df$last_stated[1:(length(start)-1)],NA)
last_number <- tail(start,1)

for(i in (length(start)+1):30000000){
  # if(i==5){break}
  last_said <- which(running_df$running_numbers==last_number)
  if(length(last_said)==0){
    running_df <- 
      bind_rows(running_df, 
                data.frame("running_numbers"=last_number, "last_stated"=(i-1), "last_last_stated" = NA))
    last_said <- which(running_df$running_numbers==last_number)
  }
  last_number <- (i-1) - running_df$last_last_stated[last_said]
  if(is.na(last_number)){
    last_number <- 0
  }
  running_df$last_last_stated <- running_df$last_stated
  running_df$last_stated[which(running_df$running_numbers==last_number)] <- i
  # print(last_number)
  if(i%%3e+05==0){print(i)}
  
}

last_number