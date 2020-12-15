library(tidyverse)
start <- as.numeric(read_lines("day15.txt") %>% str_split(",", simplify = T))

# start <- as.numeric(c("0,3,6") %>% str_split(",", simplify = T))

running_df <- data.frame("running_numbers" = start, "last_stated" = seq(1:length(start)))
running_df$last_last_stated <- c(running_df$last_stated[1:6],NA)
last_number <- tail(start,1)

for(i in (length(start)+1):2020){
  # i <- 9
  last_said <- which(running_df$running_numbers==last_number)
  if(length(last_said)==0){
    running_df <- 
      bind_rows(running_df, 
                data.frame("running_numbers"=last_number, "last_stated"=(i-1), "last_last_stated" = NA))
    last_said <- which(running_df$running_numbers==last_number)
  }
  last_number <- running_df$last_last_stated[last_said]
  last_number[is.na(last_number)] <- 0
  running_df$last_last_stated <- running_df$last_stated
  running_df$last_stated[which(running_df$running_numbers==last_number)] <- i
  # print(last_number)
  
}

last_number