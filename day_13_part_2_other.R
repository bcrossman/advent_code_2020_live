library(tidyverse)
library(DescTools)

data <- readLines("day13.txt")

target <- as.numeric(data[1])

times <- 
  data[2] %>% 
  as_tibble %>% 
  separate_rows(1, sep = ",") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(rowid = row_number()-1) %>% 
  filter(!is.na(value))

where_are_you <- 0

for(i in 2:nrow(times)){
  # if(i==2){break}
  #i <- 3
  multiple <- as.double(prod(c(1,times$value[1:(i-1)])))  #can just be LCM, but function fails. Lucky Primes
  
  # cycle <- which(((seq(from = where_are_you, 
                         # by =  multiple, 
                         # length.out = times$value[i])+times$rowid[i])%% times$value[i])==0)-1
  
  how_far_behind <- (where_are_you + times$rowid[i]) %% times$value[i]
  if(how_far_behind==0){next}
  
  how_much_catchup_cycle <- multiple %% times$value[i]
    for(cycle in 1:10000){
      how_far_behind <- (how_far_behind+how_much_catchup_cycle*1) %% times$value[i]
      if(how_far_behind==0){break}
    }
  where_are_you <- multiple*cycle+where_are_you
  print(where_are_you)
}
where_are_you

all(((where_are_you +times$rowid) %% times$value)==0)
