library(tidyverse)
data <- read_tsv("day3.txt", col_names = c("pattern"))

rights <- c(1,3,5,7,1)
downs <-  c(1,1,1,1,2)

results <- 1
for(i in 1:length(rights)){
  # print(i)
  right <- rights[[i]]
  down <- downs[[i]]
  
  result <-   
    data %>%
    mutate(key_x = ((((row_number()-1)/down)*right)+1)%% str_length(pattern),
           key_x = ifelse(key_x==0, str_length(pattern),key_x),
           key_y =  ((row_number()-1) %% down) == 0,
           is_tree = str_sub(string = pattern, start = key_x, end = key_x)=="#",
           is_tree_really = (is_tree&key_y)) %>% 
    summarize(is_tree_really = sum(is_tree_really)) %>% 
    pull()
  print(result)
  results <- result*results
}

results

