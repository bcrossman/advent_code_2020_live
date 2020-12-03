library(tidyverse)
data <- read_delim("day2.txt", col_names = c("count", "character", "password"), delim = " ")

data %>% 
  separate(col = count, into = c("min_count", "max_count"), sep = "-") %>%
  mutate_at(.vars = c("min_count", "max_count"), .funs = as.numeric) %>% 
  mutate(character = gsub(pattern = ":", replacement = "", x = character)) %>% 
  mutate(count_character = str_count(string = password, pattern = character),
         min_check = count_character>=min_count,
         max_check = count_character<=max_count) %>% 
  filter(max_check & min_check) %>% 
  nrow()

  
  