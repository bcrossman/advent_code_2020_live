library(tidyverse)
data <- read_csv("day1.csv", col_names = FALSE)

data %>% 
  rename(first_num = X1) %>% 
  mutate(second_num = 2020-first_num,
         match = ifelse(second_num %in% first_num, TRUE, FALSE)) %>% 
  filter(match) %>% 
  mutate(result = first_num*second_num) %>% 
  slice(1) %>% 
  pull(result)
  