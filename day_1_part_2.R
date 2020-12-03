library(tidyverse)
data <- read_csv("day1.csv", col_names = FALSE)

expand.grid(data$X1, data$X1, data$X1) %>% 
  set_names(c("first_num", "second_num", "third_num")) %>% 
  filter((first_num+second_num+third_num)==2020) %>% 
  slice(1) %>% 
  summarize(result = first_num*second_num*third_num) %>% 
  pull(result)

