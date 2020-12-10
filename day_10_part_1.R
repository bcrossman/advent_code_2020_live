library(tidyverse)

jolts <- as.numeric(read_lines("day10.txt"))

data <- tibble(jolts = c(0,jolts,max(jolts)+3))

data %>%
  arrange(jolts) %>% 
  mutate(dif = jolts - lag(jolts,1, default = 0)) %>% 
  count(dif) %>% 
  pivot_wider(names_from = dif, values_from = n) %>% 
  mutate(result = `1`*`3`) %>% 
  pull(result)





