library(tidyverse)

data <- tibble(yes_question= read_lines("day6.txt"))

data %>% 
  mutate(id = cumsum(yes_question=="")+1) %>% 
  separate_rows(yes_question, sep="(?=.)") %>%
  filter(yes_question != "") %>% 
  count(id, yes_question) %>% 
  count(id) %>% 
  summarize(n = sum(n)) %>% 
  pull(n)

         