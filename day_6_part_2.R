library(tidyverse)

data <- tibble(yes_question= read_lines("day6.txt"))

data %>% 
  mutate(id = cumsum(yes_question=="")+1) %>% 
  filter(yes_question != "") %>% 
  group_by(id) %>% 
  mutate(num_people = n()) %>%
  separate_rows(yes_question, sep="(?=.)") %>%
  filter(yes_question != "") %>% 
  count(id, num_people, yes_question) %>%
  group_by(id) %>% 
  mutate(valid = n==num_people) %>% 
  filter(valid) %>% 
  count(id) %>% 
  ungroup() %>% 
  summarize(n = sum(n)) %>% 
  pull(n)