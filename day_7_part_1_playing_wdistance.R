library(tidyverse)
library(igraph)

input <- 
  tibble(rule = read_lines("day7.txt")) %>% 
  separate(rule, into = c("outer", "content"), sep = " bags contain") %>% 
  separate_rows(content, sep = ",") %>% 
  # drop those that don't contain any other bags:
  filter(content != " no other bags.") %>% 
  # extract number
  extract(content, into = c("quantity", "content"), regex = "([[:digit:]]+) (.*)", convert = TRUE) %>% 
  mutate(content = str_remove_all(content, "bags|bag|\\.") %>% str_trim())

# Part I
bags_distances = input %>% 
  select(from = content, to = outer) %>% 
  graph_from_data_frame(directed = TRUE) %>% 
  distances(to = "shiny gold", mode = "in")

bags_distances[(bags_distances > 0 & is.finite(bags_distances))] %>%
  length()

