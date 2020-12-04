library(tidyverse)

data <- data.frame(data= scan("day4.txt", what = "character", blank.lines.skip = F))

data %>% 
  mutate(id = if_else(data == "", 1,0)) %>% 
  mutate(id = cumsum(id)+1) %>% 
  filter(data != "") %>% 
  separate(data, into = c("key", "value"), sep=":") %>% 
  pivot_wider(id_cols = id, names_from = "key", values_from="value") %>% 
  mutate(VALID = (rowSums(is.na(.))-as.numeric(is.na(cid)))<1) %>% 
  summarise(result = sum(VALID)) %>% 
  pull(result)

         