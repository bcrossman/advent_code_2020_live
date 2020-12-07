library(tidyverse)

data <- tibble(rules = read_lines("day7.txt"))

complete_rules <- 
  data %>% 
  separate(rules, into = c("outer", "inner"), sep = "contain") %>% 
  separate_rows(inner, sep = ",") %>% 
  mutate(inner = gsub(pattern = "\\.", replacement = "", x = inner)) %>% 
  mutate(inner = trimws(inner, which = "both")) %>% 
  mutate(outer = trimws(outer, which = "both")) %>% 
  mutate(inner = ifelse(inner=="no other bags", NA, inner)) %>%
  separate(inner, into = c("inner_num", "inner_color"), sep = 2, convert = T) %>% 
  separate(inner_color, into = c("inner_color", "extra"), sep = " bag") %>% 
  separate(outer, into = c("outer", "extra2"), sep = " bag") %>% 
  select(inner_color, inner_num, outer)

current_colors <- tibble(inner_num = 1, inner_color = "shiny gold")
ultimate_bags <- c()
addl_bags <- 1

while(addl_bags>0){
  
  df <- 
    current_colors %>% 
    left_join(complete_rules, by = c("inner_color"="outer"))
  
  current_colors <- 
    df %>% 
    select(-inner_color) %>% 
    mutate(inner_num = inner_num.x * inner_num.y) %>% 
    rename(inner_color = inner_color.y) %>% 
    select(inner_color, inner_num) %>% 
    group_by(inner_color) %>% 
    summarize(inner_num = sum(inner_num)) %>% 
    ungroup()
  
  addl_bags <- sum(current_colors$inner_num, na.rm = T)
  ultimate_bags <- c(ultimate_bags, addl_bags)  
  # print(addl_bags)
}

sum(ultimate_bags)
