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

current_colors <- c("shiny gold")
ultimate_colors <- c()

while(length(current_colors)>0){
  current_colors <- 
    complete_rules %>% 
    filter(inner_color %in% current_colors) %>% 
    filter(!(outer %in% ultimate_colors)) %>%
    pull(outer)
  print(current_colors)
  ultimate_colors <- c(ultimate_colors, unique(current_colors))  
}

print(length(ultimate_colors))
