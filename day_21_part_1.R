library(tidyverse)
library(unglue)
data <- readLines("day21.txt")

data_df <- unglue_data(data, c('{ingredient} (contains {allergen}'))

possible_matches <- 
data_df %>% 
  rowid_to_column("item") %>% 
  mutate(allergen = gsub(")", replacement = "",x = allergen, fixed = T)) %>% 
  separate_rows(allergen, sep = ", ") %>% 
  separate_rows(ingredient, sep = " ") %>% 
  filter(allergen!="")

num_allergens <- length(unique(possible_matches$allergen))

how_many_items_that_have_allergen_have_ingedient <- 
  possible_matches %>% 
  count(allergen, ingredient) %>% 
  complete(allergen, ingredient, fill = list(n =0))

how_many_items_mention_allergen  <-  
  possible_matches %>% 
  group_by(allergen) %>% 
  summarise(n_items = n_distinct(item))

does_it_show_up_enough <- 
  how_many_items_that_have_allergen_have_ingedient %>% 
  left_join(how_many_items_mention_allergen) %>% 
  mutate(check = n-n_items)

combination_exclude_add <- 
  does_it_show_up_enough %>% 
  filter(check<0) %>% 
  select(allergen, ingredient)

not_allergen <- 
  combination_exclude_add %>% 
  group_by(ingredient) %>% 
  summarise(n_allergen = n_distinct(allergen)) %>% 
  filter(n_allergen==num_allergens)

possible_matches %>% 
  filter(ingredient %in% not_allergen$ingredient) %>% 
  distinct(ingredient, item) %>% 
  nrow()
