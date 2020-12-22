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
combination_exclude <- data.frame("allergen"=c("NOT"), "ingredient" = c("NOT"))

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

combination_exclude <- bind_rows(combination_exclude, combination_exclude_add)

not_allergen <- 
  combination_exclude %>% 
  group_by(ingredient) %>% 
  summarise(n_allergen = n_distinct(allergen)) %>% 
  filter(n_allergen==num_allergens)

possible_matches %>% 
  filter(ingredient %in% not_allergen$ingredient) %>% 
  distinct(ingredient, item) %>% 
  nrow()

possible_matches <- 
  possible_matches %>% 
  anti_join(combination_exclude)

how_many_possible_allergen_for_ingredient = 
  possible_matches %>% 
  group_by(ingredient) %>% 
  mutate(n_allergen = n_distinct(allergen))

known_allergen <- 
  how_many_possible_allergen_for_ingredient %>% 
  filter(n_allergen == 1)%>% 
  distinct(ingredient, allergen)

check=T

while(check){
  
  how_many_possible_allergen_for_ingredient = 
    possible_matches %>% 
    filter(!(allergen %in% known_allergen$allergen),
           !(ingredient %in% known_allergen$ingredient)) %>% 
    group_by(ingredient) %>% 
    mutate(n_allergen = n_distinct(allergen))
  
  known_allergen_add <- 
    how_many_possible_allergen_for_ingredient %>% 
    filter(n_allergen == 1) %>% 
    distinct(ingredient, allergen)
  
  known_allergen <- bind_rows(known_allergen, known_allergen_add)
  if(nrow(known_allergen_add)<1){check=F}
}

known_allergen %>% 
  arrange(allergen) %>% 
  ungroup() %>% 
  summarise(ingredient = paste(ingredient, sep=",", collapse = ",")) %>% 
  pull(ingredient)
