library(tidyverse)

data <- read_csv("day12.txt", col_names = "raw")

all_moves <- 
  data %>% 
  separate(raw, into = c("direction", "num"), sep = 1, convert=T) %>% 
  mutate(key_1 = if_else(direction %in% c("N","S","E","W"), "compass", "turn"))

turn_translate_df <- data.frame(key_2 = "turn", cumul_turn = c(0,90,180,270), direction_2 = c("E","S","W","N"))

turn_df <- 
  all_moves %>% 
  filter(direction %in% c("F","L","R")) %>% 
  rowid_to_column("order") %>% 
  mutate(key_2 = if_else(direction=="F", "move", "turn"),
         num = if_else(direction=="L", -num, num),
         num2 = if_else(direction=="F", as.integer(0), num)) %>% 
  arrange(key_2, order) %>% 
  mutate(cumul_turn = cumsum(num2) %% 360) %>% 
  left_join(turn_translate_df) %>% 
  arrange(order) %>% 
  fill(direction_2) %>% 
  filter(direction == "F") %>% 
  mutate(direction = direction_2) %>% 
  select(direction, num)

all_moves %>% 
  filter(!(direction %in% c("F","L","R"))) %>% 
  bind_rows(turn_df) %>% 
  mutate(key_2 = if_else(direction=="N"|direction=="S", "UP-DOWN", "L-R"),
         num = if_else(direction=="S", -num, num),
         num = if_else(direction=="W", -num, num)) %>%
  group_by(key_2) %>% 
  summarise(num = abs(sum(num))) %>% 
  summarise(result = sum(num)) %>% 
  pull(result)
  
  
