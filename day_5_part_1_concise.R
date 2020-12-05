library(tidyverse)

data <- read_csv("day5.txt", col_names = "search")

num_rows <- 128
num_cols <- 8

data %>% 
  mutate(final_row = map_dbl(gregexpr(pattern = "B", text = search),
                             ~sum(num_rows/2^pmax(.x,0)))) %>% 
  mutate(final_col = map_dbl(gregexpr(pattern = "R", text = search),
                             ~sum(num_cols/2^(pmax(.x-7,0))))) %>% 
  mutate(final_row = ifelse(num_rows==final_row, 0, final_row),
         final_col = ifelse(num_cols==final_col, 0, final_col)) %>% 
  mutate(result = final_row*8+final_col) %>% 
  summarize(max(result))



