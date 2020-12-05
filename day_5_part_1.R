library(tidyverse)

data <- read_csv("day5.txt", col_names = "search")

num_rows <- 128
num_cols <- 8

data %>% 
  rowid_to_column("id") %>% 
  separate(search, into = c("row", "col"), sep = 7, remove = F) %>% 
  separate_rows(row, sep = "(?=F|B)") %>% 
  filter(row !="") %>% 
  group_by(id) %>% 
  mutate(row_id = 1:length(id)) %>% 
  mutate(movement = num_rows/(2^row_id),
         updown = ifelse(row == "B", 1, 0)) %>% 
  mutate(final_row = cumsum(movement*updown)) %>% 
  filter(row_id == max(row_id)) %>% 
  ungroup() %>% #repeat for columns
  separate_rows(col, sep = "(?=R|L)") %>% 
  filter(col !="") %>% 
  group_by(id) %>% 
  mutate(row_id = 1:length(id)) %>% 
  mutate(movement = num_cols/(2^row_id),
         updown = ifelse(col == "R", 1, 0)) %>% 
  mutate(final_col = cumsum(movement*updown)) %>% 
  filter(row_id == max(row_id)) %>% 
  ungroup() %>% 
  mutate(result = final_row*8+final_col) %>% 
  summarize(max(result))
  


