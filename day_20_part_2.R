library(tidyverse)
library(unglue)
data <- readLines("day20.txt")

data_df <- unglue_data(data, c('Tile {tile_no}:',
                               "{entry}"))

puzzle_pieces_base <-
  data_df %>% 
  fill(tile_no) %>%  
  filter(!is.na(entry)) %>% 
  filter(entry!="") %>% 
  group_by(tile_no) %>% 
  mutate(X = row_number()) %>% 
  separate_rows(entry, sep = "") %>% 
  filter(entry!="") %>% 
  group_by(tile_no, X) %>% 
  mutate(Y = row_number()) %>%
  group_by(tile_no) 

puzzle_pieces <-
  puzzle_pieces_base %>% 
  mutate(top_edge = X ==min(X),
         bot_edge = X ==max(X),
         lhs_edge = Y ==min(Y),
         rhs_edge = Y ==max(Y))

top_edges <- 
  puzzle_pieces %>% 
  filter(top_edge) %>% 
  mutate(edge_direction = 0)

bot_edges <- 
  puzzle_pieces %>% 
  filter(bot_edge) %>% 
  mutate(edge_direction = 180)

lhs_edges <- 
  puzzle_pieces %>% 
  filter(lhs_edge) %>% 
  mutate(edge_direction = 270)

rhs_edges <- 
  puzzle_pieces %>% 
  filter(rhs_edge) %>% 
  mutate(edge_direction = 90)

standard_puzzle_pieces <- 
  puzzle_pieces_base %>% 
  group_split()

puzzle_piece_edges <- 
  bind_rows(list(top_edges, bot_edges, lhs_edges, rhs_edges))%>% 
  group_by(tile_no, edge_direction) %>% 
  summarize(entry = paste(entry, collapse = "")) %>% 
  mutate(reversed = F)

puzzle_pieces_rev <- 
  puzzle_piece_edges %>% 
  mutate(entry = stringi::stri_reverse(entry),
         reversed = T)

complete_joins <- 
  puzzle_piece_edges %>% 
  left_join(bind_rows(puzzle_piece_edges, puzzle_pieces_rev), by = c("entry")) %>% 
  filter(tile_no.x != tile_no.y)

corners <- 
complete_joins %>% 
  group_by(tile_no.x) %>% 
  mutate(n = n()) %>% 
  arrange(n) %>% 
  filter(n==2) 

puzzle_pieces_base <-
  data_df %>% 
  fill(tile_no) %>%  
  filter(!is.na(entry)) %>% 
  filter(entry!="") %>% 
  group_by(tile_no) %>% 
  separate(col = entry, into = paste0("Y", (0:10)), sep = "") %>% 
  select(-Y0) %>% 
  group_by(tile_no)

puzzle_pieces_list <- 
  puzzle_pieces_base %>% 
  nest() %>% 
  mutate(data = map(.x = data, ~as.matrix(.x)))

rotate <- function(x) t(apply(x, 2, rev))
flip <- function(x) rotate(rotate(rotate(t(apply(rotate(x), 1, rev)))))

corners %>% 
  group_by(tile_no.x) %>% 
  slice(1) %>% 
  left_join(puzzle_pieces_list, by=c("tile_no.x"="tile_no")) %>% 
  mutate(data = case_when(
                edge_direction.x==90 ~ data,
                edge_direction.x==0 ~ map(data, ~rotate(.x)),
                edge_direction.x==270 ~ map(data, ~rotate(rotate(.x))),
                edge_direction.x==180 ~ map(data, ~rotate(rotate(rotate(.x)))))) %>% 
  left_join(puzzle_pieces_list, by=c("tile_no.y"="tile_no")) %>% 
  mutate(data = case_when(
    edge_direction.y==270 ~ data,
    edge_direction.y==180 ~ map(data, ~rotate(.x)),
    edge_direction.y==90 ~ map(data, ~rotate(rotate(.x))),
    edge_direction.y==0 ~ map(data, ~rotate(rotate(rotate(.x)))))) %>% 
  mutate(data = if_else(reversed.y,map(data ~flip(.x))))
                

           
mutate(X = row_number()) %>% 
  separate_rows(entry, sep = "") %>% 
  filter(entry!="") %>% 
  group_by(tile_no, X) %>% 
  mutate(Y = row_number()) %>%
  group_by(tile_no) 



