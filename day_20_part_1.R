library(tidyverse)
library(unglue)
data <- readLines("day20.txt")

data_df <- unglue_data(data, c('Tile {tile_no}:',
                               "{entry}"))

puzzle_pieces <-
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
  mutate(side_len_b = 1) %>%
  group_by(tile_no) %>% 
  mutate(top_edge = X ==min(X),
         bot_edge = X ==max(X),
         lhs_edge = Y ==min(Y),
         rhs_edge = Y ==max(Y),
         edge_c = (top_edge|bot_edge|lhs_edge|rhs_edge))

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

puzzle_piece_edges <- 
  bind_rows(list(top_edges, bot_edges, lhs_edges, rhs_edges))%>% 
  group_by(tile_no, edge, edge_direction) %>% 
  summarize(entry = paste(entry, collapse = ""))

puzzle_pieces_rev <- 
  puzzle_piece_edges %>% 
  mutate(entry = stringi::stri_reverse(entry),
         edge_direction = if_else(edge_direction==90|edge_direction==270,
                                  (edge_direction+180)%%360, edge_direction))

# puzzle_piece_edges %>% 
#   mutate(lenght = str_length(entry)) %>% 
#   filter(lenght !=10)

complete_joins <- 
puzzle_piece_edges %>% 
  left_join(bind_rows(puzzle_piece_edges, puzzle_pieces_rev), by = c("entry")) %>% 
  filter(tile_no.x != tile_no.y)

complete_joins %>% 
  group_by(tile_no.x) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  filter(n==2) %>% 
  pull(tile_no.x) %>% 
  as.numeric() %>% 
  prod() %>% 
  format(scientific =F)
