library(tidyverse)
library(unglue)
data <- readLines("day20.txt")

data_df <- unglue_data(data, c('Tile {tile_no}:',
                               "{entry}"))

raw_df <-
  data_df %>% 
  fill(tile_no) %>%  
  filter(!is.na(entry)) %>% 
  filter(entry!="") 

create_edges <- function(raw_df){
  puzzle_pieces_base <- 
    raw_df %>% 
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
  
  puzzle_piece_edges <- 
    bind_rows(list(top_edges, bot_edges, lhs_edges, rhs_edges))%>% 
    group_by(tile_no, edge_direction) %>% 
    summarize(entry = paste(entry, collapse = "")) %>% 
    mutate(reversed = F)
  
  return(puzzle_piece_edges)
}

puzzle_piece_edges <- create_edges(raw_df)


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

not_corners <- 
  complete_joins %>% 
  group_by(tile_no.x) %>% 
  mutate(n = n()) %>% 
  arrange(n) %>% 
  filter(n!=2) %>% 
  filter(!is.na(tile_no.y))

puzzle_pieces_list_orig <- 
  raw_df %>% 
  separate(col = entry, into = paste0("Y", (0:10)), sep = "") %>%
  select(-Y0) %>% 
  group_by(tile_no) %>% 
  nest() %>% 
  mutate(data = map(.x = data, ~as.matrix(.x)))

rotate <- function(x) t(apply(x, 2, rev))
flip <- function(x) rotate(rotate(rotate(t(apply(rotate(x), 1, rev)))))

combine_pieces <- function(join_df, puzzle_pieces_list){
  
  join_df %>% 
    group_by(tile_no.x) %>% 
    # slice(1) %>% 
    left_join(puzzle_pieces_list, by=c("tile_no.x"="tile_no")) %>% 
    ungroup() %>% 
    mutate(data = case_when(
      edge_direction.x==90 ~ data,
      edge_direction.x==0 ~ map(data, ~rotate(.x)),
      edge_direction.x==270 ~ map(data, ~rotate(rotate(.x))),
      edge_direction.x==180 ~ map(data, ~rotate(rotate(rotate(.x)))))) %>% 
    left_join(puzzle_pieces_list, by=c("tile_no.y"="tile_no")) %>% 
    mutate(data.y = case_when(
      edge_direction.y==270 ~ data.y,
      edge_direction.y==180 ~ map(data.y, ~rotate(.x)),
      edge_direction.y==90 ~ map(data.y, ~rotate(rotate(.x))),
      edge_direction.y==0 ~ map(data.y, ~rotate(rotate(rotate(.x)))))) %>% 
    mutate(data.y = ifelse(reversed.y==T, map(data.y, ~flip(.x)),data.y)) %>% 
    mutate(data_combined = map2(data.x, data.y, ~as.tibble(cbind(.x, .y)))) %>% 
    mutate(tile_no = paste(tile_no.x, tile_no.y, sep = "_")) %>%
    mutate(tile_no_2 = paste(tile_no.y, tile_no.x, sep = "_")) %>%
    select(tile_no, tile_no_2, data_combined) %>% 
    unnest(data_combined) %>% 
    unite("entry", -tile_no, -tile_no_2, sep="", remove = T) %>% 
    mutate(entry = gsub("NA", "", entry))
}

two_piece_corners <- 
  combine_pieces(corners, puzzle_pieces_list_orig) %>% 
  ungroup()

other_two_piece <- 
  combine_pieces(not_corners, puzzle_pieces_list_orig) %>% 
  ungroup() %>% 
  filter(!(tile_no %in% two_piece_corners$tile_no)) %>% 
  filter(!(tile_no %in% two_piece_corners$tile_no_2))

puzzle_piece_edges_base <- create_edges(two_piece_corners) 

puzzle_piece_edges_try <- create_edges(other_two_piece) 

puzzle_piece_edges_try_rev <- 
  puzzle_piece_edges_try %>% 
  mutate(entry = stringi::stri_reverse(entry),
         reversed = T)

complete_joins <- 
  puzzle_piece_edges_base %>% 
  left_join(bind_rows(puzzle_piece_edges_try, puzzle_piece_edges_try_rev), 
            by = c("entry")) 

expanded_corners <- 
  complete_joins %>% 
  group_by(tile_no.x) %>% 
  filter(!is.na(tile_no.y)) %>% 
  mutate(n = n()) %>% 
  arrange(n) %>% 
  filter(n==1) 

not_expanded_corners <- 
  complete_joins %>% 
  group_by(tile_no.x) %>% 
  filter(!is.na(tile_no.y)) %>% 
  mutate(n = n()) %>% 
  arrange(n) %>% 
  filter(n!=1) 

##Round 3

puzzle_pieces_list_new <- 
  two_piece_corners %>%
  bind_rows(other_two_piece) %>% 
  select(-tile_no_2) %>% 
  separate(col = entry, into = paste0("Y", (0:20)), sep = "") %>%
  select(-Y0) %>% 
  group_by(tile_no) %>% 
  nest() %>% 
  mutate(data = map(.x = data, ~as.matrix(.x)))

four_piece_corners<- 
  combine_pieces(expanded_corners, puzzle_pieces_list_new) %>% 
  ungroup() 

other_four_pieces <- 
  combine_pieces(not_expanded_corners, puzzle_pieces_list_new) %>% 
  ungroup() %>% 
  filter(!(tile_no %in% two_piece_corners$tile_no)) %>% 
  filter(!(tile_no %in% two_piece_corners$tile_no_2))

puzzle_piece_edges_base <- create_edges(four_piece_corners) 

puzzle_piece_edges_try <- create_edges(other_four_pieces) 

puzzle_piece_edges_try_rev <- 
  puzzle_piece_edges_try %>% 
  mutate(entry = stringi::stri_reverse(entry),
         reversed = T)

complete_joins <- 
  puzzle_piece_edges_base %>% 
  left_join(bind_rows(puzzle_piece_edges_try, puzzle_piece_edges_try_rev), 
            by = c("entry")) 

complete_joins %>% 
  group_by(tile_no.x) %>% 
  filter(!is.na(tile_no.y)) %>% 
  mutate(n = n()) %>% 
  arrange(n) 
