library(tidyverse)
library(unglue)
data <- readLines("day20_simple.txt")

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

puzzle_pieces_list_orig <- 
  raw_df %>% 
  separate(col = entry, into = paste0("Y", (0:10)), sep = "") %>%
  select(-Y0) %>% 
  group_by(tile_no) %>% 
  nest() %>% 
  mutate(data = map(.x = data, ~as.matrix(.x)))

first_piece_id <- 
  corners %>% 
  mutate(good_direction = (edge_direction.x==180|edge_direction.x==90)) %>% 
  group_by(tile_no.x) %>% 
  mutate(good_direction=sum(good_direction)) %>% 
  filter(good_direction==2) %>% 
  ungroup() %>% 
  slice(1) %>% 
  pull(tile_no.x)

first_piece <- 
  puzzle_pieces_list_orig %>% 
  filter(tile_no==first_piece_id) %>% 
  pull(data)

puzzle <- tibble("row_id" = 1, 
                 "col_id" = 1, 
                 "tile_no" = first_piece_id,
                 "puzzle_piece" = first_piece)

rotate <- function(x) t(apply(x, 2, rev))
flip_up_down <- function(x) rotate(rotate(rotate(t(apply(rotate(x), 1, rev)))))
flip_left_right <- function(x) t(apply(x, 1, rev))

max_row <- 3
max_col <- 3

for(row in 1:max_row){
  for(col in 1:max_col){
    if(row==max_row&col==max_col){break}
    if(col<max_col){
      start_id = puzzle$tile_no[puzzle$row_id==row&puzzle$col_id==col]
      
      start_piece = 
        puzzle %>% 
        filter(tile_no==start_id) %>% 
        select(tile_no,puzzle_piece) %>%
        pull(puzzle_piece)
      
      start_piece_df <- 
        start_piece[[1]] %>% 
        as_tibble() %>% 
        unite("entry", sep="", remove = T) %>% 
        mutate(tile_no = start_id)
      
      start_piece_edge <- 
        create_edges(start_piece_df) %>% 
        filter(edge_direction ==90)  
      
      complete_joins <- 
        start_piece_edge %>% 
        left_join(bind_rows(puzzle_piece_edges, puzzle_pieces_rev), by = c("entry")) %>% 
        filter(tile_no.x != tile_no.y) %>% 
        filter(!(tile_no.y %in%  puzzle$tile_no))
      
      new_piece <- 
        complete_joins %>% 
        left_join(puzzle_pieces_list_orig, by=c("tile_no.y"="tile_no")) %>% 
        mutate(data= case_when(
          edge_direction.y==270 ~ data,
          edge_direction.y==180 ~ map(data, ~rotate(.x)),
          edge_direction.y==90 ~ map(data, ~rotate(rotate(.x))),
          edge_direction.y==0 ~ map(data, ~rotate(rotate(rotate(.x)))))) %>% 
        mutate(data = ifelse(reversed.y==T, map(data, ~flip_up_down(.x)),data)) %>% 
        pull(data)
      
      puzzle_add <- tibble("row_id" = row, 
                           "col_id" = col+1, 
                           "tile_no" = complete_joins$tile_no.y[1],
                           "puzzle_piece" = new_piece)
      
      puzzle <- bind_rows(puzzle, puzzle_add)
    }
    if(col==max_col){
      start_id = puzzle$tile_no[puzzle$row_id==row&puzzle$col_id==1]
      
      start_piece = 
        puzzle %>% 
        filter(tile_no==start_id) %>% 
        select(tile_no,puzzle_piece) %>%
        pull(puzzle_piece)
      
      start_piece_df <- 
        start_piece[[1]] %>% 
        as_tibble() %>% 
        unite("entry", sep="", remove = T) %>% 
        mutate(tile_no = start_id)
      
      start_piece_edge <- 
        create_edges(start_piece_df) %>% 
        filter(edge_direction ==180)  
      
      complete_joins <- 
        start_piece_edge %>% 
        left_join(bind_rows(puzzle_piece_edges, puzzle_pieces_rev), by = c("entry")) %>% 
        filter(tile_no.x != tile_no.y) %>% 
        filter(!(tile_no.y %in%  puzzle$tile_no))
      
      new_piece <- 
        complete_joins %>% 
        left_join(puzzle_pieces_list_orig, by=c("tile_no.y"="tile_no")) %>% 
        mutate(data= case_when(
          edge_direction.y==0 ~ data,
          edge_direction.y==270 ~ map(data, ~rotate(.x)),
          edge_direction.y==180 ~ map(data, ~rotate(rotate(.x))),
          edge_direction.y==90 ~ map(data, ~rotate(rotate(rotate(.x)))))) %>% 
        mutate(data = ifelse(reversed.y==T, map(data, ~flip_left_right(.x)),data)) %>% 
        pull(data)
      
      puzzle_add <- tibble("row_id" = row+1, 
                           "col_id" = 1, 
                           "tile_no" = complete_joins$tile_no.y[1],
                           "puzzle_piece" = new_piece)
      
      puzzle <- bind_rows(puzzle, puzzle_add)
    }
  }
}

base <- 
puzzle %>% 
  select(-tile_no) %>% 
  pivot_wider(names_from = "col_id", values_from = "puzzle_piece") %>% 
  select(-row_id)

current_col <- 
  base[,1] %>% 
  unnest() %>% 
  as.data.frame()

for(i in 2:max_col){

  col <- 
    base[,i] %>% 
    unnest()%>% 
    as.data.frame()
  
  current_col <- bind_cols(current_col, col)
}

current_col %>% 
  as.matrix() 
