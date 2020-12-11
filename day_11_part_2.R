library(tidyverse)
library(stringi)

data <- read_csv("day11.txt", col_names = F)
seats <- 
  as_tibble(data$X1 %>% str_split(pattern = "", simplify = T)) %>% 
  rowid_to_column("row_id") %>% 
  pivot_longer(-row_id, names_to = "col_id", values_to = "state") %>%
  mutate(col_id = as.numeric(gsub(pattern = "V", replacement = "", col_id)))

left_row_occ_lookup <- function(row_id_use, col_id_use){
  stri_extract_last_regex(paste0(unlist(seats[seats$row_id==(row_id_use) &
                                                seats$col_id<(col_id_use)
                                              ,"state"]),
                                 collapse = ""), pattern = "L|#")=="#"
}
right_row_occ_lookup <- function(row_id_use, col_id_use){ 
  stri_extract_first_regex(paste0(unlist(seats[seats$row_id==(row_id_use) &
                                                 seats$col_id>(col_id_use)
                                               ,"state"]),
                                  collapse = ""), pattern = "L|#")=="#"}
up_col_occ_lookup <- function(row_id_use, col_id_use){  
  stri_extract_last_regex(paste0(unlist(seats[seats$row_id<(row_id_use) &
                                                seats$col_id==(col_id_use)
                                              ,"state"]),
                                 collapse = ""), pattern = "L|#")=="#"}
down_col_occ_lookup <- function(row_id_use, col_id_use){ 
  stri_extract_first_regex(paste0(unlist(seats[seats$row_id>(row_id_use) &
                                                 seats$col_id==(col_id_use)
                                               ,"state"]),
                                  collapse = ""), pattern = "L|#")=="#"}
up_left_diag_lookup <- function(row_id_use, col_id_use){ 
  stri_extract_first_regex(paste0(unlist(seats[(abs(seats$row_id-row_id_use)==abs(seats$col_id-col_id_use))&
                                                 seats$row_id<(row_id_use) & 
                                                 seats$col_id<(col_id_use)
                                               ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id_use)+abs(.$col_id-col_id_use)) %>% pull(state)),
                                  collapse = ""), pattern = "L|#")=="#"}
down_left_diag_lookup <- function(row_id_use, col_id_use){ 
  stri_extract_first_regex(paste0(unlist(seats[(abs(seats$row_id-row_id_use)==abs(seats$col_id-col_id_use))&
                                                 seats$row_id>(row_id_use) & 
                                                 seats$col_id<(col_id_use)
                                               ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id_use)+abs(.$col_id-col_id_use)) %>% pull(state)),
                                  collapse = ""), pattern = "L|#")=="#"}
up_right_diag_lookup <- function(row_id_use, col_id_use){ 
  stri_extract_first_regex(paste0(unlist(seats[(abs(seats$row_id-row_id_use)==abs(seats$col_id-col_id_use))&
                                                 seats$row_id<(row_id_use) & 
                                                 seats$col_id>(col_id_use)
                                               ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id_use)+abs(.$col_id-col_id_use)) %>% pull(state)),
                                  collapse = ""), pattern = "L|#")=="#"}
down_right_diag_lookup <- function(row_id_use, col_id_use){ 
  stri_extract_first_regex(paste0(unlist(seats[(abs(seats$row_id-row_id_use)==abs(seats$col_id-col_id_use))&
                                                 seats$row_id>(row_id_use) & 
                                                 seats$col_id>(col_id_use)
                                               ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id_use)+abs(.$col_id-col_id_use)) %>% pull(state)),
                                  collapse = ""), pattern = "L|#")=="#"}

vleft_row_occ_lookup <- Vectorize(left_row_occ_lookup)
vright_row_occ_lookup <- Vectorize(right_row_occ_lookup)
vup_col_occ_lookup <- Vectorize(up_col_occ_lookup)
vdown_col_occ_lookup <- Vectorize(down_col_occ_lookup)
vup_left_diag_lookup <- Vectorize(up_left_diag_lookup)
vdown_left_diag_lookup <- Vectorize(down_left_diag_lookup)
vup_right_diag_lookup <- Vectorize(up_right_diag_lookup)
vdown_right_diag_lookup <- Vectorize(down_right_diag_lookup)

for(i in 1:1000){
  df <-
    seats %>%
    mutate(left_row_occ = vleft_row_occ_lookup(row_id, col_id),
           right_row_occ = vright_row_occ_lookup(row_id, col_id),
           up_col_occ = vup_col_occ_lookup(row_id, col_id),
           down_col_occ = vdown_col_occ_lookup(row_id, col_id),
           up_left_diag = vup_left_diag_lookup(row_id, col_id),
           down_left_diag = vdown_left_diag_lookup(row_id, col_id),
           up_right_diag = vup_right_diag_lookup(row_id, col_id),
           down_right_diag = vdown_right_diag_lookup(row_id, col_id))
  
  df$total_see <- rowSums(df %>% select(left_row_occ:down_right_diag), na.rm = T)
  df <- 
    df %>% 
    mutate(
      state_2 = if_else(state=="L"&total_see<1, "#", state),
      state_2 = if_else(state=="#"&total_see>=5,"L", state_2))
  
  if(all(df$state==df$state_2)){break}
  
  print(paste("count:",i,"occupied_seats:", sum(df$state_2=="#")))
  df$state <- df$state_2
  
  seats <- df
  
}

sum(seats$state_2=="#")

View(seats %>% select(row_id, col_id, state_2) %>% pivot_wider(names_from =col_id, values_from = state_2))

