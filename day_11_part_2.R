library(tidyverse)
library(stringi)

data <- read_csv("day11.txt", col_names = F)
seats <- 
  as_tibble(data$X1 %>% str_split(pattern = "", simplify = T)) %>% 
  rowid_to_column("row_id") %>% 
  pivot_longer(-row_id, names_to = "col_id", values_to = "state") %>%
  mutate(col_id = as.numeric(gsub(pattern = "V", replacement = "", col_id)))

for(i in 1:1000){
  seats <-
    seats %>%
    rowwise() %>% 
    mutate(left_row_occ = 
             stri_extract_last_regex(paste0(unlist(.[.$row_id==(row_id) &
                                                        .$col_id<(col_id)
                                                      ,"state"]),
                                             collapse = ""), pattern = "L|#")=="#",
           right_row_occ = 
             stri_extract_first_regex(paste0(unlist(.[.$row_id==(row_id) &
                                                        .$col_id>(col_id)
                                                      ,"state"]),
                                             collapse = ""), pattern = "L|#")=="#",
           up_col_occ =  
             stri_extract_last_regex(paste0(unlist(.[.$row_id<(row_id) &
                                                        .$col_id==(col_id)
                                                      ,"state"]),
                                             collapse = ""), pattern = "L|#")=="#",
           down_col_occ = 
             stri_extract_first_regex(paste0(unlist(.[.$row_id>(row_id) &
                                                        .$col_id==(col_id)
                                                      ,"state"]),
                                             collapse = ""), pattern = "L|#")=="#",
           up_left_diag = 
             stri_extract_last_regex(paste0(unlist(.[(abs(.$row_id-row_id)==abs(.$col_id-col_id))&
                                                        .$row_id<(row_id) & 
                                                        .$col_id<(col_id)
                                                      ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id)+abs(.$col_id-col_id)) %>% pull(state)),
                                             collapse = ""), pattern = "L|#")=="#",
           down_left_diag = 
             stri_extract_first_regex(paste0(unlist(.[(abs(.$row_id-row_id)==abs(.$col_id-col_id))&
                                                        .$row_id>(row_id) & 
                                                        .$col_id<(col_id)
                                                      ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id)+abs(.$col_id-col_id)) %>% pull(state)),
                                             collapse = ""), pattern = "L|#")=="#",
           up_right_diag = 
             stri_extract_last_regex(paste0(unlist(.[(abs(.$row_id-row_id)==abs(.$col_id-col_id))&
                                                        .$row_id<(row_id) & 
                                                        .$col_id>(col_id)
                                                      ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id)+abs(.$col_id-col_id)) %>% pull(state)),
                                             collapse = ""), pattern = "L|#")=="#",
           down_right_diag = 
             stri_extract_last_regex(paste0(unlist(.[(abs(.$row_id-row_id)==abs(.$col_id-col_id))&
                                                        .$row_id>(row_id) & 
                                                        .$col_id>(col_id)
                                                      ,c("row_id", "col_id", "state")] %>% arrange(abs(.$row_id-row_id)+abs(.$col_id-col_id)) %>% pull(state)),
                                             collapse = ""), pattern = "L|#")=="#")
  
  seats$total_see <- rowSums(seats %>% select(left_row_occ:down_right_diag), na.rm = T)
  seats <- 
    seats %>% 
    mutate(
      state_2 = if_else(state=="L"&total_see<1, "#", state),
      state_2 = if_else(state=="#"&total_see>=5,"L", state_2))
  
  if(all(seats$state==seats$state_2)){break}
  
  print(paste("count:",i,"occupied_seats:", sum(seats$state_2=="#")))
  seats$state <- seats$state_2
  
}

sum(seats$state_2=="#")


