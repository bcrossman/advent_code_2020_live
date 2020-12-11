library(tidyverse)

data <- read_csv("day11.txt", col_names = F)
seats <- 
  as_tibble(data$X1 %>% str_split(pattern = "", simplify = T)) %>% 
  rowid_to_column("row_id") %>% 
  pivot_longer(-row_id, names_to = "col_id", values_to = "state") %>%
  mutate(col_id = as.numeric(gsub(pattern = "V", replacement = "", col_id)))

lookup_function <- function(row_id_use, col_id_use){
  
  sum(seats[seats$row_id>=(row_id_use-1)&
              seats$row_id<=(row_id_use+1)&
              seats$col_id>=(col_id_use-1)&
              seats$col_id<=(col_id_use+1)
        ,"state"]=="#")
}

vlookup_function <- Vectorize(lookup_function)

for(i in 1:1000){
  df <-
    seats %>%
    mutate(adj_occ = vlookup_function(row_id, col_id),
           state_2 = if_else(state=="L"&adj_occ<1, "#", state),
           state_2 = if_else(state=="#"&adj_occ>=5,"L", state_2))
  
  if(all(df$state==df$state_2)){break}
  
  df$state <- df$state_2
  seats <- df
  print(paste("count:",i,"occupied_seats:", sum(seats$state=="#")))
  
  
}

sum(seats$state=="#")

