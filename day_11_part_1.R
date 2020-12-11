library(tidyverse)

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
    mutate(adj_occ = sum(.[.$row_id>=(row_id-1)&
                             .$row_id<=(row_id+1)&
                             .$col_id>=(col_id-1)&
                             .$col_id<=(col_id+1)
                           ,"state"]=="#"),
           state_2 = if_else(state=="L"&adj_occ<1, "#", state),
           state_2 = if_else(state=="#"&adj_occ>=5,"L", state_2))
  
  if(all(seats$state==seats$state_2)){break}
  
  seats$state <- seats$state_2
  print(paste("count:",i,"occupied_seats:", sum(seats$state=="#")))
}

sum(seats$state=="#")

