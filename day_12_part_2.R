library(tidyverse)

data <- read_csv("day12.txt", col_names = "raw")

all_moves <- 
  data %>% 
  separate(raw, into = c("direction", "num"), sep = 1, convert=T) %>% 
  mutate(ship_east = 0,
         ship_north = 0,
         waypoint_east = 10,
         waypoint_north = 1)

max_row <- length(data$raw)

for(i in 1:max_row){
  #i <- 1
  print(i)
  if(all_moves$direction[i] == "E"){all_moves$waypoint_east[i:max_row] = all_moves$waypoint_east[i]+all_moves$num[i]}
  if(all_moves$direction[i] == "W"){all_moves$waypoint_east[i:max_row] = all_moves$waypoint_east[i]-all_moves$num[i]}
  if(all_moves$direction[i] == "N"){all_moves$waypoint_north[i:max_row] = all_moves$waypoint_north[i]+all_moves$num[i]}
  if(all_moves$direction[i] == "S"){all_moves$waypoint_north[i:max_row] = all_moves$waypoint_north[i]-all_moves$num[i]}
  
  if(all_moves$direction[i] == "F"){all_moves$ship_north[i:max_row] = all_moves$ship_north[i]+all_moves$waypoint_north[i]*all_moves$num[i]
                                    all_moves$ship_east[i:max_row] = all_moves$ship_east[i]+all_moves$waypoint_east[i]*all_moves$num[i]}
  if(all_moves$direction[i] == "R"){
    if(all_moves$num[i] == 90){
      all_moves$waypoint_east[i:max_row] = all_moves$waypoint_north[i-1]
      all_moves$waypoint_north[i:max_row] = -all_moves$waypoint_east[i-1]}
    if(all_moves$num[i] == 180){
      all_moves$waypoint_east[i:max_row] = -all_moves$waypoint_east[i-1]
      all_moves$waypoint_north[i:max_row] = -all_moves$waypoint_north[i-1]}
    if(all_moves$num[i] == 270){
      all_moves$waypoint_east[i:max_row] = -all_moves$waypoint_north[i-1]
      all_moves$waypoint_north[i:max_row] = all_moves$waypoint_east[i-1]}
  }
  if(all_moves$direction[i] == "L"){
    if(all_moves$num[i] == 90){
      all_moves$waypoint_east[i:max_row] = -all_moves$waypoint_north[i-1]
      all_moves$waypoint_north[i:max_row] = all_moves$waypoint_east[i-1]}
    if(all_moves$num[i] == 180){
      all_moves$waypoint_east[i:max_row] = -all_moves$waypoint_east[i-1]
      all_moves$waypoint_north[i:max_row] = -all_moves$waypoint_north[i-1]}
    if(all_moves$num[i] == 270){
      all_moves$waypoint_east[i:max_row] = all_moves$waypoint_north[i-1]
      all_moves$waypoint_north[i:max_row] = -all_moves$waypoint_east[i-1]}
  }
}

last <- all_moves %>% slice(n())

(abs(last$ship_east) + abs(last$ship_north)) 