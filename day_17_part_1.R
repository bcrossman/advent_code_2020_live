library(tidyverse)

data <- read_csv("day17.txt", col_names = F)
cubes <- 
  as_tibble(data$X1 %>% str_split(pattern = "", simplify = T)) %>% 
  rowid_to_column("X") %>% 
  pivot_longer(-X, names_to = "Y", values_to = "state") %>%
  filter(state=="#") %>% 
  mutate(Y = as.integer(gsub(pattern = "V", replacement = "", Y))) %>% 
  mutate(Y = as.integer(Y)) %>% 
  mutate(Z = 0) %>% 
  select(X,Y,Z)

for(i in 1:6){
  # if(i==2){break}
  # print(i)
  results <- list()
  for(x in (min(cubes$X)-1):(max(cubes$X)+1)){
    for(y in (min(cubes$Y)-1):(max(cubes$Y)+1)){
      for(z in (min(cubes$Z)-1):(max(cubes$Z)+1)){
        #x=1
        #y=2
        #z=0
        touching <- 
          cubes %>% 
          filter(X>=(x-1)&X<=(x+1),
                 Y>=(y-1)&Y<=(y+1),
                 Z>=(z-1)&Z<=(z+1)) %>% 
          nrow()
        
        current <- 
          cubes %>% 
          filter(X==(x)&X==(x),
                 Y==(y)&Y==(y),
                 Z==(z)&Z==(z)) %>% 
          nrow()
        if(current==0&touching==3){
          results[[paste(x,y,z)]] <- data.frame("X"=x,"Y"=y,"Z"=z)
        }
        if(current==1&(touching==(2+1)|touching==(3+1))){
          results[[paste(x,y,z)]] <- data.frame("X"=x,"Y"=y,"Z"=z)
        }
      }
    }
  }
  
  cubes <- bind_rows(results)
}
cubes %>% nrow()

