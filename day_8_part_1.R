library(tidyverse)

data <- tibble(code = read_lines("day8.txt")) 

df <- 
  data %>% 
  mutate(id = row_number()) %>% 
  separate(code, into = c("op", "num"), sep = " ", convert = T) %>% 
  mutate(num = ifelse(op=="nop", 0, num),
         op = ifelse(op=="nop", "acc", op)) %>% 
  pivot_wider(id_cols = id, names_from = "op", values_from = "num") %>% 
  mutate(acc = ifelse(is.na(acc),0, acc),
         jmp = ifelse(is.na(jmp),1, jmp))  

df$order <- 0

for(i in 1:length(df$id)){
  if(i==1){
    df$order[1] <- i
  }else{
    if(df$order[df$id[df$order==(i-1)]+df$jmp[df$order==(i-1)]]!=0){break}else{
      df$order[df$id[df$order==(i-1)]+df$jmp[df$order==(i-1)]] <- i}
  }
  
}

df %>% 
  filter(order != 0) %>% 
  arrange(order) %>% 
  # mutate(result = cumsum(acc)) %>% 
  summarize(result = sum(acc)) %>% 
  pull(result)
