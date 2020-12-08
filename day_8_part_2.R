library(tidyverse)

data <- tibble(code = read_lines("day8.txt")) 

test <- TRUE

index <- which(grepl(pattern = regex("nop|jmp"), x = data$code))
count <- 1

while(test){
  
  data_use <- data
  data_use$code[index[count]] <- 
    ifelse(grepl(pattern = "nop", data_use$code[index[count]]), 
           gsub("nop", "jmp", data_use$code[index[count]]),
           gsub("jmp", "nop", data_use$code[index[count]]))
  
  df <- 
    data_use %>% 
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
      if(df$id[df$order==(i-1)]+df$jmp[df$order==(i-1)]>length(df$order)){break}
      if(df$order[df$id[df$order==(i-1)]+df$jmp[df$order==(i-1)]]!=0){break}
        df$order[df$id[df$order==(i-1)]+df$jmp[df$order==(i-1)]] <- i
    }
    
  }
  
  test <- tail(df$order,1)==0
  count = count+1
  #print(count)
}

df %>% 
  filter(order != 0) %>% 
  arrange(order) %>% 
  # mutate(result = cumsum(acc)) %>% 
  summarize(result = sum(acc)) %>% 
  pull(result)
