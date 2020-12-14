library(tidyverse)
data <- tibble("inst" =readLines("day14.txt"))

intToBits_2 <- function(x){
  paste(sapply(strsplit(paste(rev(intToBits(x))),""),`[[`,2),collapse="")
}

vintToBits <- Vectorize(intToBits_2)

df1 <- 
  data %>% 
  mutate(mask_id = grepl(pattern = "mask",inst),
         mask = ifelse(mask_id, inst, NA)) %>% 
  fill(mask) %>% 
  filter(!mask_id) %>% 
  rowid_to_column() %>% 
  extract(inst, c("Mem", "Entry"), "^mem\\[(\\d+)]\\s=\\s(\\d+)",convert = T) %>% 
  extract(mask, c("mask"), "^mask\\s=\\s(.+)",convert = T) %>% 
  mutate(Entry_bit =   paste0("0000",vintToBits(Entry))) %>% 
  mutate(Check = (strtoi(x = .$Entry_bit, base = 2)==Entry)&
           (str_length(mask)== str_length(Entry_bit))) %>% 
  separate_rows(mask, Entry_bit, sep="") %>% 
  mutate(mask = as.numeric(mask),
         Entry_bit = as.numeric(Entry_bit)) %>% 
  filter(!is.na(Entry_bit)) %>% 
  mutate(final_entry = coalesce(mask, Entry_bit)) %>% 
  select(rowid, Mem, final_entry) %>% 
  group_by(rowid, Mem) %>% 
  mutate(bit_spot = 36-row_number(),
         value = final_entry*2^bit_spot) %>% 
  summarize(final_value = sum(value)) %>% 
  arrange(Mem, rowid) %>% 
  group_by(Mem) %>% 
  slice(n())

sum(df1$final_value) %>% scales::comma()
  
 

         