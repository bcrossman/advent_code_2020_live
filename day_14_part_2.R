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
  mutate(Mem_bit =   paste0("0000",vintToBits(Mem))) %>% 
  mutate(Check = (strtoi(x = .$Mem_bit, base = 2)==Mem)&
           (str_length(mask)== str_length(Mem_bit))) %>% 
  separate_rows(mask, Mem_bit, sep="") %>% 
  filter(!mask=="") %>% 
  mutate(mask = gsub(pattern = "0", replacement = NA, mask)) %>% 
  # filter(!is.na(Entry_bit)) %>% 
  mutate(final_mem = coalesce(mask, Mem_bit)) %>% 
  select(rowid, Entry, final_mem) %>% 
  group_by(rowid, Entry) %>% 
  mutate(bit_spot = 36-row_number()) %>% 
  ungroup() %>% 
  left_join(data.frame("final_mem" = "X", "seq" = c("0","1"))) %>% 
  mutate(final_final_mem = coalesce(seq, final_mem),
         mem = as.numeric(final_final_mem)*2^bit_spot) %>% 
  select(rowid, Entry, bit_spot, mem) %>% 
  pivot_wider(id_cols = c("rowid", "Entry"), names_from = bit_spot, values_from = mem) %>% 
  unnest(`35`) %>% unnest( `34`) %>% unnest( `33`) %>% unnest( `32`) %>% 
  unnest( `31`) %>% unnest( `30`) %>% unnest( `29`) %>% unnest( `28`) %>% 
  unnest( `27`) %>% unnest(`26`) %>% unnest( `25`) %>% unnest( `24`) %>% 
  unnest( `23`) %>% unnest( `22`) %>% unnest( `21`) %>% unnest( `20`) %>%
  unnest( `19`) %>% unnest( `18`) %>% unnest( `17`) %>% unnest(`16`) %>% 
  unnest( `15`) %>% unnest( `14`) %>% unnest( `13`) %>% unnest( `12`) %>% 
  unnest( `11`) %>% unnest( `10`) %>% unnest( `9`) %>% unnest( `8`) %>% 
  unnest( `7`) %>% unnest( `6`) %>% unnest(`5`) %>% unnest( `4`) %>% 
  unnest( `3`) %>% unnest( `2`) %>% unnest( `1`) %>% unnest( `0`)

df1$Mem = rowSums(df1 %>% select(`35`:`0`))

df2 <- 
  df1 %>% 
  arrange(Mem, rowid) %>% 
  group_by(Mem) %>% 
  slice(n())

sum(df2$Entry) %>% scales::comma()
