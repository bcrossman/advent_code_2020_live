library(tidyverse)

data <- 
  tibble("raw" =readLines("day16.txt")) %>% 
  mutate(splits = cumsum(raw==""))

rules <- 
  data %>% 
  filter(splits==0) %>% 
  filter(raw!="")  %>% 
  extract(col = raw, into = c("rule", 
                              "low_low_bound", "low_high_bound", 
                              "high_low_bound", "high_high_bound"),
          "(^.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)", 
          convert = T) %>% 
  select(-splits)

myticket <- 
  data %>% 
  filter(splits==1) %>% 
  separate_rows(raw, sep=",") %>% 
  mutate(code = as.numeric(raw)) %>% 
  filter(!is.na(code)) %>% 
  select(code) %>% 
  mutate(ticket = 999,
         position = row_number())

nearbyticket <- 
  data %>% 
  filter(splits==2)%>% 
  separate_rows(raw, sep=",") %>% 
  mutate(code = as.numeric(raw)) %>% 
  filter(!is.na(code)) %>%
  rowid_to_column() %>% 
  select(rowid, code)

complete_set <- 
  full_join(nearbyticket, rules, by = character()) %>% 
  mutate(possible_valid = (code>=low_low_bound)&(code<=low_high_bound)|
           (code>=high_low_bound)&(code<=high_high_bound)) %>% 
  group_by(rowid) %>% 
  summarise(possible_valid = sum(possible_valid)>0,
            code = mean(code)) %>% 
  mutate(invalid = !possible_valid) %>% 
  filter(invalid)

sum(complete_set$code) 

##Part 2

exclude <- complete_set$ticket

all_valid_tickets <- 
  data %>% 
  filter(splits==2)%>% 
  rowid_to_column("ticket") %>% 
  separate_rows(raw, sep=",") %>% 
  mutate(code = as.numeric(raw)) %>% 
  filter(!(code %in% exclude)) %>% 
  group_by(ticket) %>% 
  filter(n()==20) %>% 
  mutate(position = row_number()) %>% 
  select(ticket, position, code) %>% 
  bind_rows(myticket)

bad_positions <- 
  full_join(all_valid_tickets, rules, by = character()) %>% 
  mutate(possible_valid = (code>=low_low_bound)&(code<=low_high_bound)|
           (code>=high_low_bound)&(code<=high_high_bound)) %>% 
  filter(!possible_valid) %>% 
  ungroup() %>% 
  distinct(position, rule) %>% 
  arrange(position)
  
  # group_by(position) %>% 
  # summarize(excl_rule = paste(rule, collapse = ",")) %>% 
  # mutate(excl_rule = strsplit(excl_rule, split=","))

good_tickets <- 
  full_join(all_valid_tickets, rules, by = character()) %>% 
  anti_join(bad_positions)
  left_join(bad_positions) 
  # filter(!(rule %in% excl_rule)) %>% 
  
  good_tickets$good <- TRUE

for(i in 1:nrow(good_tickets)){
  i <- 1
  good_tickets$good[i] <- !(good_tickets$rule[i] %in% good_tickets$excl_rule[[i]])
}

good_tickets_f <- 
  good_tickets %>% 
  filter(good) %>% 
  select(ticket, position, code, rule) 

View(good_tickets %>%
       ungroup() %>% 
       count(position, rule))

