library(tidyverse)

jolts <- as.numeric(read_lines("day10.txt"))

data <- tibble(jolts = c(0,jolts,max(jolts)+3))

trib_seq = tibble(count = c(0, 1, 2, 3), 
                  trib =  c(1, 2, 4, 7))

perms <- 
  data %>%
  arrange(jolts) %>% 
  mutate(dif = jolts - lag(jolts,1, default = 0)) %>% 
  mutate(running_1 = (dif == lag(dif,1))&(dif == 1)) %>% 
  mutate(run_id = cumsum(running_1==TRUE & lag(running_1,1)==FALSE)) %>% 
  filter(running_1) %>% 
  group_by(run_id) %>% 
  summarise(count= sum(running_1)) %>% 
  left_join(trib_seq, by = "count") %>% 
  pull(trib)

prod(perms) %>% scales::comma()





