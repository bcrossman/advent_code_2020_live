library(tidyverse)
data <- read_lines("day4.txt", sep = " \\n")

data <- scan("day4.txt", what = "character", blank.lines.skip = F)

data %>% 
  separate(col = count, into = c("min_count", "max_count"), sep = "-") %>%
  mutate_at(.vars = c("min_count", "max_count"), .funs = as.numeric) %>% 
  mutate(character = gsub(pattern = ":", replacement = "", x = character)) %>% 
  mutate(check1 = str_sub(password, min_count, min_count)==character,
         check2 = str_sub(password, max_count, max_count)==character) %>% 
  filter((check1|check2)&!(check1&check2)) %>% 
  nrow()