library(tidyverse)

data <- data.frame(data= scan("day4.txt", what = "character", blank.lines.skip = F))

data %>% 
  mutate(id = if_else(data == "", 1,0)) %>% 
  mutate(id = cumsum(id)+1) %>% 
  filter(data != "") %>% 
  separate(data, into = c("key", "value"), sep=":") %>% 
  pivot_wider(id_cols = id, names_from = "key", values_from="value") %>% 
  mutate(VALID = (rowSums(is.na(.))-as.numeric(is.na(cid)))<1) %>%
  mutate_at(c("byr", "iyr", "eyr"), as.numeric) %>% 
  filter(grepl(x = byr, pattern = "\\d{4}"),
         byr >=1920 & byr <=2002) %>% 
  filter(grepl(x = iyr, pattern = "\\d{4}"),
         iyr >=2010 & iyr <=2020) %>% 
  filter(grepl(x = eyr, pattern = "\\d{4}"),
         eyr >=2020 & eyr <=2030) %>% 
  separate(hgt, into = c("hgt_val", "hgt_type"), sep = "(?=[[:alpha:]][[:alpha:]])", convert=T) %>% 
  filter((hgt_type == "cm" & hgt_val >=150 & hgt_val <=193)| 
           (hgt_type == "in" & hgt_val >=59 & hgt_val <=76)) %>% 
  filter(grepl(x = hcl, pattern = "^#{1}[[:alnum:]]{6}$")) %>% 
  filter(!grepl(x = hcl, pattern = "[g-z]+")) %>% 
  filter(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% 
  filter(grepl(x = pid, pattern = "^[[:digit:]]{9}$")) %>% 
  summarise(result = sum(VALID)) %>% 
  pull(result)


