library(tidyverse)
library(unglue)
data <- readLines("day19.txt")

data_df <- unglue_data(data, c('{rule_no}: {rule1=[[:alnum:]]+} {rule2=[[:alnum:]]+} | {rule3=[[:alnum:]]+} {rule4=[[:alnum:]]+}',
                               '{rule_no}: {rule1=[[:alnum:]]+} {rule2=[[:alnum:]]+}',
                               '{rule_no}: {rule1=[[:alnum:]]+} | {rule3=[[:alnum:]]+}',
                               '{rule_no}: {rule1=[[:alnum:]]+}',
                               '{rule_no}: "{rule1}"',
                               "{entry=[a-b]+}"))

rules <- 
  data_df %>% 
  filter(is.na(entry),
         !is.na(rule_no)) %>% 
  select(-entry) %>% 
  mutate(rule_no = as.numeric(rule_no)) %>% 
  arrange(rule_no)

entries <- 
  data_df %>% 
  filter(!is.na(entry)) %>% 
  select(entry)

paste_na <- function(vec) {paste(vec[!is.na(vec)], collapse = "")}

build_regex <- function(x){
  # x <- NA
  result <- rules$rule1[x+1]
  if(is.na(as.numeric(rules$rule1[x+1]))){return(result)}else{
    
    var1 <- paste_na(c(build_regex(as.numeric(rules$rule1[x+1])),
                     build_regex(as.numeric(rules$rule2[x+1]))))
    var2 <- paste_na(c(build_regex(as.numeric(rules$rule3[x+1])),
                     build_regex(as.numeric(rules$rule4[x+1]))))
      
  return(paste0('(',var1,"|",var2, ')'))
  }
}

rule_string <- build_regex(0)

rule_string <- paste0("^", rule_string,"$")
rule_string <- gsub(pattern = "|)", replacement = ")", x = rule_string, fixed = T)

sum(str_detect(entries$entry, rule_string))
