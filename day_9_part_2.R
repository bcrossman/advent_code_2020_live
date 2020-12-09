library(tidyverse)

data <-  as.numeric(read_lines("day9.txt"))

preamble_length <- 25

sum_n_num <- function(num_set, units, sum_check){
  if(sum_check == 0){return(TRUE)}
  if(units<1|sum_check<0){return(FALSE)} #(preamble_length-nums_to_sum)
  
  var = sum_n_num(num_set, units-1, sum_check - num_set[units])
  var_2 = sum_n_num(num_set, units-1, sum_check)
  return(var|var_2)
}

for(i in ((preamble_length+1):length(data))){
  if(sum_n_num(data[(i-preamble_length):i], preamble_length, data[i])==FALSE){break}
}

target <- data[i]

target

key_vec <- c()
##Part 2
for(contig_len in 2:length(data)){
  if(length(key_vec>0)){break}
  for(i in (contig_len:length(data))){
    if(sum(data[((i-contig_len+1):i)])==target){
      key_vec <-data[((i-contig_len+1):i)]
      break
      }
  }
}

min(key_vec)+max(key_vec)
