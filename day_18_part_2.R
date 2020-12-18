library(tidyverse)

data <- readLines("day18.txt")
# data <- "1 + 2 * 3 + 4 * 5 + 6"
# "%*%" <- function(x,y){x*y}
"%+%" <- function(x,y){x+y}
evaluate <- function(x){eval(parse(text=x))}

# data <- gsub(pattern = "*", replacement = "%*%", x = data, fixed = T)
data <- gsub(pattern = "+", replacement = "%+%", x = data, fixed = T)

sum(map_dbl(data, evaluate)) %>% format(scientific=FALSE)



