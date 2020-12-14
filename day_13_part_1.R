library(tidyverse)
data <- readLines("day13.txt")

target <- as.numeric(data[1])

times <- data[2] %>% as_tibble %>% separate_rows(1, sep = ",") %>% pull(value) %>% as.numeric()

wait <- min(-(target %% times) + times, na.rm = T)

bus <- times[which.min(-(target %% times) + times)]

bus*wait