library(tidyverse)
library(unglue)
data <- readLines("day22.txt")

data_df <- 
  unglue_data(data, c('{player}:',
                                '{card=\\d+}')) %>% 
  fill(player) %>% 
  filter(!is.na(card)) %>% 
  group_by(player) %>% 
  group_split()

player1 <- data_df[[1]] %>% pull(card) %>% as.numeric()

player2 <- data_df[[2]] %>% pull(card) %>% as.numeric()

play_war_round <- function(deck1, deck2){
  if(deck1[1]>deck2[1]){
    deck1 <- c(deck1[-1], deck1[1], deck2[1])
    deck2 <- c(deck2[-1])
  }else{
    deck2 <- c(deck2[-1], deck2[1], deck1[1])
    deck1 <- c(deck1[-1])
  }
  return(list(deck1, deck2))
}

while((length(player1)*length(player2))>0){
 x <-  play_war_round(player1, player2)
 player1 <- x[[1]]
 player2 <- x[[2]]
}  
max(sum((player1)*((length(player1)+length(player2)):1)),
    sum((player2)*((length(player1)+length(player2)):1)))

