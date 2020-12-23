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

# while((length(player1)*length(player2))>0){
#  x <-  play_war_round(player1, player2)
#  player1 <- x[[1]]
#  player2 <- x[[2]]
# }  
# max(sum((player1)*((length(player1)+length(player2)):1)),
#     sum((player2)*((length(player1)+length(player2)):1)))

create_seen <- function(player1, player2){
  paste(paste0(player1,collapse = ""), 
        paste0(player2,collapse = ""),
        sep=",")
}

initial_seen <- character(0)

recursive_combat <- function(deck, seen){
  while((length(deck[[1]])*length(deck[[2]]))>0){
    # deck1 <- deck[[1]]
    # deck2 <- deck[[2]]
    # browser()
    current_seen <-  create_seen(deck[[1]], deck[[2]])
    
    if(current_seen %in% seen){
      deck[[1]] <- c(deck[[1]], deck[[2]])
      deck[[2]] <- c()
      return(list(deck[[1]], deck[[2]]))}
    
    seen <- c(seen, current_seen)
    if(!((deck[[1]][1]<length(deck[[1]]))&(deck[[2]][1]<length(deck[[2]])))){
      
      card1 <- deck[[1]][1]
      card2 <- deck[[2]][1]
      if(card1>card2){
        deck[[1]] <- c(deck[[1]][-1], card1, card2)
        deck[[2]] <- c(deck[[2]][-1])
      }else{
        deck[[2]] <- c(deck[[2]][-1], card2, card1)
        deck[[1]] <- c(deck[[1]][-1])
      }
      
      recursive_combat(list(deck[[1]], deck[[2]]), seen)
    }else{
      if(((deck[[1]][1]<length(deck[[1]]))&(deck[[2]][1]<length(deck[[2]])))){
        card1 <- deck[[1]][1]
        card2 <- deck[[2]][1]
        
        if(max(deck[[1]][2:(1+card1)])>max(deck[[2]][2:(1+card2)])){
          deck[[1]] <- c(deck[[1]][-1], card1, card2)
          deck[[2]] <- c(deck[[2]][-1])
        }else{
          deck[[2]] <- c(deck[[2]][-1], card2, card1)
          deck[[1]] <- c(deck[[1]][-1])
        }
        
        recursive_combat(list(deck[[1]], deck[[2]]), seen)
      }
    }
  }
  return(list(deck[[1]], deck[[2]]))
}

result <- recursive_combat(list(player1, player2), initial_seen)

player1 <- result[[1]]
player2 <- result[[2]]

max(sum((player1)*((length(player1)+length(player2)):1)),
    sum((player2)*((length(player1)+length(player2)):1)))
