### steering through a forest
# You start on the open square (.) in the top-left corner and need to reach the
# bottom (below the bottom-most row on your map). 
# 
# Starting at the top-left
# corner of your map and following a slope of right 3 and down 1, how many trees
# would you encounter?
# 
# '#' means tree and '.' means open square.
# 
# Do we go to start again?
library(dplyr)
library(stringr)
hill <- readr::read_table("data/day3.txt",col_names = "hill") %>% 
    pull(hill) %>%  
    str_split(pattern = "") %>% 
    unlist() %>% 
    matrix(nrow = 323,ncol=31, byrow = TRUE)

## some checks if I did it right.
hill[1,7]=="#"
hill[6,1]=="#"
hill[5,1]=="."

max_col <- NCOL(hill)
max_row <- NROW(hill)
startpos <- c(1,1)
move_one_step <- function(position){
    row <- position[1]
    col <- position[2]
    c(row+1L, col+3L)
}
# test
# move_one_step(startpos) == c(2,4)
retrieve_value <- function(position){
    hill[position[1],position[2]]
}
# test
#retrieve_value(startpos) == "."
create_sequence <- function(startpos){
    max_length <- ceiling(max_row / 1)
    max_width <- ceiling(max_col / 3)
    steps <- min(max_length, max_width)
    values <- character(length = steps)
    pos <- startpos
    for (step in seq_len(steps)) {
        values[step] <- retrieve_value(pos)
        pos <- move_one_step(pos)
    }
    values
}

create_sequence(startpos) # "." "#" "." "#" "." "." "." "." "." "#"
# let's manually check that, it's wrong, I didn't order them correclty
create_sequence(startpos) #"." "." "." "#" "#" "#" "#" "#" "." "#"
# almost correct, I need to add a final step, it stops too early

## So how many trees would we encounter?
trees <- sum(create_sequence(startpos)=="#")# 6 is not the right answer.
## do we loop around? Suggestions of 'the same pattern repeats many times'
## and 'traversing the map' say so.


move_one_step_round <- function(position){
    row <- position[1]+1
    col <- position[2]+3
    col <- ifelse(col > 31, col-31, col)
    c(row, col)
}

create_sequenceround <- function(startpos){
    steps <- max_row
    values <- character(length = steps)
    
    pos <- startpos
    for (step in seq_len(steps)) {
        values[step] <- retrieve_value(pos)
        pos <- move_one_step_round(pos)
    }
    values
}
sum(create_sequenceround(startpos)=="#")
