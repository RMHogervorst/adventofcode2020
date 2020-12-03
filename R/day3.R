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
N_trees <- function(ride_sequence){
    sum(ride_sequence=="#")
}
N_trees(create_sequenceround(startpos))
sum(create_sequenceround(startpos)=="#")
### Part 2 ----------

# Determine the number of trees you would encounter if, for each of the
# following slopes, you start at the top-left corner and traverse the map all
# the way to the bottom:
    
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

# In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s)
# respectively; multiplied together, these produce the answer 336.

# What do you get if you multiply together the number of trees encountered on
# each of the listed slopes?
# 


### 
# So now it is wise to generalize the next_step function.

step_down_slope <- function(position, rowstep, colstep){
    row <- position[1]+rowstep
    col <- position[2]+colstep
    col <- ifelse(col > 31, col-31, col)
    c(row, col)
}
## and to make the steps dynamic.

toboggan_ride <- function(startpos, down, right){
    steps <- ceiling(max_row/down)
    values <- character(length = steps)
    
    pos <- startpos
    for (step in seq_len(steps)) {
        values[step] <- retrieve_value(pos)
        pos <- step_down_slope(pos, down, right)
    }
    values
}
r1d1 <- toboggan_ride(startpos, 1,1)
r3d1 <- toboggan_ride(startpos, 1,3)
## this should be the same as previous
#all(r3d1 == create_sequenceround(startpos))
r5d1 <-toboggan_ride(startpos, 1,5)
r7d1 <-toboggan_ride(startpos, 1,7)
r1d2 <-toboggan_ride(startpos, 2,1)

list(r1d1,r3d1,r5d1,r7d1,r1d2) %>% 
    purrr::map_dbl(N_trees) %>% # dbls in stead of integers because int gives an overflow!
    purrr::reduce( `*`)
