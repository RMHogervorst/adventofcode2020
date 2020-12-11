#--- Day 11: Seating System ---
    
# All decisions are based on the number of occupied seats adjacent to a given
# seat (one of the eight positions immediately up, down, left, right, or
# diagonal from the seat). The following rules are applied to every seat
# simultaneously:
    
# If a seat is empty (L) and there are no occupied seats adjacent to it, the
# seat becomes occupied. 
# If a seat is occupied (#) and four or more seats
# adjacent to it are also occupied, the seat becomes empty. 
# Otherwise, the seat's state does not change.

# Floor (.) never changes; seats don't move, and nobody sits on the floor.

#### This makes me think about model thinking (coursera) people as
#### atoms. Simple rules that lead to complex behaviors!
#### Like standing ovation model, Conway's game of life, etc.
#
# At this point, something interesting happens: the chaos stabilizes and 
# further applications of these rules cause no seats to change state! 
# Once people stop moving around, you count 37 occupied seats.
#
# Simulate your seating area by applying the seating rules repeatedly until 
# no seats change state. How many seats end up occupied?
input <- readLines("data/day11.txt")
library(purrr)
## for every round, for every seat, calculate state.
### (.) remains
### (L) and all around  also (L) become (#)
### (#) and (4 of all positions are #)  seat become (L)
### my thinking is this: turn it into a matrix, calculate future state by 
### iterating over every element in the matrix, write result to new matrix.
### in the end, calculate difference in matrix. stop if difference is 0.

## Normally I would make this more pretty.
width = 98
height = 91
seat_matrix <- matrix(
    unlist(purrr::map(input, ~unlist(strsplit(.x, split="")))), 
    byrow = TRUE,
    ncol = width, nrow = height)
## check if I did it correct row 1, column 5 L, column 6 . 
## row 6, 1 L 7,1 L
## seat_matrix[1,6] etc.

empty_matrix <- matrix("P",ncol=width, nrow = height) 
# p for problem (if I don't fill it entirely)

all_around <- expand.grid(row=c(1,0, -1), col = c(1,0,-1))
all_around <- all_around[!(all_around$row ==0 & all_around$col==0),]

# This is not strictly necessary, but makes the intentions more clear.
get_value <- function(row, column, matrix){
    matrix[row, column]
}

check_vicinity <- function(row, column, matrix =seat_matrix){
    width = NCOL(matrix)
    height= NROW(matrix)
    rows_to_check = row +all_around$row
    cols_to_check = column +all_around$col
    valid_rows <- rows_to_check >0 & rows_to_check <=height
    valid_cols <- cols_to_check >0 & cols_to_check <= width
    valid <- valid_rows & valid_cols
    purrr::map2_chr(
        .x = rows_to_check[valid],
        .y = cols_to_check[valid],
        .f = get_value, matrix=matrix
    )
}
# test
# check_vicinity(2,4)
# length(check_vicinity(1,1)) ==3

## returns one character
apply_action <-function(row, column, matrix =seat_matrix){
    current  <- get_value(row, column, matrix)
    # (.) remains
    result <- "."
    if(current != "."){
        result <- "L"
        around <- check_vicinity(row, column, matrix)
        occupied <- sum(around == "#")
        # (L) and all around  no occupied seats become (#)
        # (#) and (4 of all positions are #)  seat become (L) 
        if(current == 'L' & occupied ==0){
            result <- "#"
        }
        if(current == "#" & occupied <4){
            result <- "#"
        }
    }
    result
}

step_seats <- function(matrix){
    width = NCOL(matrix)
    height= NROW(matrix)
    new_matrix <- matrix("p", ncol=width, nrow = height)
    for (col in seq_len(width)) {
        for (row in seq_len(height)){
            new_matrix[row,col] <- apply_action(row, col, matrix)
        }
    }
   if(sum(new_matrix=="p")> 0){stop("You did not replace all values")} 
    new_matrix
}
#test <- step_seats(seat_matrix)
# example <- matrix(unlist(strsplit("L.LL.LL.LLLLLLLLL.LLL.L.L..L..LLLL.LL.LLL.LL.LL.LLL.LLLLL.LL..L.L.....LLLLLLLLLLL.LLLLLL.LL.LLLLL.LL","")),ncol=10, byrow=TRUE)
# round1  <- matrix(unlist(strsplit("#.##.##.#########.###.#.#..#..####.##.###.##.##.###.#####.##..#.#.....###########.######.##.#####.##","")),ncol=10, byrow=TRUE)
# round2  <- matrix(unlist(strsplit("#.LL.L#.###LLLLLL.L#L.L.L..L..#LLL.LL.L##.LL.LL.LL#.LLLL#.##..L.L.....#LLLLLLLL##.LLLLLL.L#.#LLLL.##","")),ncol=10, byrow=TRUE)
# sum(step_seats(example) != round1)
# sum(step_seats(step_seats(example)) != round2) ## yes! this is where it is going wrong!
# sum(step_seats(round1) != round2)
# apply_action(1,7,matrix=round1) # fixed
# apply_action(1,5,matrix=round1)

loop_to_no_change <- function(matrix, debug=FALSE){
    identical <- FALSE
    loop <- 0
    old_matrix <- matrix
    while (!identical) {
        matrix <- step_seats(old_matrix)
        identical <- sum(matrix != old_matrix)==0
        loop <- loop +1
        if(debug){
            cat("loop ", loop, " seats occupied: ", sum(matrix=="#"), "\n")
            cat("old_matrix[3,2:6]: ",old_matrix[3,2:6], "\n")
            cat("new_matrix[3,2:6]: ",matrix[3,2:6], "\n")
        }
        old_matrix <- matrix
    }
    matrix
}
result_part_1 <- loop_to_no_change(seat_matrix)
