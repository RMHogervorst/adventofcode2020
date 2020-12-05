## Part 1 ----
# Instead of zones or groups, this airline uses binary space partitioning to
# seat people. A seat might be specified like FBFBBFFRLR, where F means "front",
# B means "back", L means "left", and R means "right".
#
# The first 7 characters will either be F or B; these specify exactly one of the
# 128 rows on the plane (numbered 0 through 127). Each letter tells you which
# half of a region the given seat is in. Start with the whole list of rows; the
# first letter indicates whether the seat is in the front (0 through 63) or the
# back (64 through 127). The next letter indicates which half of that region the
# seat is in, and so on until you're left with exactly one row.
#
# For example, consider just the first seven characters of FBFBBFFRLR:
#
#     Start by considering the whole range, rows 0 through 127.
#     F means to take the lower half, keeping rows 0 through 63.
#     B means to take the upper half, keeping rows 32 through 63.
#     F means to take the lower half, keeping rows 32 through 47.
#     B means to take the upper half, keeping rows 40 through 47.
#     B keeps rows 44 through 47.
#     F keeps rows 44 through 45.
#     The final F keeps the lower of the two, row 44.
# 
# The last three characters will be either L or R; these specify exactly one of
# the 8 columns of seats on the plane (numbered 0 through 7). The same process
# as above proceeds again, this time with only three steps. L means to keep the
# lower half, while R means to keep the upper half.
# 
#     Start by considering the whole range, columns 0 through 7.
#     R means to take the upper half, keeping columns 4 through 7.
#     L means to take the lower half, keeping columns 4 through 5.
#     The final R keeps the upper of the two, column 5.
# 
# As a sanity check, look through your list of boarding passes. What is the
# highest seat ID on a boarding pass?
#
library(purrr)
dataset <- readLines("data/day5.txt")

### turn code into number
# example 
#BFFFBBFRRR: row 70, column 7
all_rows <- 0:127

### logic to partition the data
b_half <- function(seq){
    ceiling(median(seq)):seq[length(seq)]
}
f_half <- function(seq){
    seq[1]:floor(median(seq))
}
### Create seperate instructions for the row.
get_row_sequence <- function(datasetline){
    gsub("[LR]+","",datasetline) %>% 
        strsplit(split = "") %>% 
        unlist()
}
## What I want is to turn the sequence into functions
## so that sequence F F B F F F F turns into 
## all_rows %>% f_half() %>% f_half() %>% b_half() %>% f_half() %>% f_half() %>% f_half() %>% f_half()

choose_function <- function(seats, char=c("F","B")){
    if(char=="F"){f_half(seats)}else{b_half(seats)}
}

get_row_number <- function(row_seq){
    seats <- all_rows
    for (step in seq_len(length(row_seq))) {
        seats <- choose_function(seats, row_seq[[step]])
    }
    seats
}

datasetline_to_row_number <- function(datasetline){
    get_row_number(get_row_sequence(datasetline))
}

### seatnumbers  ----
all_seats <- 0:7


### Create seperate instructions for the row.
get_seat_sequence <- function(datasetline){
    gsub("[FB]+","",datasetline) %>% 
        strsplit(split = "") %>% 
        unlist()
}
# test
#dataset[1] %>% get_seat_sequence()

get_seat_number <- function(seat_seq){
    seats <- all_seats
    for (step in seq_len(length(seat_seq))) {
         if(seat_seq[[step]] == "R"){
             seats <- b_half(seats)
         }else{
             seats <- f_half(seats)
         }
        print(seats)
    }
    seats
}
datasetline_to_seat_number <- . %>% get_seat_sequence() %>% get_seat_number()
#datasetline_to_seat_number(dataset[2])

## seatid seat ID: multiply the row by 8, then add the column. In this example,
## the seat has ID 44 * 8 + 5 = 357.


seats_in_airplane <- data.frame(
    rownumber = purrr::map_int(dataset,datasetline_to_row_number),
    seatnumber = purrr::map_int(dataset, datasetline_to_seat_number)
)
seats_in_airplane$seatID <- seats_in_airplane$rownumber*8 + seats_in_airplane$seatnumber
max(seats_in_airplane$seatID)

### Part 2  ----
# Ding! The "fasten seat belt" signs have turned on. Time to find your seat.

# It's a completely full flight, so your seat should be the only missing
# boarding pass in your list. However, there's a catch: some of the seats at the
# very front and back of the plane don't exist on this aircraft, so they'll be
# missing from your list as well.

# Your seat wasn't at the very front or back, though; the seats with IDs +1 and
# -1 from yours will be in your list.
# What is the ID of your seat?
# 
## So I want an ID that is missing, that is not the lowest and not the hightest.
#
minmax <- range(seats_in_airplane$seatID) # 6-813
all_seats_available <- seq(minmax[1], minmax[2])
missing_seats <- all_seats_available[!all_seats_available %in% seats_in_airplane$seatID]
# leaves only 1