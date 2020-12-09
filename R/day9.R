# --- Day 9: Encoding Error ---
#  Part 1 ----
## Every line is a number
# almost every number is the sum of two of the previous 5 numbers
# (after preamble of 5?)
# 
##### Assignment
# The first step of attacking the weakness in the XMAS data is to find the first
# number in the list (after the preamble) which is not the sum of two of the 25
# numbers before it. What is the first number that does not have this property?

#  #### thoughts
# So for every number extract the previous 25 numbers
# make all combinations (but not with itself)
# if one of the combinations matches the number it is valid
numbers <- readLines("data/day9.txt")
# a 1000 numbers.
# start at 26 to end

get_source_data <- function(index, size=25){
    stopifnot(index >size)
    as.numeric(numbers[(index-size):(index-1)])
}
get_value <- function(index){
    as.numeric(numbers[index])
}
# length(get_source_data(27), 25) == 25
XMAS_valid_number <- function(source_data, number){
    df <- expand.grid(a = source_data, b=source_data)
    sums <- (df$a+df$b)[df$a != df$b]
    sum_values <- unique(sums)
    if(sum(sum_values == number) ==1){
        "valid"
    }else{
            "invalid"
        }
}

XMAS_validator <- function(index, size=25){
    XMAS_valid_number(
        source_data = get_source_data(index, size), 
        number = get_value(index)
        )
}

results <- purrr::map_chr(26:length(numbers), XMAS_validator, size=25)
get_value(which(results == "invalid")+25 ) # 387360330 too low.
## I forgot to add the offset! 


## Part 2 ----- 

