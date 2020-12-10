# --- Day 10: Adapter Array ---

# Find a chain that uses all of your adapters to connect the charging outlet to
# your device's built-in adapter and count the joltage differences between the
# charging outlet, the adapters, and your device. What is the number of 1-jolt
# differences multiplied by the number of 3-jolt differences?
# 
### Make a chain from 0, using all the values in the dataset.
### sort from low to high, count how many have 1 diff, and how many have
### 3 diff.
### wow, scan is much handier!
adapters<- sort(scan("data/day10.txt"))

n_adapters <- length(adapters)
ad_diff <- function(number){
    if(number==1){
       adapters[number]-0
    }else{
        adapters[number]-adapters[number-1]
    }
}

all_diffs <- purrr::map_dbl(1:n_adapters, ad_diff)
max(all_diffs) # nice, so there are no diffs larger than 3
sum(all_diffs==1) * sum(all_diffs==3)
# device has also 3 volt difference.

### part 2 ----
# What is the total number of distinct ways you can arrange the adapters to
# connect the charging outlet to your device?
#### so how can I efficiently count all possibilities?
#### let's do a small piece and expand from there
#### adapters[1:10] # 1  2  5  6  7  8  9 12 15 16
#### 0-1-2 - 5
#### 0-2-5
#### ### 
#### ### if the difference is 3, there is no other way of connecting.
#### # 5, 6,7,8,  9,12
#### 5  ,7,8,  9,12
#### 5 ,8,  9,12
#### 5  ,6,8,  9,12
#### 5  ,6,    9,12
#### ## 3,1,3 diffs 3,4,7 
#### 
device <- max(adapters)+3
device

### fixed: max(adapters), every 3 distance, and the one before.
pos3 <- all_diffs == 3
pos3[which(pos3)-1] <- TRUE
pos3[length(adapters)] <- TRUE

variable_numbers <- adapters[!pos3]
ad_diff2 <- function(number, adapters){
    if(number==1){
        adapters[number]-0
    }else{
        adapters[number]-adapters[number-1]
    }
}

diff_diff <- purrr::map_dbl(seq_along(variable_numbers), ad_diff2, variable_numbers)
## 2147483648 is too low and 2^50 is too high.

# I guess it only multiplies when consequtive 
which(pos3)

boundaries <- which(pos3)
boundaries

index_adapters_sequence <- function(index){
    if(index==1){
        1:boundaries[1]
    }else{
        boundaries[index-1]:boundaries[index]
    }
}

retrieve_values <- function(seq){
    adapters[seq]
}
# test
# retrieve_values(index_adapters_sequence(index)) # 145, 146,147, 149

parse_possiblities <- function(index){
    values <- retrieve_values(index_adapters_sequence(index))
    iterators <- 2:(length(values)-1)
    
}

purrr::map(seq_along(boundaries), index_adapters_sequence)

library(dplyr)
dataset <- 
    data.frame(
    adapter = adapters,
    diff = all_diffs,
    nochange= pos3
)

### I just didn't get it.
# I used https://github.com/bcrossman/advent_code_2020_live/blob/main/day_10_part_2.R
# 