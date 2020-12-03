# The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
# "Something's wrong with our computers; we can't log in!" You ask if you can
# take a look.
#
# Their password database seems to be a little corrupted: some of the passwords
# wouldn't have been allowed by the Official Toboggan Corporate Policy that was
# in effect when they were chosen. To try to debug the problem, they have
# created a list (your puzzle input) of passwords (according to the corrupted
# database) and the corporate policy when that password was set.
library(stringr)
library(magrittr)
library(purrr)
passwords <- read.table("data/day2.txt",sep = ":")
names(passwords) <- c("rule","password")
passwords$target <- stringr::str_extract(passwords$rule, "[a-z]{1}")
passwords$bounds <- stringr::str_replace(passwords$rule, "[a-z]{1}", "")
values <- passwords$bounds %>% str_trim(side="both") %>%  str_split('-')
passwords$min <- purrr::map_chr(values, 1)
passwords$max <- purrr::map_chr(values, 2)

# seperate out the letters, count them. compare against target.
# no make a function that takes target, returns count of letter.
teststring <- "xzzz"
stringr::str_split(teststring, pattern = "") %>% unlist() %>% table()

count_character <- function(string, letter){
    res <- stringr::str_split(string, pattern = "") %>% 
        unlist() %>% 
        table() 
    if(!any(names(res)== letter)){
        0
    }else{
        res[[letter]]
    }
    
}
## tests
# count_character(teststring, "x")
# count_character(teststring, "y")

passwords$target_count <- purrr::map2_dbl(passwords$password, passwords$target, count_character)
passwords$target_count
valid <- passwords$min < passwords$target_count & passwords$max > passwords$target_count
sum(valid) # this is wrong. too low.
valid <- passwords$min <= passwords$target_count & passwords$max >= passwords$target_count
sum(valid) # 385
# still too low!
# 
# 
# fill tidy
library(dplyr)
passwords2 <- readr::read_delim("data/day2.txt",delim=":",col_names = c("rule","password"))
tmp2 <- passwords2 %>%
    mutate(
        password = str_trim(password, side="both"),
        rule_min= str_extract(rule,"^[0-9]{1,2}") %>% as.integer(),
        rule_max= str_extract(rule,"-[0-9]{1,2}") %>% str_remove("-") %>% as.integer(),
        target=str_extract(rule, "[a-z]{1,2}"),
        target_list = str_extract_all(password, target),
        target_count = purrr::map_int(target_list, length)
        ) 
tmp2 %>% 
    filter(rule_min <= target_count) %>% 
    filter(rule_max >= target_count) %>% 
    nrow()
# 385  ## too low it says
# check if the wrong ones are indeed wrong.
tmp2 %>% 
    filter(rule_min > target_count)
# # aah! character vs numeric
# 
tmp2 %>% 
    filter(rule_min <= target_count) %>% 
    filter(rule_max >= target_count) %>% 
    nrow() # with correct integer, 524! correct

#
# While it appears you validated the passwords correctly, they don't seem to be
# what the Official Toboggan Corporate Authentication System is expecting.
#
# The shopkeeper suddenly realizes that he just accidentally explained the
# password policy rules from his old job at the sled rental place down the
# street! The Official Toboggan Corporate Policy actually works a little
# differently.
#
# Each policy actually describes two positions in the password, where 1 means
# the first character, 2 means the second character, and so on. (Be careful;
# Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
# these positions must contain the given letter. Other occurrences of the letter
# are irrelevant for the purposes of policy enforcement.
# Given the same example list from above:
#
# 1-3 a: abcde is valid: position 1 contains a and position 3 does not. 
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b. 
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c. 
# 
# How many
# passwords are valid according to the new interpretation of the policies?
tmp2
teststring %>% str_sub(1,1)
tmp2 %>% 
    mutate(
        letterpos1= str_sub(password, start = rule_min, end = rule_min),
        letterpos2= str_sub(password, start = rule_max, end = rule_max),
        valid = letterpos1 == target | letterpos2 == target
    ) %>% 
    filter(valid) %>% 
    nrow() # 741 ## too high, because I used or, should be xor
tmp2 %>% 
    mutate(
        letterpos1= str_sub(password, start = rule_min, end = rule_min),
        letterpos2= str_sub(password, start = rule_max, end = rule_max),
        valid1 = letterpos1 == target,
        valid2 = letterpos2 == target,
        valid = valid1+valid2 == 1
    ) %>% 
    filter(valid) %>% 
    nrow() 
