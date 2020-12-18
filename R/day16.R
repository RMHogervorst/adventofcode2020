# --- Day 16: Ticket Translation ---
#
#
# The rules for ticket fields specify a list of fields that exist somewhere on
# the ticket and the valid ranges of values for each field. For example, a rule
# like class: 1-3 or 5-7 means that one of the fields in every ticket is named
# class and can be any value in the ranges 1-3 or 5-7 (inclusive, such that 3
# and 5 are both valid in this field, but 4 is not).
# 
# # find values not valid for any field.
# dataset contains three pieces: rules, your ticket, and nearby tickets.
all <- readLines("data/day16.txt")
rules <- all[1:20]
my_ticket <- all[23]
nearby_tickets <- all[26:length(all)]

# extract ranges
library(stringr)
library(purrr)
ranges <- rules %>% str_extract_all("[0-9-]+") %>% unlist()
### create sequences for every valid range and reduce to unique numbers. 
### Any number is tested against the valid numbers.
rules <- data.frame(
    from= ranges %>% str_split('-') %>% purrr::map_chr(1) %>% as.integer(),
    to = ranges %>% str_split('-') %>% purrr::map_chr(2) %>% as.integer()
)

valid_numbers <- 
    map2(rules$from, rules$to, ~seq.int(from=.x, to=.y, by=1)) %>% 
    unlist() %>% 
    unique()
## for this task, we do not care what the ticketnumber is. we only want the
## unvalid numbers
all_tickets <- nearby_tickets %>% str_split(",") %>% unlist()%>% as.integer()

sum(all_tickets[! all_tickets %in% valid_numbers])
