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
number_rules <- all[1:20]
my_ticket <- all[23]
nearby_tickets <- all[26:length(all)]

# extract ranges
library(stringr)
library(purrr)
ranges <- number_rules %>%
  str_extract_all("[0-9-]+") %>%
  unlist()
### create sequences for every valid range and reduce to unique numbers.
### Any number is tested against the valid numbers.
number_rules <- data.frame(
  from = ranges %>% str_split("-") %>% purrr::map_chr(1) %>% as.integer(),
  to = ranges %>% str_split("-") %>% purrr::map_chr(2) %>% as.integer()
)

valid_numbers <-
  map2(number_rules$from, number_rules$to, ~ seq.int(from = .x, to = .y, by = 1)) %>%
  unlist() %>%
  unique()
## for this task, we do not care what the ticketnumber is. we only want the
## unvalid numbers
all_tickets <- nearby_tickets %>%
  str_split(",") %>%
  unlist() %>%
  as.integer()

sum(all_tickets[!all_tickets %in% valid_numbers])


### part 2  ----
# Now that you've identified which tickets contain invalid
# values, discard those tickets entirely. Use the remaining valid tickets to
# determine which field is which.
#
# # I need to go back and match against the invalids
invalid_ticket_numbers <- all_tickets[!all_tickets %in% valid_numbers]

# test <- nearby_tickets[2]
invalidate_tickets <- function(ticket) {
  nmbrs <- ticket %>%
    str_split(",") %>%
    unlist() %>%
    as.integer()
  any(nmbrs %in% invalid_ticket_numbers)
}
valid_tickets <-
  nearby_tickets[!map_lgl(nearby_tickets, invalidate_tickets)]
# 190, like other people.



make_ticket_rules_df <- function() {
  rules <- all[1:20]
  part1 <- rules %>%
    str_extract(":.+or") %>%
    str_extract("[0-9]+-[0-9]+") %>%
    str_split("-")
  part2 <- rules %>%
    str_extract("or .+$") %>%
    str_remove("or ") %>%
    str_split("-")

  data.frame(
    rule_name = rules %>% str_extract("^.+:") %>% str_remove(":"),
    from_1 = as.integer(map_chr(part1, 1)),
    to_1 = as.integer(map_chr(part1, 2)),
    from_2 = as.integer(map_chr(part2, 1)),
    to_2 = as.integer(map_chr(part2, 2))
  )
}

ticket_rules <- make_ticket_rules_df()


### my idea is to extract other tickets and mine per position.
### hold against all rules.
extract_position <- function(col, tickets = valid_tickets) {
  tickets %>%
    str_split(",") %>%
    purrr::map_chr(col) %>%
    as.integer()
}

extract_position(1, valid_tickets) # seems to work.


rules_column_match <- matrix(1, ncol = 20, nrow = 20)

for (ticket_position in 1:20) {
  for (rule in 1:nrow(ticket_rules)) {
    tickets <- c(extract_position(ticket_position,tickets = valid_tickets), extract_position(column, my_ticket))
    part1 <- (tickets >= ticket_rules$from_1[rule] & tickets <= ticket_rules$to_1[rule])
    part2 <- (tickets >= ticket_rules$from_2[rule] & tickets <= ticket_rules$to_2[rule])
    rules_column_match[ticket_position, rule] <-
      as.integer(
          sum((part1 | part2) ) == length(tickets)
      )
  }
}

# matrix has a row for every ticket positions
rownames(rules_column_match)<- 1:20
# and a column for every rule.
colnames(rules_column_match) <- ticket_rules$rule_name 

ticket_rules$position <- NA
while(NROW(rules_column_match)>1 | NCOL(rules_column_match) > 1){
    unique_valid_rule_idx <- which(colSums(rules_column_match) ==1)
    if(length(unique_valid_rule_idx)>0){
        for (rule in unique_valid_rule_idx) {
            rule_name <- colnames(rules_column_match)[rule]
            position <- which(rules_column_match[,rule]==1)
            ticket_rules$position[
                ticket_rules$rule_name == rule_name
            ] <- as.integer(rownames(rules_column_match)[position])
            rules_column_match <- rules_column_match[-position,-rule]
        }
    }
    
    unique_valid_pos_idx <- which(rowSums(rules_column_match) ==1)
    if(length(unique_valid_pos_idx)>0){
        for (pos in unique_valid_pos_idx) {
            rule_pos <- which(rules_column_match[pos,]==1)
            ticket_rules$position[
                ticket_rules$rule_name == colnames(rules_column_match)[rule_pos]
            ] <- as.integer(rownames(rules_column_match)[pos])
            rules_column_match <- rules_column_match[-pos,-rule_pos]
        }
    }
}
ticket_rules$position[is.na(ticket_rules$position)] <- setdiff(1:20, as.integer(na.omit(ticket_rules$position)))

prod(purrr::map_dbl(ticket_rules[1:6,"position"], extract_position, tickets=my_ticket) )
