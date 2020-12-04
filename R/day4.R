### part 1 -----
# The automatic passport scanners are slow because they're having trouble
# detecting which passports have all required fields. The expected fields are as
# follows:
#     byr (Birth Year)
#     iyr (Issue Year)
#     eyr (Expiration Year)
#     hgt (Height)
#     hcl (Hair Color)
#     ecl (Eye Color)
#     pid (Passport ID)
#     cid (Country ID)
 #
#Passport data is validated in batch files (your puzzle input). Each passport is
#represented as a sequence of key:value pairs separated by spaces or newlines.
#Passports are separated by blank lines.
#
# Count the number of valid passports - those that have all required fields. 
# Treat cid as optional. In your batch file, how many passports are valid?
# 
# 
# This is actually something I've done recently! I'm parsing r-package 
# DESCRIPTION and NAMESPACE files, and I can use my lessons learned!
# I will flatten the files into a passport each, I'm not going to copy my
# code but think back about the logic and applying it.
library(stringr)
library(purrr)
library(dplyr)

passports <- readLines("data/day4.txt")

flatten_piece <- function(first_line, last_line, data=passports){
    paste0(data[first_line:last_line],collapse = " ")
}
# test
# flatten_piece(first_lines[1], last_lines[1])

flatten_passports <- function(passports){
    empty_newlines <- str_detect(passports,"^$")
    total_lines <- seq_len(length(passports))
    # the line above the empty line and last line
    last_lines <- c(total_lines[empty_newlines]-1, length(passports))
    # first lines are above
    first_lines <-c(1, total_lines[empty_newlines]+1)
    purrr::map2_chr(first_lines, last_lines, flatten_piece)
}

all_passports <- flatten_passports(passports)
## checked 6,7,8 to see if they are correctly splitted, they were
## checked 292 to see if it was indeed the last in the dataset. also yes

#And than we can turn it into a new list, and then a dataframe.

make_labels_list <- function(single_passport){
    # split character vector on the spaces return the charachter vector
    vec <- single_passport %>% str_split(pattern = " ") %>% .[[1]]
    # extract the keys (before the :)
    keys= vec %>% str_extract_all("^.+:") %>% str_remove(":")
    # and values (after the :)
    values = vec %>% str_remove_all("^.+:")
    # return named elements
    values %>% setNames(keys)
}
# test
# make_labels_list(all_passports[[2]])

parsed_passports <- purrr::map(all_passports, make_labels_list)
# this is now a list
# and the beauty of bind_rows is that it is happy to parse this
# into a dataframe with NA when not found.
parsed_passports_df <- dplyr::bind_rows(parsed_passports)

important_fields <- parsed_passports_df %>% 
    select(-cid)
    # there is probably a nice rowwise or across possible
    # here
validness_logic <- apply(important_fields, 2, is.na)

valid_passports<- parsed_passports_df[rowSums(validness_logic) ==0 ,]
nrow(valid_passports)
### part 2 -----

# You can continue to ignore the cid field, but each other field has strict
# rules about what values are valid for automatic validation:
    
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#     If cm, the number must be at least 150 and at most 193.
#     If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.


# Count the number of valid passports - those that have all required fields and
# valid values. Continue to treat cid as optional. In your batch file, how many
# passports are valid?
# 
# 
# Probably a nice place to use the validate package
# https://github.com/data-cleaning/validate
# but I'm going to filter rows on meeting criteria.
# I can just continue with the valid passports 
# and add the rules.
valid_passports %>%  # 245
    # birth year byr
    mutate(byr= as.integer(byr)) %>% 
    filter(byr >= 1920 & byr <=2002) %>% # 184 left
    #issue year
    mutate(iyr = as.integer(iyr)) %>% 
    filter(iyr >= 2010 & iyr <=2020) %>% #162 left
    # expiration year
    mutate(eyr= as.integer(eyr)) %>% 
    filter(eyr >= 2020 & eyr<=2030) %>%  # 151 left
    # hair color
    filter(str_detect(hcl, "^#[0-9a-f]{6}$")) %>%  # 148 left
    # eye color
    filter(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% #143 left
    # pasport id
    filter(str_detect(pid, "^[0-9]{9}$")) %>%  # 136 (without caret^ and dollarsign $ it would match one more)
    #height
    mutate(
        unit = str_extract(hgt,"in|cm"),
        hgt  = str_remove(hgt,"in|cm"),
        hgt = as.integer(hgt)
           ) %>% 
    filter(!is.na(unit)) %>% # 135
    mutate(hgt_valid = case_when(
        unit == "in" & hgt>=59 & hgt <=76 ~ "valid",
        unit == "cm" & hgt>=150 & hgt <=193 ~ "valid",
        TRUE ~ "invalid"
    )) %>% 
    filter(hgt_valid == "valid") %>% 
    nrow() 
