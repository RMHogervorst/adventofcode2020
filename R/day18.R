### day 18 --- Day 18: Operation Order ---
### part 1  ----
### math done in order from left to right...
library(stringr)
homework <- readLines("data/day18.txt")
testresults <- tibble::tribble(
    ~example, ~ result,
    "2 * 3 + (4 * 5)", 26,
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",437,
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",12240,
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2",13632
)

### So, I have to create a faulty math parser!
## Functions ----
round_brackets <- function(line){
    unlist(stringr::str_extract_all(line,"\\([0-9 +\\*]+\\)"))
}
# test
# length(round_brackets(testresults$example[[4]])) ==2

extract_bracket_parts <- function(line){
    round_brackets(line) %>% 
        str_remove_all("\\(") %>% 
        str_remove_all("\\)")
}
# test
# extract_bracket_parts(testresults$example[2]) == "8 * 3 + 9 + 3 * 4 * 3"

solve_left_to_right <- function(text_part){
   with_round_brackets <- str_replace_all(text_part, "([0-9]) ([+*])",replacement = "\\1) \\2")
   n_rb <- with_round_brackets %>% str_count("\\)")
   res <- paste0(paste0(rep("(",n_rb),collapse = ""), with_round_brackets, collapse = "")
   eval(parse(text=res))
}
#solve_left_to_right("1 + 2 * 3 + 4 * 5 + 6") ==71

make_regex_safe <- function(text_part){
    text_part %>% 
        str_replace_all("\\+", "\\\\+") %>% 
        str_replace_all("\\*","\\\\*")
}
add_brackets <- function(text_part){
    text_part %>% 
        str_replace_all("$","\\\\)") %>% 
        str_replace_all("^","\\\\(")
}

replace_brackets_with_result <- function(line){
    bracket_parts <- extract_bracket_parts(line)
    replacements <- purrr::map_dbl(bracket_parts, solve_left_to_right)
    bracket_regex <- make_regex_safe(bracket_parts) %>% add_brackets()
    replacing_set <-setNames(as.character(replacements),bracket_regex)
    str_replace_all(line, replacing_set)
}

solve_line <- function(line,debug=FALSE){
    while (str_count(line,"\\(")>0) {
        line <- replace_brackets_with_result(line)
    }
    if(debug){cat(line,"\n")}
    solve_left_to_right(line)
}
## test
#purrr::map_dbl(testresults$example, solve_line, debug=TRUE) == testresults$result
#### solve part 1  ----
results <- purrr::map_dbl(homework,solve_line)
part1 <- sum(results)


### part 2  -----
# my approach:
# two replacement steps
# - replace 
# - add brackets around every number + number
# 
adv_testcases <-tibble::tribble(
        ~example, ~ result,
        "1 + 2 * 3 + 4 * 5 + 6", 231,
        "1 + (2 * 3) + (4 * (5 + 6))", 51,
        "2 * 3 + (4 * 5)",46,
        "5 + (8 * 3 + 9 + 3 * 4 * 3)",1445,
        "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",669060,
        "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340,
        "9 + 2 + 8 + 9 * 9 + 3 * 1 + 2 + 4", 2352,
        "(7 + 2) + 2 + 8 + 9 * 9 + (8 + (7 * 8 * 9 * 6) * 8 + 9)", 1443484,
        "8 + 3024 * 8 + 9", 3032 * 17
    )

brackets_around_plus_seq <- function(text_part) {
    text_part %>% 
        str_replace_all("([0-9]+) ([\\+ 0-9]+)", "(\\1 \\2\\) ")
}

solve_advanced <- function(text_part){
    with_plus_brackets <-brackets_around_plus_seq(text_part)
    eval(parse(text=with_plus_brackets))
}
# test
# solve_advanced(adv_testcases$example[[1]]) == adv_testcases$result[[1]]
# solve_advanced(adv_testcases$example[[9]]) == adv_testcases$result[[9]]

replace_brackets <- function(line){
    bracket_parts <- extract_bracket_parts(line)
    replacements <- purrr::map_dbl(bracket_parts, solve_advanced)
    bracket_regex <- make_regex_safe(bracket_parts) %>% add_brackets()
    replacing_set <-setNames(as.character(replacements),bracket_regex)
    str_replace_all(line, replacing_set)
}
# test
# solve_advanced(replace_brackets(adv_testcases$example[[3]])) == adv_testcases$result[[3]]
# solve_advanced(replace_brackets(adv_testcases$example[[4]])) == adv_testcases$result[[4]]
# replace_brackets(replace_brackets(adv_testcases$example[[8]])) == (9 + 2 + 8 + 9)*(9 + 51544)
solve_adv_line <- function(line,debug=FALSE){
    while (str_count(line,"\\(" ) > 0) {
        line <- replace_brackets(line)
    }
    if(debug){cat(line,"\n")}
    solve_advanced(line)
}
## test
#purrr::map_dbl(adv_testcases$example, solve_adv_line, debug=FALSE) == adv_testcases$result
results2 <- purrr::map_dbl(homework,solve_adv_line)
part2 <- sum(results2)
