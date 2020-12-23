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
