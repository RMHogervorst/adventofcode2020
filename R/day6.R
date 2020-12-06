# --- Day 6: Custom Customs ---
# part 1  -----
# As your flight approaches the regional airport where you'll switch to a much larger plane, customs declaration forms are distributed to the passengers.

# The form asks a series of 26 yes-or-no questions marked a through z. All you
# need to do is identify the questions for which anyone in your group answers
# "yes". Since your group is just you, this doesn't take very long.
#
# However, the person sitting next to you seems to be experiencing a language
# barrier and asks if you can help. For each of the people in their group, you
# write down the questions for which they answer "yes", one per line. For
# example:

# abcx
# abcy
# abcz

# In this group, there are 6 questions to which anyone answered "yes": a, b, c,
# x, y, and z. (Duplicate answers to the same question don't count extra; each
# question counts at most once.)

# Another group asks for your help, then another, and eventually you've
# collected answers from every group on the plane (your puzzle input). Each
# group's answers are separated by a blank line, and within each group, each
# person's answers are on a single line. For example:
#
# abc
#
# a
# b
# c
#
# ab
# ac
#
# a
# a
# a
# a
#
# b
#
# This list represents answers from five groups:
#
# The first group contains one person who answered "yes" to 3 questions: a, b, and c.
# The second group contains three people; combined, they answered "yes" to 3 questions: a, b, and c.
# The third group contains two people; combined, they answered "yes" to 3 questions: a, b, and c.
# The fourth group contains four people; combined, they answered "yes" to only 1 question, a.
# The last group contains one person who answered "yes" to only 1 question, b.
# In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.
#
# For each group, count the number of questions to which anyone answered "yes".
# What is the sum of those counts?
#
#### So like before groups are splitted by whitespace lines
#### But every line is a person answering yes to questions
#### so per group find the union of answers. all the unique letters.
####
#### So I'm thinking splitting up the data into groups, concat together
#### run unique, and count
####
#### I'm reusing some code from day4, changed some names and
#### functionality though.
library(purrr)
groupanswers <- readLines("data/day6.txt")

flatten_piece <- function(first_line, last_line, data = groupanswers) {
  paste0(data[first_line:last_line], collapse = "")
}

flatten_answers <- function(groupanswers) {
  empty_newlines <- grepl("^$", groupanswers)
  total_lines <- seq_len(length(groupanswers))
  # the line above the empty line and last line
  last_lines <- c(total_lines[empty_newlines] - 1, length(groupanswers))
  # first lines are above
  first_lines <- c(1, total_lines[empty_newlines] + 1)
  purrr::map2_chr(first_lines, last_lines, flatten_piece)
}

all_group_answers <- flatten_answers(groupanswers)
# test
# all_group_answers[1]

count_unique <- function(x) {
  length(unique(unlist(strsplit(x, ""))))
}
# test
# count_unique("abcdefgaaa") == 7
#
lengths_group_answers <- purrr::map_int(all_group_answers, count_unique)
sum(lengths_group_answers)

##### Part 2 ----
# You don't need to identify the questions to which anyone
# answered "yes"; you need to identify the questions to which everyone answered
# "yes"!

### Now I could change my flattening method, because I lose the number of rows
### in the data with that step. But we already have the information of rows in
### steps before: the first and last lines

flatten_answers2 <- function(groupanswers) {
  empty_newlines <- grepl("^$", groupanswers)
  total_lines <- seq_len(length(groupanswers))
  # the line above the empty line and last line
  last_lines <- c(total_lines[empty_newlines] - 1, length(groupanswers))
  # first lines are above
  first_lines <- c(1, total_lines[empty_newlines] + 1)
  data.frame(
    persons = last_lines - first_lines + 1,
    answers = purrr::map2_chr(first_lines, last_lines, flatten_piece)
  )
}
answers_with_people <- flatten_answers2(groupanswers)
# checkif the first are indeed 2 lines, and the second 4

characters_more_than_x <- function(groupdata, x) {
  loose_char <- unlist(strsplit(groupdata, ""))
  count_char <- table(loose_char) # table makes a count
  length(count_char[count_char >= x])
}
# test
# characters_more_than_x(answers_with_people$answers[[1]], 2)

answers_with_people$everyone <-
  purrr::map2_int(
    answers_with_people$answers,
    answers_with_people$persons,
    characters_more_than_x
  )
sum(answers_with_people$everyone)
