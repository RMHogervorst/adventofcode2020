# https://adventofcode.com/2020/day/1/
# downloaded data from browser
#######
# Before you leave, the Elves in accounting just need you to fix your expense
# report (your puzzle input); apparently, something isn't quite adding up.
#
# Specifically, they need you to find the two entries that sum to 2020 and then
# multiply those two numbers together.
#
# Find the two entries that sum to 2020
expenses <- read.table("data/day1.txt",header = FALSE)
expenses <- expenses$V1
## brute-force approach is combining everything with eachother
## a full auto join?
## 
## But maybe we can exclude some values.
range(expenses) # 28-2002   So, no we cannot exclude any, they thought of this.
all <- expand.grid(left=expenses, right=expenses)
e2020 <- all[all$left + all$right == 2020,] 
e2020$left* e2020$right


# part 2 
all2 <- expand.grid(expenses, expenses, expenses)
e2_2020 <- all2[all2$Var1 + all2$Var2 + all2$Var3 == 2020,] 
components <- unique(as.integer(unlist(e2_2020))) 
purrr::reduce(components, `*`)
