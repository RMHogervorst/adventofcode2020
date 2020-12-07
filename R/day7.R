# https://adventofcode.com/2020/day/7
# 

# How many bag colors can eventually contain at least one shiny gold bag? (The
# list of rules is quite long; make sure you get all of it.)

### this is like a graph, or a tree. 
### If I turn the data into a graph I can look up all connected nodes.
library(purrr)

bagtext <- readLines("data/day7.txt")
# parse text.
# pattern is [word word bags] [contain] [number] [word word bags]
#  dot (.) means end of line and comma (,) means expansion
#  I'm going to start with creating a dataframe with 
#  bags number bags
#  
#  - plural is bags, singular is bag: make them all bag
bag_2_bag <- function(bagtextline){
    bags <- unlist(strsplit(bagtextline, split=" contain |, "))
    res <- data.frame(
        from = bags[[1]],
        to = base::setdiff(bags, bags[[1]])
    )
    res$number <- as.numeric(gsub("[A-z .]+","", res$to))
    res$to <- gsub("[0-9.]+","", res$to)
    ### Normally I would use str_trim here
    res$to <- gsub("^ ", "", res$to)
    res$to <- gsub(" $", "", res$to)
    ## they're good bags, brent
    res$from <- gsub("bags", "bag", res$from)
    res$to <- gsub("bags", "bag", res$to)
    res
}

bag_df <- map_dfr(bagtext, bag_2_bag)

length(unique(bag_df$from, bag_df$to)) # 1197 unique bag types

## I could use igraph/ tidygraph to make this work but I think I'm going to
## try something different here.

bags_above <- function(bag){
    bag_df$from[bag_df$to == bag]
}
bagnumber <- 1
bag_vec <- character(0)
startbag <- "shiny gold bag"
while (bagnumber >0) {
    upper_bags <- bags_above(startbag)
    bagnumber <- length(upper_bags)
    bag_vec <- c(bag_vec, upper_bags)
    
}

retrieve_underlying_bags <- function(bag){
    # done and to do vector
    # while to do is not length(0)
    #   pick the first one of the list
    #   add item to done, add others to to do
    to_retrieve <- bags_above(bag)
    done <- bag
    to_do <- base::setdiff(to_retrieve, done)
    while (length(to_do)>0) {
        to_retrieve <- bags_above(to_do[[1]])
        done <- c(done, to_do[[1]])
        to_do <- setdiff(to_do, done)
        print(paste0(length(done), " done, and ",length(to_do), " to do"))
        if(length(to_retrieve)==0) {
             next
            }else{
                to_do <- c(to_do, base::setdiff(to_retrieve, done))
            }
    }
    done
}
all_above <- retrieve_underlying_bags("shiny gold bag")
length(all_above[all_above != "shiny gold bag"])

### part 2 ------
# How many individual bags are required inside your single shiny gold bag?
## (moving in the other direction)
## but wait, we need to multiply them!
bag_df2 <- bag_df[bag_df$to != "no other bag",]

bags_in_the_bags <- function(bag, number=1){
    res <- bag_df2[bag_df2$from == bag,c("to", "number")]
    res$number = res$number * number
    res
}

yo_dog_I_heard_you_like_bags <- function(bag){
    done <- bag
    doggiebag <- bags_in_the_bags(bag,number = 1)
    todo <- base::setdiff(doggiebag$to, done)
    N_bags <- doggiebag$number
    tododf <- data.frame(to=todo, number=N_bags)
    while(nrow(tododf)>0){
        doggiebag <- bags_in_the_bags(tododf$to[[1]], tododf$number[[1]])
        done <- c(done, tododf$to[[1]])
        tododf <- tododf[!tododf$to %in% done,]
        tododf <- rbind(tododf, doggiebag[!doggiebag$to %in% done,])
        print(paste0(length(done), " done, and ",nrow(tododf), " to do"))
        N_bags <- c(N_bags, doggiebag$number )
    }
    N_bags
}

bags <- yo_dog_I_heard_you_like_bags("shiny gold bag")
sum(bags, na.rm = TRUE) # 119 is too low, 29838 is also too low!
#### this part misses the total number of bags. If I see the same bag again
#### I discard it, but I should add the numbers.
# this one <https://github.com/bcrossman/advent_code_2020_live/blob/main/day_7_part_2.R>
# shows a nice joining example.
## It should be possible to recurse all the way till you get 0 rows back.

bags_in_the_bags_ultimate <- function(bag){
    res <- bags_in_the_bags(bag)
    if(nrow(res)==0){
        1
    }else{
        purrr::map_dbl(res$to, bags_in_the_bags_ultimate)* res$number
    }
}

bags <- bags_in_the_bags_ultimate("shiny gold bag")
# this fails too.


current_bags <- tibble(from="shiny gold bag", number=1)
ultimate_bags <- c()
addl_bags <- 1
while(addl_bags>0){
    step <- 
        left_join(current_bags, bag_df2, by=c("from"="from"))
    
    current_bags <- 
        step %>% 
        select(-from) %>% 
        mutate(number = number.x * number.y) %>% 
        select(from=to, number) %>% 
        group_by(from) %>% 
        summarize(number = sum(number),.groups = "keep") %>% 
        ungroup()
    addl_bags <- sum(current_bags$number, na.rm = T)
    ultimate_bags <- c(ultimate_bags, addl_bags)
    print(paste0(length(ultimate_bags), " done, and ",length(addl_bags), " to do"))
    }

sum(ultimate_bags)
# ### Try graph anyways
# bag_graph <- as_tbl_graph(bag_df2)
# # sink = only incoming edges
# # source only outgoing edges.
# last_bags <- bag_df[bag_df$to == "no other bag",][["from"]]
# bag_graph <- 
#     bag_graph %>% 
#     mutate(endpoint = node_is_sink(),
#            rowid = dplyr::row_number())
# routes <- bag_graph %>% morph(to_subgraph)