# --- Day 15: Rambunctious Recitation ---

puzzle_input <- as.integer(c(0,14,6,20,1,4))

history <- c(0,3,6,0,3,3)
speak_number <- function(history){
    l_his <- length(history)
    last_spoken <- history[l_his]
    repeats <- which(history == last_spoken)
    if(length(repeats) ==1){
        age <- 0
    }else{
       age <-repeats[length(repeats)] - repeats[(length(repeats) -1)]
    }
    age
}

speak_number(c(0,3,6))==0
speak_number(c(0,3,6,0)) == 3
speak_number(c(0,3,6,0,3)) == 3
speak_number(c(0,3,6,0,3,3)) == 1
speak_number(c(0,3,6,0,3,3,1)) == 0
speak_number(c(0,3,6,0,3,3,1,0)) == 4
speak_number(c(0,3,6,0,3,3,1,0, 4)) == 0

all_numbers <- rep(NA_integer_, 2020)
all_numbers[1:6] <- puzzle_input

calc_all_numbers <- function(input, to){
    all_numbers <- rep(NA_integer_, to)
    all_numbers[1:length(input)] <- as.integer(input)
    
    for(int in (length(input)):(to-1)){
        all_numbers[int+1] <-speak_number(all_numbers[1L:int])
    }
    all_numbers[to]
}
# calc_all_numbers(c(1,3,2),2020) == 1
# calc_all_numbers(c(2,1,3),2020) == 10
# calc_all_numbers(c(1,2,3),2020) == 27
# calc_all_numbers(c(2,3,1),2020) == 78
# calc_all_numbers(c(3,1,2),2020) == 1836

calc_all_numbers(c(0,14,6,20,1,4),2020)

#### part 2 ----
calc_all_numbers(c(1,3,2),30000000) == 18 ## the problem is the amount of time it takes.
# and the memory it consumes.


### the problem is keeping track of the enourmous vector while what we need
### is only the round number of latest time we've seen the number.
# keep track of unique numbers
# keep track of round
# keep track of round last seen
# 
# For every round: 
#  - latest is kept apart
#  - is latest in recent? 
#  - - yes = get diff between current round and recent. 
#     add latest to recent, overwriting the previous value with current round
#     set the diff as new latest
#     increment roundnumber
#  - - no = add latest to recent,
#    overwrite latest with 0
#    increment roundnumber
# 


library(R6)


Recitation <- R6Class(classname = "Recitation",
                      public= list(
                          initialize = function(vec){
                              self$round = length(vec)
                              for (idx in 1:(length(vec)-1)) {
                                  private$recent[[paste0('n',vec[idx])]] <- idx
                              }
                              self$unique_numbers <- unique(vec[1:(length(vec)-1)])
                              self$latest <- vec[length(vec)]
                              },
                          print = function(){
                              cat("current round: ", self$round,"\n")
                              cat("unique_numbers: ", length(self$unique_numbers),"\n" )
                              cat("last seen number:", self$latest, "\n")
                              cat("first 10:", self$unique_numbers[1:10], "\n")
                          },
                          unique_numbers = c(),
                          round = 0,
                          latest = NA_integer_,
                          progress_one_step = function(debug=FALSE){
                              # check latest against recent
                              last_spoken <- private$number_last_spoken(debug=debug)
                              
                              if(last_spoken ==0){
                                  res <- 0
                              }else{
                                  res <- self$round-last_spoken
                              }
                              if(debug){
                                  cat("latest number", self$latest, "\n")
                                  cat("[rnd",self$round+1, "] new number", res, "\n")
                              }
                              private$add_latest_to_recent(res)
                              private$increment()
                          }
                          #, # count total function
                          # next_round
                          #
                      ),
                      private = list(
                          recent = list(),
                          increment = function(){
                              self$round <- self$round +1
                          },
                          number_last_spoken = function(debug=FALSE){
                              if(self$latest %in% self$unique_numbers){
                                  if(debug){cat("number seen before\n")}
                                  private$recent[[paste0('n',self$latest)]]
                              }else{
                                  if(debug){cat("number NOT seen before\n")}
                                  0
                              }
                          },
                          add_latest_to_recent = function(new_latest){
                              latest <- self$latest
                              private$recent[[paste0('n',latest)]] <- self$round
                              self$unique_numbers <- unique(c(self$unique_numbers, latest))
                              self$latest <- new_latest
                          }
                      ) 
                      )


test2 <- Recitation$new(c(0,3,6))
test2
test2$progress_one_step(debug=TRUE) #==0  #speak_number(c(0,3,6))==0  # ronde 4
test2
test2$progress_one_step(debug=TRUE)# ==3 # speak_number(c(0,3,6,0)) == 3 ## ronde 5
test2
test2$progress_one_step(debug=T) # =3 round6 
test2$progress_one_step(debug=TRUE) # =1 r 7
test2$progress_one_step(debug=TRUE) # =0 r 8
test2$progress_one_step(debug=TRUE) # =4 r 9
test2$progress_one_step(debug=TRUE) # =0 r 10

run_sequence_for_n_steps <- function(seq, steps){
    elfs_game <- Recitation$new(seq)
    while (elfs_game$round < steps) {
        #if(elfs_game$round %in% round(steps * c(0.25, 0.5,0.75))){
            # cat("round:", elfs_game$round, "\n")
            # }
        elfs_game$progress_one_step()
    }
    cat(elfs_game$latest,"\n")
    elfs_game
}
test1 <- run_sequence_for_n_steps(c(2,1,3),2020) #== 10
run_sequence_for_n_steps(c(1,2,3),2020) # == 27
run_sequence_for_n_steps(c(3,1,2),2020) # 1836
run_sequence_for_n_steps(c(2,3,1),2020) # 78
## Timing while increasing with tenfold at the time.
## also comparing with first solution.
system.time(
    run_sequence_for_n_steps(c(1,3,2),3000) 
) # .21
system.time(
    calc_all_numbers(c(1,3,2),3000)
) # 0.05
system.time(
    run_sequence_for_n_steps(c(1,3,2),30000) 
) # 5.33
system.time(
    calc_all_numbers(c(1,3,2),30000)
) # 3.72
system.time(
    run_sequence_for_n_steps(c(1,3,2),300000) 
) # 454.58
system.time(
    calc_all_numbers(c(1,3,2),300000)
) # 351.63

# system.time(
#     run_sequence_for_n_steps(c(1,3,2),3000000) 
# )
# 
# system.time(
#     run_sequence_for_n_steps(c(1,3,2),30000000) # 2578
# )

### use a vector for direct lookup. 
# https://selbydavid.com/2020/12/06/advent-2020/#day15

# Treat the indices of a vector (-1, because R indexes from 1) as the possible
# spoken numbers, and the values at those indices as the last time that number
# was spoken, or zero if it has not been said so far.
# 
faster_r_version <- function(vec, steps){
    n= length(vec)
    # allocate an empty numeric vector
    spoken <- numeric(max(steps,vec)+1)
    # put the start seq inside spoken, using the actual numbers as indices
    spoken[vec[-n]+1] <- seq_len(n-1)
    current <- vec[n]
    ### elegant solution by David Selby, if bigger than 0, it becomes true == 1
    ### He is sidestepping an if else step here
    for (step in n:(steps-1)) {
        next_number <- (spoken[current +1] > 0) * step -spoken[current +1]
        spoken[current+1] <- step
        current <- next_number
    }
    current
}

system.time(
    faster_r_version(c(1,3,2),3000) 
) # 0.01
system.time(
    faster_r_version(c(1,3,2),30000) 
) # 0.,01
system.time(
    faster_r_version(c(1,3,2),300000) 
) #0.08
system.time(
    faster_r_version(c(1,3,2),3000000) 
)# 0.75

system.time(
    faster_r_version(c(1,3,2),30000000) 
)# 7.31
system.time(
    run_for_steps(c(1,3,2),30000000)
)# 1.2


faster_r_version(c(0,14,6,20,1,4),3e7) 


results <- microbenchmark::microbenchmark(
    rcpp_version = run_for_steps(c(1,3,4),3000),
    vector_v_1 = calc_all_numbers(c(1,3,4),3000),
    oop_version = run_sequence_for_n_steps(c(1,3,4),3000),
    vec_as_idx = faster_r_version(c(1,3,4),3000)
)
ggplot2::autoplot(results)

### This is maybe time for rcpp?
# keep looping and processing in cpp
# only return to R with result and maybe push some info back.

