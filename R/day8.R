# --- Day 8: Handheld Halting ---
#
#
# acc increases or decreases a single global value called the accumulator by the
# value given in the argument. For example, acc +7 would increase the
# accumulator by 7. The accumulator starts at 0. After an acc instruction, the
# instruction immediately below it is executed next.
#
# jmp jumps to a new
# instruction relative to itself. The next instruction to execute is found using
# the argument as an offset from the jmp instruction; for example, jmp +2 would
# skip the next instruction, jmp +1 would continue to the instruction
# immediately below it, and jmp -20 would cause the instruction 20 lines above
# to be executed next.
#
# nop stands for No OPeration - it does nothing. The
# instruction immediately below it is executed next.


#### part 1 ----
# Run your copy of the boot code. Immediately before any instruction is executed
# a second time, what value is in the accumulator?
instructions <- readLines("data/day8.txt")


### Attempt 1, functional perspective, while loops. ----
# we start at line 1 and acc=0
# increment_accumulator <- function(amount, acc) {
#   acc <- acc + amount
# }
# parse_instructions <- function(text, acc) {
#   cmd <- stringr::str_extract(text, "^nop|^jmp|^acc")
#   move <- switch(cmd,
#     "jmp" = stringr::str_remove(stringr::str_remove(text, "jmp "), "\\+"),
#     "nop" = "1",
#     "acc" = "1"
#   )
#   if (cmd == "acc") {
#     acc <- increment_accumulator(
#       as.numeric(
#         stringr::str_remove(
#           stringr::str_remove(text, "acc"),
#           "\\+"
#         )
#       ),
#       acc
#     )
#   }
#   as.numeric(move)
# }
# 
# 
# ## Returns a linenumber
# run_instructions <- function(linenumber) {
#   parse_instructions(instructions[linenumber])
# }
# 
# #### make sure the instructions are only read once.
# run_program_once <- function(debug = FALSE) {
#   acc <- 0
#   position <- 1
#   all_linenumbers <- data.frame(
#     ln_nbr = seq_len(length(instructions)),
#     executed = FALSE
#   )
#   while (sum(all_linenumbers$executed != length(instructions))) {
#     if (debug) {
#       print(paste0("Position: ", position, " acc=", acc))
#     }
#     if (all_linenumbers$executed[all_linenumbers$ln_nbr == position]) {
#       message(paste0("Position ", position, " already done, stopping"))
#       break
#     }
#     # mark_position
#     all_linenumbers$executed[all_linenumbers$ln_nbr == position] <- TRUE
#     # move to new position
#     position <- position + run_instructions(position)
#   }
#   acc
# }
# 
# run_program_once(TRUE)
# 
# 

### Attempt 2: R6 oop style ----- 
### This is an excellent case for OOP
library(R6)

### Create  a class that checks the moves and 
### 
Handheld <- R6Class("Handheld",
  public = list(
    initialize = function(acc = 0, pos = 1) {
      private$acc <- acc
      private$pos <- 1
      private$set_position(1)
      
    },
    print = function(...) {
      cat("Handheld: \n")
      cat("  acc: ", private$acc, "\n", sep = "")
      cat("  pos: ", private$pos, "\n", sep = "")
    },
    add = function(x = 0) {
      private$acc <- private$acc + x
    },
    move = function(x=0){
        position <- private$pos + x
        private$valid_position(position)
        private$check_position(position)
        private$set_position(position)
    },
    print_done = function(...){
        cat("Handheld: \n")
        cat("total:",nrow(private$all_linenumbers), "\n")
        cat("done: ", sum(private$all_linenumbers$executed), "\n")
        cat(" >.", private$all_linenumbers$ln_nbr[private$all_linenumbers$executed], ".< \n")
    },
    done = function(...){
        # done when either one number is already seen, or all numbers are done.
        if(private$donecheck){
            TRUE
            }else if(
                sum(private$all_linenumbers$executed) == nrow(private$all_linenumbers)
                ){
                TRUE
                }else{
            FALSE
        }
        
    }
  ),
  private = list(
    acc = NA,
    pos = NA,
    donecheck = FALSE,
    all_linenumbers = data.frame(
        ln_nbr = seq_len(length(instructions)),
        executed = FALSE
    ),
    check_position = function(position=0){
        if(private$all_linenumbers$executed[private$all_linenumbers$ln_nbr ==position]){
            message("Position already done")
            private$donecheck <- TRUE
        }
    },
    valid_position = function(position=0){
        mx_ <- max(private$all_linenumbers$ln_nbr)
        if(position == 0 | position > mx_){
            stop(paste0("Invalid position (1-",mx_, ")"),call. = FALSE)
        }
    },
    set_position = function(position=0){
        private$all_linenumbers$executed[private$all_linenumbers$ln_nbr == position] <- TRUE
        private$pos <- position
        }
  ),
  active = list(
      current_position = function(){
          private$pos
      }
  )
)
# test cases
# game1 <- Handheld$new()
# game1$print_done()
# game1$add(1)
# game1$move(1000)
# game1
# game1$move(1)
# game1
# game1$print_done()
# game1$current_position
# game1$done()


add_if_acc <- function(textline){
    if(stringr::str_detect(textline,"^acc")){
        as.numeric(stringr::str_extract(textline,"[0-9-]+"))
    }else{
        0
    }
}

direction <- function(textline){
    if(stringr::str_detect(textline,"^jmp")){
        as.numeric(stringr::str_extract(textline,"[0-9-]+"))
    }else{
        1
    }
}

run_program <- function(debug=FALSE){
    handheldgame <- Handheld$new()
    
    while (!handheldgame$done()) {
        current_pos <- handheldgame$current_position
        line <- instructions[current_pos]
        handheldgame$add(add_if_acc(line))
        handheldgame$move(direction(line))
        if(debug){
            handheldgame$print_done()
        }
    }
    handheldgame
}

res <- run_program(debug = FALSE)

#### PART 2  ---- 
# The program is supposed to terminate by attempting to execute
# an instruction immediately after the last instruction in the file.
#
#### it should now terminate when instruction is one more than last.
#### so nrow(+1)

# Fix the program so that it terminates normally by changing exactly one jmp (to
# nop) or nop (to jmp). What is the value of the accumulator after the program
# terminates?
## Add terminate instruction to the class
## replace one by one the jmps by nops and nop to jmp


Fixed_Handheld <- R6Class("Handheld",
                          public = list(
                              initialize = function(acc = 0, pos = 1) {
                                  private$acc <- acc
                                  private$pos <- 1
                                  private$set_position(1)
                                  
                              },
                              print = function(...) {
                                  cat("Handheld: \n")
                                  cat("  acc: ", private$acc, "\n", sep = "")
                                  cat("  pos: ", private$pos, "\n", sep = "")
                              },
                              add = function(x = 0) {
                                  private$acc <- private$acc + x
                              },
                              move = function(x=0){
                                  position <- private$pos + x
                                  private$valid_position(position)
                                  private$check_position(position)
                                  private$set_position(position)
                              },
                              print_done = function(...){
                                  cat("Handheld: \n")
                                  cat("total:",nrow(private$all_linenumbers), "\n")
                                  cat("done: ", sum(private$all_linenumbers$executed), "\n")
                                  cat(" >.", private$all_linenumbers$ln_nbr[private$all_linenumbers$executed], ".< \n")
                              },
                              done = function(...){
                                  # done when either one number is already seen, or all numbers are done.
                                  if(private$donecheck){
                                      TRUE
                                  }else if(
                                      sum(private$all_linenumbers$executed) == nrow(private$all_linenumbers)
                                  ){
                                      TRUE
                                  }else{
                                      FALSE
                                  }
                                  
                              },
                              score = function(...){
                                  private$acc
                              }
                          ),
                          private = list(
                              acc = NA,
                              pos = NA,
                              donecheck = FALSE,
                              terminus = FALSE,
                              all_linenumbers = data.frame(
                                  ln_nbr = seq_len(length(instructions)),
                                  executed = FALSE
                              ),
                              check_position = function(position=0){
                                  if(position < max(private$all_linenumbers$ln_nbr)){
                                      if(private$all_linenumbers$executed[private$all_linenumbers$ln_nbr ==position]){
                                          message("Position already done")
                                          private$donecheck <- TRUE
                                      }
                                  }
                                  
                              },
                              valid_position = function(position=0){
                                  mx_ <- max(private$all_linenumbers$ln_nbr)
                                  if(position == mx_+1){
                                      message("Program terminated")
                                      private$terminus <- TRUE
                                  }else if(position == 0 | position > mx_+1){
                                      stop(paste0("Invalid position (1-",mx_, ")"),call. = FALSE)
                                  }
                              },
                              set_position = function(position=0){
                                  private$all_linenumbers$executed[private$all_linenumbers$ln_nbr == position] <- TRUE
                                  if(!private$terminus){
                                      private$pos <- position   
                                  }
                              }
                          ),
                          active = list(
                              current_position = function(){
                                  private$pos
                              },
                              terminated = function(){
                                  private$terminus
                              }
                    
                          )
)

tmp1 <- Fixed_Handheld$new()
tmp1$done()

## A program that also takes into account the instructionset
run_program2 <- function(debug=FALSE, instructions = instructions){
    handheldgame <- Fixed_Handheld$new()
    
    while (!handheldgame$done()) {
        if(handheldgame$terminated){break}
        current_pos <- handheldgame$current_position
        line <- instructions[current_pos]
        handheldgame$add(add_if_acc(line))
        handheldgame$move(direction(line))
        if(debug){
            handheldgame$print_done()
        }
    }
    handheldgame
}

# Changing the instructions
## replace one by one the jmps by nops and nop to jmp
instructions2 <- instructions

jumps <- stringr::str_detect(instructions,"^jmp")
ch_jump <- seq_len(length(instructions))[jumps]


change_instruction <- function(pos, from, to){
    res<- instructions
    res[pos]<- stringr::str_replace(res[pos], from, to)
    res
}

run_program2_purrr <- function(jmp, from, to, debug=FALSE){
    result <- run_program2(debug = debug, instructions = change_instruction(jmp, from, to))
    data.frame(
        terminated = result$terminated,
        acc = result$score(),
        from=from,
        to=to
    )
}

jumpers <- purrr::map_dfr(ch_jump, run_program2_purrr, from="jmp", to="nop")
jumpers[jumpers$terminated,]
## never had to do the other one