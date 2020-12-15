### --- Day 13: Shuttle Search ---
### increment for every bus from timestamp 0
### till you are larger than starttime.
starttime <- 1007268
busses <- "17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,937,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,397,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,19"
busses <- unlist(strsplit(busses,","))
valid_busses <- as.numeric(busses[busses != "x"])
timetable <-
    data.frame(
    bus = valid_busses,
    earliest_starttime = ceiling(starttime / valid_busses) * valid_busses
)
timetable$waittime <- timetable$earliest_starttime - starttime
timetable[order(timetable$earliest_start),][1,]

## -- part 2 ---- 
# find timepoint where 17 starts, and 7 minutes later 41, etc.
set_delays <- function(vec){
    bus_delays <- data.frame(bus = vec)
    for (index in seq_len(length(vec))) {
        bus_delays$delay[index] <- index-1
    }
    bus_delays[bus_delays$bus != "x",]
}
example1 <- set_delays(c("17","x","13","19"))

#17 * x = 13 * z + 2
#17x = 13z +2
#17x/13z = 2
# 17x = 13y +2 = 19z +3 
## I don't know how to solve this elegantly.
## maybe gradient descent?
# bus_result <- function(df, weights){
#     # put in weights, weights must be integer
#     # cannot be 0.  
#     function(weights){
#         sd(as.numeric(df$bus) * weights +df$delay)        
#     }
# }
# bus_example1 <- bus_result(set_delays(c("17","x","13","19")))
# optim(c(1,1,1), bus_example1)
### guess we need to do integer / linear programming

#alright, just go for dump loops. 
find_number <- function(df, debug=FALSE){
    df$bus <- as.numeric(df$bus)
    done <- FALSE
    bus_length <- length(df$bus)
    increment <- df$bus[1]
    ### assumption, we need at least max(bus) * secondmax(bus)
    amount <- prod(sort(df$bus,decreasing = TRUE)[1:2])
    while (!done) {
        bus_match <- rep(FALSE, (bus_length))
        if(debug){cat(amount,"\n")}
        for (step in 1:bussers) {
            comparevalue <- amount + df$delay[step]
            if(comparevalue %% df$bus[step] !=0){ 
                break
            }else{
                bus_match[step] <- TRUE     
                }
        }
        if(sum(bus_match) == bus_length){
            done <- TRUE
            cat(amount)
        }else{
            amount <- amount+increment
            if(amount %% 50 == 0){cat("\n")}
            if(amount %% 2 ==0){cat('.')} # just so I have an indicator
        }
        
    }
    amount
}
find_number(example1, debug = FALSE)

timestamp <- function(vec, debug=FALSE){
    find_number(set_delays(vec),debug = debug)
}

### testdata
timestamp(c("17","x","13","19")) == 3417
timestamp(c("67","7","59","61"), debug = FALSE) == 754018
#67,x,7,59,61 first occurs at timestamp 779210.
#67,7,x,59,61 first occurs at timestamp 1261476.
#1789,37,47,1889 first occurs at timestamp 1202161486.
#