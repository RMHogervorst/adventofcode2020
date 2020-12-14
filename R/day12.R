#--- Day 12: Rain Risk ---
# part 1 ----    
#### the ship has both a direction and position

testdata <- c('F10',"N3","F7","R90","F11")
startdirection <- "E"
startpos <- c(0,0)
endpos <- c(17,8)
distance <- 25


#### I can either make an object or let the functions
#### return a dataframe with direction, east, north
start <- list(
    direction= 90,
    east = 0,
    north=0
)

parse_actionline <- function(actionline){
    list(
    action = gsub("[0-9]","", actionline),
    amount = as.integer(gsub("[A-z]","",actionline))
    )
}
pos_move <- function(actionlist, pos_list){
    if(actionlist$action == "N"){
        pos_list$north <- pos_list$north + actionlist$amount
    }else if(actionlist$action == "S"){
        pos_list$north <- pos_list$north - actionlist$amount
    }else if(actionlist$action == "E"){
        pos_list$east <- pos_list$east + actionlist$amount
    }else if(actionlist$action == "W"){
        pos_list$east <- pos_list$east - actionlist$amount
    }
    pos_list
}

pos_turn <- function(actionlist, pos_list){
    if(actionlist$action == "R"){
        pos_list$direction = pos_list$direction + actionlist$amount
    }else if (actionlist$action == "L"){
        pos_list$direction = pos_list$direction - actionlist$amount
    }
    pos_list$direction = (pos_list$direction %% 360)
    pos_list
}

pos_forward <- function(actionlist, pos_list){
    if(actionlist$action == "F"){
    pos_list <- 
        switch (as.character(pos_list$direction),
                "0" =   pos_move(list(action="N",amount=actionlist$amount),pos_list),
                "90" =  pos_move(list(action="E",amount=actionlist$amount),pos_list),
                "180" = pos_move(list(action="S",amount=actionlist$amount),pos_list),
               "270" = pos_move(list(action="W",amount=actionlist$amount),pos_list)
        )
    }
    pos_list
} 

# returns a pos_list ----
# just apply all the actions after each other
move_ship <- function(actionline, pos_list){
    actionlist = parse_actionline(actionline)
    pos_list = pos_move(actionlist, pos_list)
    pos_list = pos_turn(actionlist, pos_list)
    pos_list = pos_forward(actionlist, pos_list)
    pos_list
}

distance_calc <- function(startposition, endposition){
    abs(endposition$east - startposition$east) +
        abs(endposition$north - startposition$north)
}

follow_instructions <- function(instructions, startposition, debug=FALSE){
    position <- startposition
    for (line in instructions) {
        position <- move_ship(line, position)
        if(debug){
            cat(line, "\n")
            cat("direction:", position$direction, 
                " E:",position$east, 
                ' N:',position$north, "\n")
            }
    }
    position
}
end <- follow_instructions(testdata, start, debug = FALSE)
distance_calc(start, end) == 25

### Now on the real data.
instructions <- readLines("data/day12.txt")

res <- follow_instructions(instructions, start)
distance_calc(start, res)

# ---- part 2 ----
# The waypoint starts 10 units east and 1 unit north
# waypoint is always relative
waypoint_start <- list(east=10, north=1)
start
# F10 means move 10 times: east 100 (10 * 10) north 10 (10*1)
# NSEW leave the ship but move the waypoint 
# N3 moves waypoint 3n to east10, noth4
# I can reuse most!

## returns only the ships position
pos_forward2 <- function(actionlist, pos_list, waypoint_position){
    if(actionlist$action == "F"){
        pos_list$east = pos_list$east + actionlist$amount * waypoint_position$east
        pos_list$north = pos_list$north + actionlist$amount * waypoint_position$north
    }
    pos_list
}
#pos_forward2(list(action="F",amount=10),start, waypoint_start)
#pos_move(list(action="N",amount=3), waypoint_start)

move_ship2 <- function(actionline, pos_list, waypoint){
    actionlist = parse_actionline(actionline)
    pos_list = pos_forward2(actionlist, pos_list,waypoint)
    pos_list
}

pos_turn2 <- function(actionlist, pos_list){
    if(actionlist$action == "L"){
        actionlist$action <- "R"
        actionlist$amount <- 360-actionlist$amount
    }
    ## This is ugly, but I think correct
    if(actionlist$action == "R"){
       if(actionlist$amount == 90){
           e <- pos_list$east
           pos_list$east <- pos_list$north
           pos_list$north <- -e
       }else if(actionlist$action == 180){
           pos_list$east * -1
           pos_list$north * -1
       }else if(actionlist$action == 270){
           e <- pos_list$east
           pos_list$east <- -pos_list$north
           pos_list$norh <- e
       }
    }
    pos_list
}

move_waypoint <- function(actionline, waypoint){
    actionlist = parse_actionline(actionline)
    waypoint = pos_move(actionlist, waypoint)
    waypoint = pos_turn2(actionlist, waypoint)
    waypoint
}

follow_instructions2 <- function(instructions, startposition,startwaypoint, debug=FALSE){
    position <- startposition
    waypoint <- startwaypoint
    for (line in instructions) {
        waypoint <- move_waypoint(line, waypoint)
        position <- move_ship2(line, position, waypoint)
        if(debug){
            cat(line, "\n")
            cat(" E:",position$east, 
                ' N:',position$north, "\n")
            cat("waypoint relative: E", waypoint$east, "N",waypoint$north,"\n")
        }
    }
    position
}
## debug
res <- follow_instructions2(testdata, start, waypoint_start, TRUE)
distance_calc(start, res)

res <- follow_instructions2(instructions, start, waypoint_start, TRUE)
distance_calc(start, res)