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
