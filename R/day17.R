#day 17 --- Day 17: Conway Cubes --- 
#
#### shiiiit a 3d version of Game of life!
#
#Each cube only ever considers its neighbors: any of the 26 other cubes where
#any of their coordinates differ by at most 1. For example, given the cube at
#x=1,y=2,z=3, its neighbors include the cube at x=2,y=2,z=2, the cube at
#x=0,y=2,z=3, and so on. During a cycle, all cubes simultaneously change their
#state according to the following rules:
#
#If a cube is active and exactly 2 or 3 of its neighbors are also active, the
#cube remains active. Otherwise, the cube becomes inactive. If a cube is
#inactive but exactly 3 of its neighbors are active, the cube becomes active.
#Otherwise, the cube remains inactive.
# 
### active is #, inactive is nothing or .
### 
## So the input is at z0 and the other dimensions are thus x and y

example_t0 <- # x
".#.
..#
###"

example_t1zm1 <-
"#..
..#
.#."

example_t1z0 <-
    ".#
.##
.#."

example_t1z1 <- 
    '#..
..#
.#.'


# 3,3,1 -> 3,3,3
# -> 5,5,5 -> 7,7,5


# what If i only kept the active ones in a dataframe?
# cube, x, y , z
active_cubes_test <- data.frame(
    x=c(2,3,1,2,3),
    y=c(1,2,3,3,3),
    z=c(0,0,0,0,0)
)

# retrieve_neighbors
neighborhood <- function(x,y,z){
    steps <- c(-1,0,1)
    neighbors <- expand.grid(x=steps, y=steps, z=steps)
    neighbors <- neighbors[!(neighbors$x==0 & neighbors$y==0 & neighbors$z==0),]
    neighbors$x <- neighbors$x + x
    neighbors$y <- neighbors$y + y
    neighbors$z <- neighbors$z + z
    rownames(neighbors) <- NULL
    stopifnot(nrow(neighbors)==26)
    neighbors
}

# neighborhood(x=1,y=2,z=3) # includes 2,2,2 &0,2,3

print_cubes <- function(df){
    x_ = range(df$x)
    y_ = range(df$y)
    z_ = range(df$z)
    for(z in seq(z_[1], z_[2])){
        df1 <-subset(df, df$z==z)
        cat("z=",z,"\n")
        for( y in seq(y_[1],y_[2])){
            for (x in seq(x_[1],x_[2])) {
                token <- "."
                res <- sum(df1$x == x & df1$y == y & df1$z == z)
                if(res==1){token <- "#"}
                cat(token)
            }
            cat("\n")
        }
    }
}
#print_cubes(active_cubes_test)

library(dplyr)


## No there is a more elegant way.
places_to_check <- function(cubes){
    x_ = range(cubes$x)
    y_ = range(cubes$y)
    z_ = range(cubes$z)
    expand.grid(
        x = seq(x_[1]-1, x_[2]+1),
        y = seq(y_[1]-1, y_[2]+1),
        z = seq(z_[1]-1, z_[2]+1)
    )
}
count_actives_neighborhood <- function(x,y,z, df_values){
    nb <-neighborhood(x,y,z)
    nb %>% 
        left_join(df_values, by = c("x", "y", "z")) %>% 
        summarize(actives = sum(active, na.rm = TRUE)) %>% 
        pull(actives)
}
count_actives_neighborhood(previous_step$x[[1]], previous_step$y[[1]], previous_step$z[[1]], actives) ==1
count_actives_neighborhood(1,1,-1, actives)
neighborhood(previous_step$x[[1]], previous_step$y[[1]], previous_step$z[[1]])


do_step <- function(previous_step){
    cat('steppin\n')
    coordinates <- places_to_check(previous_step)
    actives <- previous_step %>% mutate(active=1)
    coordinates$nbs <- purrr::pmap_dbl(
        list(x=coordinates$x,y=coordinates$y, z=coordinates$z), 
        count_actives_neighborhood, df_values=actives)
    coordinates <- coordinates %>% left_join(actives, by=c("x","y","z"))
    # apply the rules of the cycle
    coordinates$next_round <- 0
    active_cds <- !is.na(coordinates$active)
    coordinates$next_round[active_cds] <- as.integer(coordinates$nbs[active_cds] %in% c(2,3))
    coordinates$next_round[!active_cds] <- as.integer(coordinates$nbs[!active_cds] ==3)
    # return only the active cases
    coordinates[coordinates$next_round ==1,c("x","y","z")]
}
test_step1 <- tribble(
    ~x,~y,~z,
    1,1,-1,
    3,2,-1,
    2,3,-1,
    1,1,0,
    3,1,0,
    2,2,0,
    3,2,0,
    2,3,0,
    1,1,1,
    3,2,1,
    2,3,1
)
test_step1 %>% print_cubes()
tmp1 <- active_cubes_test %>% do_step()
# it works! but, it shifts!

tmp1 %>% do_step() %>% print_cubes()

active_cubes_test %>% 
    do_step() %>% # 1
    do_step() %>% # 2
    do_step() %>% # 3
    print_cubes()

active_cubes_test %>% 
    do_step() %>% # 1
    do_step() %>% # 2
    do_step() %>% # 3
    do_step() %>% # 4
    do_step() %>% #5
    do_step() %>% #6
    nrow() == 112


# 
#| .##.####  x=2,3,5,6,7,8 y=1,z=0
#| .#.....#
#| #.###.##
#| #####.##
#...##.#
#| #######.
#| ##.#####
#| .##...#.

values <- which(
    matrix(
        unlist(strsplit(".##.####.#.....##.###.#######.###...##.########.##.#####.##...#.", "")),
        ncol=8,
        nrow=8, 
        byrow=TRUE) =="#",arr.ind=TRUE)



# put the data into process
values %>% 
    as.data.frame() %>% 
    rename(x=col,y=row) %>% 
    mutate(z=0) %>% # print_cubes() # ### check if correct representation
    do_step() %>% # 1
    do_step() %>% # 2
    do_step() %>% # 3
    do_step() %>% # 4
    do_step() %>% #5
    do_step() %>% #6
    nrow()
    