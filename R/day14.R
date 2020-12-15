# ---- day 14 Docking Data ----
# part 1 ----
# a line like mem[8] = 11 would write the value 11 to memory address 8.
#
# The bitmask is always given as a string of 36 bits, written with the most
# significant bit (representing 2^35) on the left and the least significant bit
# (2^0, that is, the 1s bit) on the right. The current bitmask is applied to
# values immediately before they are written to memory: a 0 or 1 overwrites the
# corresponding bit in the value, while an X leaves the bit in the value
# unchanged.
#
# intToBits() should work perfectly, but does only 32 bits
# and I need a 36 bit with the least significant bit right.
# I also need to apply a mask to the values
# I think representing values as vector of length 64 is easiest.
# I might make an S3 printing method too!
### https://stackoverflow.com/a/26346350 
### and https://www.rapidtables.com/convert/number/decimal-to-binary.html
library(magrittr)
to_binary_vec <- function(int){
    vec <- rep(0L, 36)
    i <- 0
    while (int >0) {
        vec[(36-i)] <- as.integer(int %% 2)
        int <- int %/% 2
        i <- i +1
    }
    class(vec) <- "bitrepresentation"
    vec
}

binary_vec_to_integer <- function(vec){
    stopifnot(length(vec)==36)
    sum(vec * 2^(35:0))
}

print.bitrepresentation <- function(x){
   cat("(",binary_vec_to_integer(x),") ", as.integer(x))
}
apply_bitmask <- function(vec, mask){
    stopifnot(length(vec)==36)
    stopifnot(length(mask)==36)
    vec[mask ==1] <- 1L
    vec[mask ==0] <- 0L
    vec
}
make_vec_from_string <- function(line){unlist(stringr::str_split(line,""))}
# 
# to_binary_vec(11)
# to_binary_vec(101)
# mask1 <- unlist(strsplit('XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X',""))
# apply_bitmask(to_binary_vec(11), mask1) # is indeed 73
# apply_bitmask(to_binary_vec(101), mask1) # is indeed 101
# apply_bitmask(to_binary_vec(0), mask1) # is inded 64

### parse the inputdata
ferry_init_program <- readLines("data/day14.txt")
masks <- stringr::str_detect(ferry_init_program,"^mask")
#all_mems <- stringr::str_extract_all(ferry_init_program, "mem\\[[0-9]{1,5}\\]") %>% stringr::str_extract("[0-9]{1,5}")
all_memory_adresses <- list()

extract_mask <- function(line){
    stringr::str_remove(line,"mask = ") %>% 
        make_vec_from_string()
}
extract_number <- function(line){
    as.integer(stringr::str_extract(line,"[0-9]+$"))
}

extract_mem_adress <- function(line){
    stringr::str_extract(line, "mem\\[[0-9]{1,5}\\]") %>% 
        stringr::str_extract('[0-9]{1,5}')
        
}
set_to_memory <- function(masknumber, line_seq){
    mask <- extract_mask(ferry_init_program[masknumber])
    for (line in line_seq) {
        mem_adress <- paste0('m',extract_mem_adress(ferry_init_program[line]))
        bin_vec = to_binary_vec(extract_number(ferry_init_program[line]))
        bin_vecmasked <- apply_bitmask(bin_vec, mask)
        ## and assign it to the memory adress.
        all_memory_adresses[[mem_adress]] <<- bin_vecmasked
    }
}
start <- (1:length(ferry_init_program))[masks]
end <- c(start -1, length(ferry_init_program))
end <- end[end !=0]
for (step in seq_len(length(start))) {
    set_to_memory(start[step], ((start[step])+1):end[step])
}
options(digits=20)
sum(purrr::map_dbl(all_memory_adresses, binary_vec_to_integer))

### part 2 -----
# Of course there is always a catch.
# 
all_memory_adresses2 <- list()

apply_bitmask2 <- function(vec, mask){
    stopifnot(length(vec)==36)
    stopifnot(length(mask)==36)
    vec[mask ==1] <- 1L
    vecs <- matrix(vec, nrow = 1, byrow = TRUE)
    ### the dumb way, growing a matrix
    for (x in which(mask == 'X')) {
        vecs_row = NROW(vecs)
        vecs <- rbind(vecs, vecs)
        vecs[,x]<- 0
        vecs[1:vecs_row, x] <- 0 
        
    }
    vecs
}
apply_bitmask2(
    vec = to_binary_vec(42), 
    mask = make_vec_from_string("000000000000000000000000000000X1001X"))

creator <- function(nrow, nr){
    stopifnot(nrow %% 2 ==0)
    stopifnot(nrow/2 >= nr)
    x <- nrow / (2^nr)
    y <- nrow / (2*x)
    rep(c(rep(1, x), rep(0,x)),y)
}

## never growing the matrix
apply_bitmask3 <- function(vec, mask){
    stopifnot(length(vec)==36)
    stopifnot(length(mask)==36)
    vec[mask =="1"] <- 1L
    mask_positions <- which(mask == 'X')
    ### first make the matrix
    vecs <- matrix(vec, ncol=36, byrow = TRUE, nrow = 2^length(mask_positions))
    ## then modify in place
    for (x in mask_positions) {
        vecs[,x] <- creator(NROW(vecs), nr = which(mask_positions == x))
    }
    vecs
}


set_to_memory_v2 <- function(masknumber, line_seq){
    mask <- extract_mask(ferry_init_program[masknumber])
    for (line in line_seq) {
        mem_adress <- to_binary_vec(as.integer(extract_mem_adress(ferry_init_program[line])))
        tmp <- apply_bitmask3(bin_vec, mask)
        # turn matrix in vectors (every row) and make them integer again.
        adresses <- apply(tmp, 1, binary_vec_to_integer)
        #bin_vec = to_binary_vec(extract_number(ferry_init_program[line]))
        number <- extract_number(ferry_init_program[line])
        ## and assign it to the memory adressses.
        for (adress in adresses) {
            all_memory_adresses2[[paste0("m",adress)]] <<- number
        }
    }
}


start <- (1:length(ferry_init_program))[masks]
end <- c(start -1, length(ferry_init_program))
end <- end[end !=0]
for (step in seq_len(length(start))) {
    set_to_memory_v2(start[step], ((start[step])+1):end[step])
}
sum(purrr::map_dbl(all_memory_adresses2, 1)) # 1435625767696 too low
