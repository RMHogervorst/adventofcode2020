#doesn't seem to work?
get_data <- function(endpoint, ext = '.csv'){
    url <- paste0("https://adventofcode.com/2020/day/", endpoint)
    loc <- paste0("data/", endpoint)
    message(paste0("downloading file into ", loc))
    download.file(url=url, destfile = paste0("loc",ext))
}
