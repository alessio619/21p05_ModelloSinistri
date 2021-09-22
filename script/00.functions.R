
# This counts from the left and then extract n characters

str.left <- function(string, n) {
  substr(string, 1, n)
}



# This counts from the right and then extract n characters

str.right <- function(string, n) {
  substr(string, nchar(string) - (n - 1), nchar(string))
}


# This extract characters from the middle

str.mid <- function(string, from = 2, to = 5){
  
  substr(string, from, to)
}


# keep columns with pattern and other 

keep.cols <- function(DT, pattern, ...) {
    
    keeped <- unlist(c(list(...), grep(as.character(pattern), names(DT), value = TRUE)), recursive = TRUE, use.names = TRUE)
    keeped
}
