## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	a <- NULL
      chng_vec <- function(y) {
            x <<- y
            a <<- NULL
      }
      ret_vec <- function() x
      inverse_1 <- function(solve) a <<- solve
      inverse_2 <- function() a
      list(chng_vec = chng_vec, ret_vec = ret_vec,
           inverse_1 = inverse_1,
           inverse_2 = inverse_2)
}


## Function “makeCacheMatrix” is defined to cache the inverse of a matrix
## ret_vec is to return the vector x stored in memory.
## chng_vec is to store the changed vector

cacheSolve <- function(x, ...) {
     a <- x$inverse_2()
      if(!is.null(a)) {
            message("getting cached data")
            return(a)
      }
      data <- x$ret_vec()
      a <- solve(data, ...)
      x$inverse_1(a)
      
