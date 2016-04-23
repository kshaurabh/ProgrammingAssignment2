
## Function “makeCacheMatrix” is defined to cache the inverse of a matrix
## makeCacheMatrix is called to create a matrix; PLZ create only square matrix as solve works on square matrix only
## the matrix created will be used in cacheSolve function
## ret_vec is to return the vector x stored in memory.
## chng_vec is to store the changed vector 
## we use `<<-` to assign a value to an object in an environment different from the current environment. 

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


## x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()
## if the inverse has already been calculated get it from the cache and skips the computation
## otherwise, calculates the inverse sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
     a <- x$inverse_2()
      if(!is.null(a)) {
            message("getting cached data")
            return(a)
      }
      data <- x$ret_vec()
      a <- solve(data, ...)
      x$inverse_1(a)
      return(a)
}    
