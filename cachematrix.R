#' these two functions speed up the inverse matrix calculations for large 
#' matrices e.g. when they are repeated in loops. They store the calculated 
#' inversions in cache and retrieve them if called repeatedly



#' the first function creates a list of functions to set and get the matrix and
#' to set and get the inverse matrix. It sets the inverse to NULL for any new 
#' input matrix. 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


#' The following function calculates the inverse matrix from the list provided 
#' by 'makeCacheMatrix'. However, it first checks to see if the inverse has 
#' already been calculated. If so, it gets the inverse from the cache and skips 
#' the computation. Otherwise, it calculates the inverse using solve and sets 
#' the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached matrix inverse")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      ## Return a matrix that is the inverse of 'x'
      inv
}
