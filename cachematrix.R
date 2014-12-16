## makeCacheMatrix will accept a matrix
## and creates object methods similar to makeVector.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL 
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}


## cacheSolve will solve & return solution for the matrix
## OR return a solution from cache
## We assume passed matrix is square and invertible

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }

     data <- x$get()
     m <- solve(data, ...)
     x$setmean(m)
     m


}
