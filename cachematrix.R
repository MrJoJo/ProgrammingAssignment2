## This creates functions similar to makeVector and cachemean.
## makeCacheMatrix will be passed a matrix, and make methods any makeCacheMatrix objects. 
## 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL #solve to NULL
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



## Write a short comment describing this function

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
