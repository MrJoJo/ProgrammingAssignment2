## makeCacheMatrix will accept a matrix
## and creates object methods for cacheSolve() to get & set the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL           
     
     ##Define 1st object method: call as object$set(); allows you to
     ##reset the original passed matrix in the object 
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     ##Define 2nd object method: call as object$get(); returns the original matrix
     get <- function() x
     
     ##Define 3rd method: call as object$setinverse(); allows you to set 
     ##the matrix in the object to the inverse of the matrix
     setinverse <- function(matx) m <<- matx
     
     #Define 4th method: call as object$getinverse(); returns the inverse matrix
     getinverse <- function() m
     
     #returns list
     list(set = set, get = get,
          setinverse = setinverse, 
          getinverse = getinverse)
}



## cacheSolve will solve & return the inverse for the passed matrix
## OR return the inverse matrix from cache
## We assume passed matrix is square and invertible

cacheSolve <- function(x, ...) {
     
     ##uses the 4th object method to get the inverse matrix 
     ##will be null initially. 'x' is place hold for the object name
     m <- x$getinverse() 
     
     #checks to see matrix is initially null.
     if(!is.null(m)) {   
          message("getting cached data")
          return(m)      #if matrix not NULL, then it's the inverse matrix, so return it!
     }

     #uses the 2nd object method to get the original passed matrix
     data <- x$get() 
     m <- solve(data, ...) #uses the solve() function to get the inverse
     
     #uses the 3rd objectd method to set m to the new inverse matrix.
     x$setinverse(m) 
     m


}
