## This file contains functions:
##    1) to create a matrix object that can cache it's inverse, and
##    2) computes the inverse of the matrix returned by the first function
##

## create matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL
  set <- function(y){
    x <<- y
    minv <<- NULL
  } 
  
  get <- function() x
  setinverse <- function(inverse) minv <<- inverse
  getinverse <- function() minv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## computes the inverse of matrix using cached value if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinverse(minv)
  minv
  
}
