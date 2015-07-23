## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function that;
## A. sets the value of the matrix
## B. gets the value of the matrix
## C. sets the inverse of the matrix
## D. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x

    ## sets & gets inverse
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# This function returns the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## if the inverse has already been computed then skip and return cache data
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## compute the inverse 
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
