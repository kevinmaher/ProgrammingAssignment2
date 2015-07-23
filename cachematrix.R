
## Here  are 2 functions that cache and then compute the inverse of a matrix

## This function creates a special matrix objectthat can cache its inverse.

makeCacheMatrix <- function(matx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    matx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(matx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## matrix returned by `makeCacheMatrix` above. 



cacheSolve <- function(matx, ...) {
  inverse <- matx$getinv()
  ##If the inverse has already been calculated, then
  ## cacheSolve function should retrieve the inverse from the cache.
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  ## else calculate the inverse 
  data <- matx$get()
  invserse <- solve(data, ...)
  matx$setinv(inverse)
  return(inverse)
}