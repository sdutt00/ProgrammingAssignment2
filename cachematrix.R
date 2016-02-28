## makeCacheMatrix sets a given matrix and its inverse from cache

## The function will allow setting a matrix and its inverse in cache
## It will also retrieve the matrices from cache

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL;
  set <- function(y) {
    x <<- y;
    inv_x <<- NULL;
  }
  get <- function() x
  getinv <- function() inv_x
  setinv <- function(ix) inv_x <<- ix
  list (set = set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve will look for a cached inverse of a matrix
## and retrieve it if available. Otherwise it will calculate
## and set the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached inverse matrix...")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data)
  x$setinv(inv_x)
  inv_x
}
