## MakeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.

## If the matrix inverse has already been calculated, it will instead find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
get <- function() x
setinverse <- function(solve) m <<- solve
getinverse <- function() m
list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## If the cached inverse is available, cacheSolve retrieves it, while if not, it computes, caches, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }else{
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
