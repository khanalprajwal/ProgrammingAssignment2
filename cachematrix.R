## Creates cache object for the inverse of a invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  cacheInvMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheInvMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cacheInvMatrix <<- inverse
  getInverse <- function() cacheInvMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Returns the inverse of a given invertible matrix 

cacheSolve <- function(x, ...) {
  
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("<< cached data >>")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
