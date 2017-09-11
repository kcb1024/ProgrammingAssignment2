## Solves for the inverse of a matrix and caches
## the result. If cacheSolve is called to solve
## for the inverse of a matrix that has already
## been solved, it pulls the cached inverse from
## memory instead of re-computing it.

## Create a cache-able matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inv) m <<- inv
  
  getInv <- function() m
  
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns the inverse of a matrix, either by
## calculing the inverse (and then caching) or
## by returning the cached inverse if it's been
## calculated before.
## Note: can only be called on results of makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m		
}
