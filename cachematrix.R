## Assignment: Caching the Inverse of a Matrix

## create matrix and cache it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes the inverse of makeCacheMatrix 
## If inverse exists already then retrieve from cache

cacheSolve <- function(x, ...) {
  ##  Check to see if inverse already exists   
  m <- x$getInverse()
  ## Yes - get inverse from cache & skip comp    
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## No - compute inverse      
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
