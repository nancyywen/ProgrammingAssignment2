## Implements caching for matrix inversion by using a special
## matrix object

## Testing out the functions:
## > source("cachematrix.R")
## > m <- makeCacheMatrix()
## > m$get() 
## > m$set(matrix(1:4, 2, 2))
## > m$get()
## > cacheSolve(m)
## > cacheSolve(m)


## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix above
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){  ## if inverse is found in cache
    message("getting cached data")
    return(i)       ## return cached inverse, skips computation
  }
  data <- x$get()
  i <- solve(data, ...) ## computes the inverse
  x$setinverse(i)       ## sets the inverse in cache
  i
}
