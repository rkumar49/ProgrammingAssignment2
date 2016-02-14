# ProgrammingAssignment2
## Make a cache Matrix and compute inverse matrix 


## Takes a matrix and sets inverse to NULL and
## provides functions set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## If Inverse is not yet set, it computes the inverse matrix, sets and returns it
## Uses functions from list in makeCacheMatrix 

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
