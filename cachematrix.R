##  The functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##Testing

# Sample run:
x = rbind(c(6, 2), c(2, 4))
inv = makeCacheMatrix(x)
inv$get()

#        [,1] [,2]
# [1,]    6    2
# [2,]    2    4


# No cache
cacheinverse(inv)

#       [,1] [,2]
# [1,]  0.2 -0.1
# [2,] -0.1  0.3

# the second run
# getting cached data.

cacheinverse(inv)

#       [,1] [,2]
# [1,]  0.2 -0.1
# [2,] -0.1  0.3
