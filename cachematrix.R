## Functions that create an object to store a matrix
## and cache the inverse of that matrix

## Creates matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## compute inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# test functions
# result should look like:
#      [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# test <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# test
# matrix_object <- makeCacheMatrix(test)
# cacheSolve(matrix_object)
