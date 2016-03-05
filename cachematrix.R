## Put comments here that give an overall description of what your
## functions do

##################################################
##          Author: GZ
##################################################

## makeCacheMatrix: create a matrix, which supports get, set,
##    setInverse, getInverse
## cacheSolve: "calculate" inverse of the input by recalling cache
##    or calculate it on the fly

## Write a short comment describing this function
## This function contains 4 sub-components
## --- set()
## --- get()
## --- setInverse()
## --- getInverse()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data: matrix inverse")
    return (i)
  }
  data <- x$get()
  # matrix inverse here
  i <- solve(data)
  x$setInverse(i)
  i
}
