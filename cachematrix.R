## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Like the example given in mean. First we creat a null object called inv.
## The steps followed are similar to the example program. 
## Here we are calculating the inverse of matrix which is a special matrix
## Which can cache its inverse for the input. Only a square matric can invertable.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This is also similar to the eaxmple. 
## the FUN calculates the inverse of matrix calculated in above FUN
## It checks if inv is null: if yes then calculates the result. 
## if no it means it has already been calculated so will display the result
## already stored in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
 
# Output
#m <- matrix(rnorm(9), 3, 3)
#> s <- makeCacheMatrix(m)
#> cacheSolve(s)
#[,1]       [,2]        [,3]
#[1,] -3.0496520 -1.9466311 -1.56778708
#[2,] -3.3856609 -1.6791424  0.09793871
#[3,] -0.8195609  0.5042958  0.13202845
