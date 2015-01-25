## Two step process: First create a matrix named makeCacheMatrix which will
## be a vector of objects that hold information related to matrix inverse

## The function cacheSolve will check to see if inverse has already been
## calculated. If not, it will compute the new inverse

## To test cacheSolve, run it twice, the first time it will calculate the
## inverse, the second time, when run, it will say 'Retrieving cached data"
## then give the inverse

makeCacheMatrix <- function(x = matrix()) { ## this is function that creates a new R object
  matrix_inverse <- NULL ## calls a new function, made null so it doesnt get corrupted by new data
  set <- function(y) { ##
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrix_inverse <<- solve
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} ## these four functions recall the previously calculated mean
##        Length Class  Mode    
## set     1      -none- function
## get     1      -none- function
## setmean 1      -none- function
## getmean 1      -none- function
## This is a summary, a list of four objects used when you implement cacheSolve

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
## This is how it works: first you define a new matrix with makeCacheMatrix
## Then calculate the new matrix and use cacheSolve to calculate the inverse
## You run cacheSolve twice, once to to calculate, and twice to make sure
## the matrix was stored and then retrieved
