## These two functions cache the inverse of a given matrix.
## If the inverse has not yet been calculated then the cachesolve
## computes it.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL}
  get <-function() x
  setinverse <- function(inverse) inve <<- inverse
  getinverse <- function() inve
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inve <- x$getinverse()
        if(!is.null(inve)){
          message("getting catched inversed matrix")
          return(inve)
        }
        matrx <- x$get()
        inve <- solve(matrx,...)
        x$setinverse(inve)
        inve
}
