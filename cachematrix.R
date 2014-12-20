## This file contains two function makeCachMatrixs and cachSolve. The first function 
## returns an object that wrapps a matrix. The wrapper also can manage the matrix 
## and its related object


## makeCacheMatrix returns an object that can store a matrix and its related values
## for example, it can cache an inverse of a matrix. the object also have a set 
## function that reset the wrapped matrix and clear any cached valuem if anty

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSove delegate to solve function to get the inverse of a given
## matrix via  the wrapper object returned by above makeCacheMatrix function,.
## If the object does have a cached inverse. The value will be 
## returnd. Otherwise, it calculate the inverse and store it 
## the wrapper object before it return. 

cacheSolve <- function(x, ...) {
  
        sol <- x$getinv()
        if (!is.null(sol)) {
          message("getting cached data")
          return (sol)
        }
        data<-x$get()
        sol <- solve(data,...)
        x$setinv(sol)
        sol
}
