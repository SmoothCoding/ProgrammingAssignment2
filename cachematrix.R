## Similar to the example given in the programming assignment
## description I designed the following functions.
##  - The makeCacheMatrix function caches a matrix and its inverse.
##  - The cacheSolve function calculates the inverse if the matrix
##    has changed.


## makeCacheMatrix(x)
## ------------------
## Since the variable given as parameter x in the function is 
## defined in another environment, the operator <<- has
## to be used. This operator forces R to search for variables
## with the given name in parent environments.
## As soon as the matrix is set within the set()-function 
## the inverse matrix is set to NULL, which makes it possible
## to detect whenever the matrix changes.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve(x)
##---------------
## The parameter of this function is a makeCacheMatrix function.
## The function proceeds as follows:
## 1. Check wether the inverse is NULL (this does also account
##    for any changes)
## 2. If the inverse is not NULL then it is returned
## 3. If the inverse is NULL, the inverse of the matrix is 
##    calculated; cached in makeCacheMatrix function instance
##    and returned.

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
