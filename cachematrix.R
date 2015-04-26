# Caching the inverse of a matrix

## Matrix invertibility can be a very time consuming job when
## they are either:
## 1) Very large matrices; or
## 2) Multiple matrices wrapped in an array, list, etc.
##
## As such, we address this issue by creating functions which
## stores these matrices and "prepare" them to be invertible,
## and then create the invertible matrices from these 
## stored "prepared" matrices. Below we shall briefly overview
## what each function is doing. 


### The function below creates a special matrix object that
### can cache the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
  inv.x <- NULL
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv.x <<- inverse
  getinverse <- function() inv.x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### The function below computes the inverse of the special 
### matrix returned by makeCacheMatrix above. If the inverse
### has already been calculated (and the matrix has not 
### changed) then the cachesolve should retrieve the 
### inverse from the cache.

#### We shall assume that the matrix being input is always
#### invertible.

cacheSolve <- function(x, ...) {
  inv.x <- x$getinverse()
  if(!is.null(inv.x)) {
    message("obtaining cached inverse matrix")
    return(inv.x)
  }
  else {
    inv.x <- inverse(x$get(), ...)
    x$setinverse(inv.x)
    return(inv.x)
  }
}
















