## These are 2 functions that together create a matrix and then proceeds
## to caculate the inverse of the matrix
## If the inverse has already been calculated (and the matrix has not changed)
## the cached value is used instead
## This is done because matrix inversion is usually a costly computation


## This first function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated , then the functions retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Returns the inverse of x (martix)
   inv <- x$getinv()
   ## Returns cached value when available
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   ## Calculated the inverse if there's no cached data (ie it's null)
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv)
   inv
}