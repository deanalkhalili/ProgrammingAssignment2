
# We need to increase the computational performance of a matrix 
# inversion function. 
 

# One functions will create a matrix object that can cache its inverse.
# The second function will compute the inverse of the object using solve()
# and store it in cache.



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
           x <<- y
           inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){
              inver <- ginv(x)
              inver%*%x
  }
                
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function will compute the inverse of the matrix object returned
# by the first function and save it in cache for subsequent queries.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}