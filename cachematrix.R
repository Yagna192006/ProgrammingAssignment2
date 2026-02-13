## =========================================================
## makeCacheMatrix: creates a special "matrix" object that
##                  can cache its inverse
## =========================================================

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL   # this will store the cached inverse
  
  # set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # reset cached inverse when matrix changes
  }
  
  # get the matrix
  get <- function() x
  
  # set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # get the inverse
  getInverse <- function() inv
  
  # return list of functions
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## =========================================================
## cacheSolve: computes the inverse of the special matrix.
##              If already cached, return cached version.
## =========================================================

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  # if inverse already exists, return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # otherwise compute inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache the inverse
  x$setInverse(inv)
  
  inv
}
