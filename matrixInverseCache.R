# Create a matrix with the ability to store it's inverse as a variable, caching it for later use without computation time.
makeCacheMatrix <- function(x = matrix()) {
# default the inverse to null
  inv <- NULL
# Sets the value of the matrix and resets the invers to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
# returns the matrix data
  get <- function() x
# allows the manual setting of the inverse
  setinverse <- function(inverse) inv <<- inverse
# returns the inverse value  
  getinverse <- function() inv
# list of available methods for the matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Extracts the inverse from our previous matrix object, whether cached or not, simply 
# skipping computation if the matrix has a previously cached inverse.
cacheSolve <- function(x, ...) {
# Initial retrieval of inverse from parameter
  inv <- x$getinverse()
# Execute if the inverse has not been previously computed (is null)
  if(!is.null(inv)) {
    message("getting cached data")
# Exit function, returning the inverse
    return(inv)
  }
# Procede with calculating inverse if no  cache
  data <- x$get()
# Solve for the inverse
  inv <- solve(data, ...)
# Cache inverse for future use
  x$setinverse(inv)
# Return the inverse
  inv
}