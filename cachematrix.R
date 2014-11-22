# This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  
              inv <- NULL # set inverse matrix to NULL
              set <- function(y) { # Modifies existing matrix to Y
                x <<- y
                inv <<- NULL
              }
              get <- function() x # Returns original matrix
              setinverse <- function(inverse) inv <<- inverse # Set inverse of the matrix
              getinverse <- function() inv # Returns matrix inverse
              list(set = set, get = get,
                   setinverse = setinverse,
                   getinverse = getinverse)
}


# Computes, caches, and returns matrix inverse.
# If the inverse has already been calculated, then the cachesolve take inverse from cache,
# if not it calculates inverse and writes it in the cache for new matrix:

cacheSolve <- function(x, ...) {
  
              inv <- x$getinverse()  # Return a matrix that is the inverse of 'x'
              if(!is.null(inv)) { # Checks if there is a cache for inverse
                message("getting cached data")
                return(inv)
              }
              data <- x$get()
              inv <- solve(data, ...)
              x$setinverse(inv)
              inv
}
