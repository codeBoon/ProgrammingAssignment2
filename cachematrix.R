## Using Lexical Scoping to preserve state inside R object
## Program saves the inverse of the matrix to avoid costly computations

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL             # Initialize to null
    # function to assign value from makeCacheMatrix() environment
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    # function to get the matrix
    get <- function() x
    
    #function uses lexical scoping to get inverse from the environment
    setinverse <- function(inverse) m <<- inverse 
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Function calculates the inverse of the created matrix, if it has not been,
## already calculated..

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## check if cache is not empty retrive cached data.
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)       # compute the inverse of the matrix.
    x$setinverse(m)
    m

}
