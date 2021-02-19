## This file contains two functions:
## makeCacheMatrix - maintains a cache of a matrix and its inverse
## cacheSolve
## Comments at the end describe how to test


## This function stores a special "matrix" and its inverse that are manipulated
## with a list of functions:
##          set - stores the value of the matrix (and NULLs the inverse)
##          get - returns value of the matrix
##          setInverse - stores the value of the inverse
##          getInverse - returns the vale of the matrix

makeCacheMatrix <- function(X = matrix()) {
        Inverse <- NULL
        
        set <- function(y) {
                X <<- Y
                inverse <<- NULL
          
        }
        get <- function() X
        setInverse <- function(I) Inverse <<- I
        getInverse <- function() Inverse
        list (set = set, get=get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated for the current "matrix", it returns the cached inverse
## to save time.  If it has not been calculated, it calculates the inverse, stores the inverse in the cache
## and returns the newly calculated inverse.

cacheSolve <- function(x, ...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message ("Getting cached inverse")
    return(Inv)
  }
  M <- x$get()
  I <- solve(M,...)
  x$setInverse(I)
  I      
}

## Test sequence for the functions
##
## 
## m1 <- matrix(rnorm(25),5,5)  # populate a matrix
## solve(m1)                    # Calculate it's inverse
##
## M <- makeCacheMatrix(m1)     # Create the cached matrix
## cacheSolve(M)                # Calculate inverse (no message)
## cacheSolve(M)                # Retrieve inverse from cache (expect message)



## 


