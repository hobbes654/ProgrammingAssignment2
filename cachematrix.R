## Put comments here that give an overall description of what your
## functions do
## There are two functions on this file.  When used together this functions will help inverse a matrix
## but in the case that the matrix value was obtained once then value will be obtain from cache saving 
## calculation time

##makeCacheMatrix is a function that takes a matrix as an argument and creates 
## a special list with 4 elements set, get, setinverse, and getinverse. These four elements are functions
## that can be used by other functions that take the special list as an argument
## and taking advantage of lexical scoping in R

makeCacheMatrix <- function(x = matrix()) {
      matrix1 <- NULL
      set <- function(y) {
            x <<- y
            matrix1 <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) matrix1 <<- inverse
      getinverse <- function() matrix1
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cachsolve takes as an argument as special list created with makecachematrix function
## cachesolve uses the functions that are passed in the special list argument to calculate the 
## inverse of the matrix.  If the matrix has not been cached getinverse will return null and 
## we will use solve to inverse the matrix.  After this we will store the value obtained by 
## calling setinverse
## the next time the matrix is entered the cache will be used.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      matrix1 <- x$getinverse()
      if(!is.null(matrix1)) {
            message("getting cached data")
            return(matrix1)
      }
      data <- x$get()
      matrix1 <- solve(data, ...)
      x$setinverse(matrix1)
      matrix1
      
}
