
# makeCacheMatrix creates a list containing a function for
# 1) seting the value of the matrix
# 2) getting the value of the matrix
# 3) setting the value of inverse of the matrix
# 4) getting the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }# end of set
    
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
} # end of makeCacheMatrix


## cacheSolve function assumes that the matrix is always invertible
# cacheSolve function returns inverse of the matrix. First it checks if
# the inverse has already been computed, If yes, it gets the result from the memory
# If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# 

cacheSolve <- function(x, ...) {
        
      i <- x$getinverse()
      # check for cached data
      if(!is.null(i)) {
          message("getting cached data.")
          return(i)
      }# end of if
     
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
      
}# end of cachesolve
