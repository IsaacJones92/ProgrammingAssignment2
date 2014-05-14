
##The functions will take a matrix and put the inverse of the meatrix into a cache 
##to be used for future use

##The following function will create a list of functions that will be used to create the 
##cache object

makeCacheMatrix <- function(x = matrix()) {
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInv<- function(Inv) m <<- Inv
      getInv <- function() m
      list(set = set, get = get, setInv= setInv, getInv = getInv)
}

## This function will calculate the inverse of the matrix and also checked it the value 
## previously been cached 

cacheSolve <- function(x, ...) {
      m <- x$getInv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInv(m)
      m
      
      ## Return a matrix that is the inverse of 'x'
}
