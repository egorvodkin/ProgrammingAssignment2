## Function calculates the inverse matrix for a chosen matrix, stores it in a and gets its value.


makeCacheMatrix <- function(x = matrix()) {
    
     m <- NULL
     set <- function(y) {
      x <<- y
       m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
    
  }


+## Cashes inverse of a matrix
  +
  cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    m
    
  }
