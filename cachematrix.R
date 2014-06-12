## These two functions will get the invert of a matrix by either calculating it directly or sourcing
## it from the cache

## makeCacheMatrix defines the functions necessary to create the matrix, get the matrix,invert the matrix, get the resulting inverted matrix 

makeCacheMatrix <- function(x = matrix()) {

      
            m <- NULL
            set <- function(y) {
                  x <<- y
                  m <<- NULL
            }
            get <- function() x
            
            invert <- function(solve){
                  m <<- solve
            }
            
            getinvert <- function() m
            list(set = set, get = get,
                 invert = invert,
                 getinvert = getinvert)
      
}


## cacheSolve uses the functions from makeCacheMatrix to execute the invertion on the matrix
## or retreive it from the cache if it's already been calculated

cacheSolve <- function(x, ...) {
      m <- x$getinvert()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$invert(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
