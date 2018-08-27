## Caches the inverse of a matrix


## makeCacheMatrix() creates a function list that allow you to assign and retrieve
## data within an object

makeCacheMatrix <- function(x = matrix()) {    ## Function that takes a matrix
      m <- NULL                                ## Sets m to NULL           
      set <- function(y) {                     ## Sets the input of set function to x from parent environment and sets m to NULL
            x <<- y
            m <<- NULL
      }              
      get <- function() x                      ## Gets x from parent environment
      setinverse <- function(inverse) m <<- inverse   ## Sets value of m in parent environment                                                 
      getinverse <- function() m                      ## Gets m from parent environment
      list(set = set, get = get,               ## Assigns names to functions and puts them in a list (ends up in parent environment)
           setinverse = setinverse,
           getinverse = getinverse)}



## Calculates the inverse of the matrix created in makeCacheMatrix()
## and returns that matrix (m). If it's already been calculated it
## returns the matrix from the cache.

cacheSolve <- function(x, ...) {
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
