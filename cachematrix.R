# Those functions benefits from the lexical scope to store one matrix and its 
# inverse in another environment used as cache and prevent unecessary reevaluations
# of the solve() function. The functions are both largely based on the examples 
# presented on the assigment text.

# Returns a list of functions that allows the user to store two 
# matrices "x" and "i' in cache through the functions set() and setinverse() 
# respectively and obtain those same matrices through get() and getinverse().
# OBS.: If the matrice "x" changes "i" is automaticaly erased.

makeCacheMatrix <- function(x = matrix()) {
     
     i <- NULL
     
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     get <- function() x
     
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


# Verifies if "x" has already its inverse calculated on cache. If it has, 
# returns the cached value and warns the user that it is the cached version, 
# otherwise, evaluate the inverse through the solve() function and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached inverse")
          return(i)
     }
     m <- x$get()
     i <- solve(m)
     x$setinverse(i)
     i
     
}
