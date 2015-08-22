#makeCacheMatrix function
#creates a special Matrix and cache its inverse returned from cacheSolve
#cacheSolve function
#returns the inverse from cache if already calculated
#calculates the inverse for the first time, sends to cache in makeCacheMatrix

#Creates a special matrix whose inverse can be cached
makeCacheMatrix <-  function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#Returns the inverse of special matrix created using makeCacheMatrix
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
