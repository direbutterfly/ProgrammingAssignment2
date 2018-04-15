#makeCacheMatrix function
#creates a special Matrix and cache its inverse returned from cacheSolve
#cacheSolve function
#returns the inverse from cache if already calculated
#calculates the inverse for the first time, sends to cache in makeCacheMatrix

#Creates a special matrix whose inverse can be cached
makeCacheMatrix <-  function(x = matrix()) {
        i <- NULL
        #Set the value
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #Get the Value
        get <- function() x
        #Set inverse
        setinv <- function(solve) i <<- solve
        #Get Inverse
        getinv <- function() i
        #Return list of values
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#Returns the inverse of special matrix created using makeCacheMatrix
cacheSolve <- function(x, ...) {
        #First get the inverse if it exists
        i <- x$getinv()
        #If it exists, return that the cached data is being used, return the cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #If it doesnt exist, calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
        #Cache the value for future
        x$setinv(i)
        i
}
