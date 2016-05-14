# This two functions can calculate the inverse of a given matrix. If the 
# inverse of the matrix has been calculated, the function can get the 
# inverse from the cache directly. If not, the function can calculate the
# inverse and save the answer to the cache.
# Assume the matrix supplied is always invertible. 

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
# Declaring the functions needed to create the list returned
# set() is used to assign the matrix need to be inversed to the variable x in parent environment
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
# get() is used to return the matrix x
        get <- function() x
# setinverse() is used to assign the calculated result to variable in perent environment
        setinverse <- function(inverse) i <<- inverse
# getinverse() is used to return the variable assigned in setinverse()
        getinverse <- function() i
# return a list contains the functions needed to calculate the inverse in cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.

cachesolve <- function(x, ...) {
        m <- x$getinverse()
# Check the inverse has been calculated or not
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
# Get the metrix needed to be inversed
        data <- x$get()
# Calculate the inverse via solve()
        m <- solve(data, ...)
# Assign the result via setinverse()
# so it can be found if we calculate the same matrix next time
        x$setinverse(m)
# Return the result
        m
}
