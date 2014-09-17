#  Following two functions will calculate and cache the inverse of a matrix at its first use. When we need it again, it can be looked up in the cache rather than recomputed.

#################################################################################

# This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        # set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # get the value of the matrix
        get <- function() x

        # set the value of the inverse
        setinverse <- function(inverse) m <<- inverse

        # get the value of the inverse
        getinverse <- function() m

        # return the results
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#################################################################################

#  This function retrieves the inverse of the matrix from the cache. 
#  If not in the cache, computes inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {

        m <- x$getinverse()

        #  Return the inverse if available in the cashe
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        #  calculate the inverse and cashe it when not there
        data <- x$get()
        m <-  solve(data, ...)
        message("getting calculated data")
        x$setinverse(m)

        # return the results
        m

}
