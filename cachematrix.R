## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## declare the cache object and assign it the NULL value
        cache <- NULL

        ## setter function for the matrix
        ## the <<- operator can be used to assign a value to an object in
        ## an environment that is different from the current environment.
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

        ## getter function for the matrix
        get <- function() x

        ## setter function for the inverse matrix
        setInverse <- solve(x)

        ## getter function for the inverse matrix
        getInverse <- function() x

        ## create a list of the above functions
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse

        ## display the inverted matrix if it exists in cache
        if (!is.null(cache)) {
               cache
        }
        else
        {
                ## the inverted matrix is not in cache, so create it       
                solve(x)
        }
}
