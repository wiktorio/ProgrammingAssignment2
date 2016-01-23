## Those functions are doing matrix inversion and uses caching

## This function create list of functions for getting cached inversed matrixes
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setim <- function(inverse) im <<- inverse
        getim <- function() im
        list(set = set, get = get,
             setim = setim,
             getim = getim)
}


## This function uses the list of functions to get cched inversoin or solve it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getim()
        ## check if it was cached
        if(!is.null(im)) {
                ## it was cached
                message("getting cached data")
                return(im)
        }
        ## solve inversion
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
}
