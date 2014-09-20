## This program has two functions defined: makeCacheMatrix and cacheSolve
## Both receive as input a Identity qualified matrix
## makeCacheMatrix will keep the Identity Matrix, if generated and will retrieve it 
## by getmatriz() call. If no prior Identity matrix has been generated NULL will be displayed
## cacheSolve() will generate the identity matrix retrievable by getmatriz() function

## Function that sets the Identity matrix in cache, retrievable 
 
makeCacheMatrix <- function(x = Numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatriz <- function(solvex) m <<- solvex
        getmatriz <- function() m
        list(set = set, get = get,
             setmatriz = setmatriz,
             getmatriz = getmatriz)
}

## Function that will get the Identity Matrix if it has not been generated, otherwise returns the ## one in Cache
cacheSolve <- function(x, ...) {
        m <- x$getmatriz()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setmatriz(m)
        m
}