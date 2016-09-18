## The two functions would take a matrix and then
## 1. Invert the matrix if it has not been inverted before
## 2. Retrive the stored inverted matrix if it had been inverted before.

## makeCacheMatrix generate the four functions that link to the matrix needed to be inverted.

makeCacheMatrix <- function(x = matrix()){
        invertX <- NULL
        set <- function(y){
                x <<- y
                invertX <<- NULL
        }
        get <- function() x
        setInvert <- function(invert) invertX <<- invert
        getInvert <- function() invertX
        list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}

## cacheSolve would check if a matrix had been inverted. If a matrix has been invert, it retrieve the data stored in cache. 
## If the inverted matrix had not been generated, it will generate the inverted matrix anew.

cacheSolve <- function(x) {
        m <- x$getInvert()    
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInvert(m)
        m
}
