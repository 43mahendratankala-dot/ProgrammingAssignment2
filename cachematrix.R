This code is given to fulllfillment of assiginment
## Creates a special "vector" object that can cache its mean

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             ## holds the cached inverse matrix
    set <- function(y) {                    ## assign new matrix value
        x <<- y
        inv <<- NULL                       ## reset cache for new matrix
    }
    get <- function() x                     ## return the matrix
    setinverse <- function(inverse) inv <<- inverse  ## cache the inverse
    getinverse <- function() inv            ## return cached inverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## Computes inverse of the special matrix, using cache if available
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)                 ## compute inverse
    x$setinverse(inv)                       ## cache it
    return(inv)
}
