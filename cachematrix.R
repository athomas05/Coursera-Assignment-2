## Code for Assignment 2. 
##


##makeCacheMatrix- creates functions that will set, get the inverse of given matirx

makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL
        setmat <- function(Y) {
                X <<- Y
                inv <<- NULL
        }
        getmat <- function() X
        setinverse <- function(MatInv) inv <<- MatInv
        getinverse <- function() inv
        list(setmat = setmat, getmat = getmat,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve will either calculate the inverse or simply retrieve the cached inverse 

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- X$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix_to_cache <- X$getmat()
        #Matrix inverse
        inv <- solve(matrix_to_cache)
        #Matrix inverse ends
        X$setinverse(inv)
        inv
}
