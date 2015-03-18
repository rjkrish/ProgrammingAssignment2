## cachematrix.R
##
## This file defines a pair of functions that implement
## matrix inversion. Matrix inversion is an expensive
## operation; the functions in this file provide for
## caching a once-computed inverse in order to significantly 
## speed up repeated inversions of the same matrix.
##
## usage:
##       matrix  <- makeCacheMatrix( rbind( c(1, -1/4), c(-1/4, 1) ) )
##       inverse <- cacheSolve( matrix )
##

## Returns a wrapper around an invertible square matrix,
## and its inverse along with accessors and mutators for the 
## matrix and its inverse. 
##
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
          x <<- y
          inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse     
        list( set = set
            , get = get
            , setInverse = setInverse
            , getInverse = getInverse )
}

## Returns the inverse of an invertible square matrix.
## The inverse is computed upon first invocation and 
## cached; subsequent invocations return the cached 
## inverse.
##
cacheSolve <- function(x, ...) {
        inv <- x$getInverse() 
        if( is.null(inv) ){
            inv <- solve( x$get() )
            x$setInverse(inv)
        }
        inv
}

## end