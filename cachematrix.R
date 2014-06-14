## These two functions provide a wrapper around a matrix object to reduce calls to the "solve" function when
## computing an inverse matrix.  Inverse is calculated once and then stored in the makeCacheMatrix wrapper

## This function takes a matrix object and constructs a wrapper that stores an inverse matrix once calculated
makeCacheMatrix <- function(x = matrix()) {
      
        ## initialize inverse value 
        mInv <- NULL
      
        ## function to reset value of matrix and inverse
        set <- function(mm){
                m <<- mm
                mInv <<- NULL
        }## end func set

        ## Function to return original matrix
        get <- function() m
      
        ## function to return inverse matrix
        getInverse <- function() mInv

        ## function to calculate inverse matrix
        setInverse <- function(solve) mInv <<- solve

        ## return list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}## end function makeCacheMatrix


## Function returns the inverse matrix of a "makeCacheMatrix" object, either by looking up the value
## if previous calculated, or calling the "solve" function for the first time
cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        
        ## check if inverse was previously calculated
        if(!is.null(m)) {
                return(m)
        }

        ## if inverse is needed, then pull matrix and send to solve()
        data <- x$get()
        m <- solve(data, ...)

        ## store inverse for later then return it
        x$setInverse(m)
        m

}## end function cacheSolve
