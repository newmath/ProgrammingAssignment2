## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## INIT CACHED INVERSE TO NULL
        m <- NULL
        ## SET METHOD FOR SETTING UNDERLYING MATRIX AND CLEARING CACHED INVERSE
        set <- function(y){
                x <<- y
                m <<- NULL
        } 
        ## GET METHOD TO RETRIEVE UNDERLYING MATRIX
        get <- function() x
        ## METHODS TO EXPLICITLY SET AND GET THE INVERSE
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        ## RETURN LIST OF FUNCTIONS
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## ATTEMPT TO GET CACHED INVERSE
        m <- x$getinv()
        ## IF EXISTS, RETURN CACHED RESULT
        if(!is.null(m))
                message("getting cached data")
                return(m)
        }
        ## OTHERWISE, GET OBJECT DATA
        data <- x$get()
        ## CALCULATE INVERSE
        m <- solve(data, ...)
        ## CACHE FOR LATER RETRIEVAL
        x$setinv(m)
        ## RETURN INVERSE
        m
}
