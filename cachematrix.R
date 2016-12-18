## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = is.matrix())
{
        sm <- NULL
        set <- function(y) {
                x <<- y
                sm <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) sm <<- solve
        getinverse <- function() sm
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        si <- x$getinverse()
        if(!is.null(si))
        {
                message("getting cached data")
                return(si)
        }
        data <- x$getinverse()
        si <- solve(data, ...)
        x$setinverse(si)
        si
}
