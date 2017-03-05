## Write a short comment describing this function
## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
## 1. receives a matrix vector defined by x
## 2. set the value of the vector
## 3. get the value of the vector
## 4. get the value of the inverse
## 5. set the value of the inverse

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

## This function calculates the inverse of the special "vector" created with the above function. 
## It first does a check to see if the inverse has already been calculated.
## If true, then it gets the inverse from the cache and skips the computation.
## If false, then it calculates the inverse of 'data' and sets the value of the inverse in the cache using the setinverse function.

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
