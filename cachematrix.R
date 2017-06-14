## Creating a function to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix())
{
        inv <- NULL
        set <- function(y)
        {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Inverse of the matrix created by above function

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv))
        {
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
