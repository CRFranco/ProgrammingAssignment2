## Sometimes it pays off to develop some kind of caching mechanism for
## operations that are constly in terms of computation.
## The two functions below create a cache for inverse of a matrix

## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        cachedInverseMatrix <- NULL

        set <- function(value = matrix()){
                x <<- value #put x in the parent env with the original matrix
                cachedInverseMatrix <<- NULL
        }

        get <- function() x

        setInverse <- function(inverseValue){
                cachedInverseMatrix <<- inverseValue
        }

        getInverse <- function() cachedInverseMatrix

        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been calculated,
## returns the inverse matrix from the cache
cacheSolve <- function(x, ...) {
        cachedInverseMatrix <- x$getInverse()
        # if it is already in cache
        if(!is.null(cachedInverseMatrix)){
                message('Obtaining cache data')
                return(cachedInverseMatrix)
        }
        # first execution creates the cache
        data <- x$get()
        cachedInverseMatrix <- solve(data) #calculates the inverse of a sqare matrix
        x$setInverse(cachedInverseMatrix)
        return(cachedInverseMatrix)

       
}
