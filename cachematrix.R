## These functions create a list to cache the inverse of a matrix in.
## The inverse is calculated by a second function or read out if it was already 
## calculated.



## "makeCacheMatrix" creates a list with the necessary functions for "cacheSolve"

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        setMatrix <- function(y) {
                if(!identical(x,y)) { # only reset if the new matrix is different
                        x <<- y
                        inv <<- NULL # reset inv as the matrix has changed
                }
                else {message("new matrix identical to old, keeping cached data")}
        }
        getMatrix <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setinv = setinv,
             getinv = getinv)
}


## "cacheSolve" reads the inverse matrix from the cache list if present 
## or calculates it if not, the inverse matrix is returned.

cacheSolve <- function(x, ...) {
        q <- x$getinv()
        if(!is.null(q)) {
                message("getting cached data")
                return(q)
        }
        data <- x$getMatrix()
        q <- solve(data)
        x$setinv(q)
        q
}
