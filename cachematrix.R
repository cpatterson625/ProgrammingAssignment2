## makeCacheMatrix takes a matrix as an argument and creates a list containing functions that:
##  1. set the matrix value
##  2. get the matrix value
##  3. set the matrix inverse value
##  4. get the matrix inverse value


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the matrix inverse value.  If the value has been already solved, it
## returns cached data.  If not, then it solves for the inverse of the matrix and caches
## the solution.

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
         if(!is.null(inv)){
             message("getting cached data")
             return(inv)
         }
         data <- x$get()
         inv <- solve(data)
         x$setinverse(inv)
         inv
}
