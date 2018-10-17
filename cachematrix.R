

makecacheMatrix <- function(x= matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverseMatrix <- function(solve) inverse <<- solve
    getinverseMatrix <- function() inverse
    list(set=set, get=get,
         setinverseMatrix = setinverseMatrix,
         getinverseMatrix = getinverseMatrix)
}



cacheSolve <- function(x, ...) {
    inverse <- x$getinverseMatrix()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverseMatrix(inverse)
    inverse
}