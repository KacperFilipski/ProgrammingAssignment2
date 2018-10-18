## makecacheMatrix - function to store inversed matrix result

makecacheMatrix <- function(x= matrix()) {
    inv <- NULL         ## creating empty variable to store inversed matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverseMatrix <- function(solve) inv <<- solve
    getinverseMatrix <- function() inv
    list(set=set, get=get,
         setinverseMatrix = setinverseMatrix,
         getinverseMatrix = getinverseMatrix)
}

##cacheSolve - function to invert a matrix using solve function

cacheSolve <- function(x, ...) {
    inv <- x$getinverseMatrix()
    if(!is.null(inv)) {                 ##checking cache for stored inverted data
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverseMatrix(inv)
    inv
}