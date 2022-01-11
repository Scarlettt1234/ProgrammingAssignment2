## makeCacheMatrix is a consist of: set the matrix, get the matrix, set the inverse of matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                # initialize "inv" as null
        set <- function(y) {
                x <<- y
                inv <- NULL  
        }
        get <- function() x     #get the matrix
        setinv <- function(inverse) inv <<- inverse  
        getinv <- function() inv        #get the inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculated the inverse check to see if the inverse has been calculated before. 
##If it is not a null, then the inverse can be returned from cache data. Otherwise, it'll calculate the inverse. 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")   # if the inverse is not null, return the inverse value
                return(inv)
        }
        data<- x$get()
        inv <- solve(data, ...)   #otherwise, calculate the inverse
        x$setinv(inv)
        inv
}

##Test my functions
mat <- makeCacheMatrix(matrix(2:7, 2, 2))
mat$get()
mat$getinv()
cacheSolve(mat)
cacheSolve(mat)
mat$getinv()
