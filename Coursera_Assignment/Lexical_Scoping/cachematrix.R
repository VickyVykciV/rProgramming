#Here we are using Lexical Scoping to cache a data and using it for later purpose, 
#instead of repeating the certain operation again and again

#This specific function holds return the list of functions which helps you to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#This function returns the inverse of a data
#It inturn calls the above function to stores(cache) the inverse of a matrix, for the first time
#For further calls, it will return the matrix inverse from the cache instead of calculating it again
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
