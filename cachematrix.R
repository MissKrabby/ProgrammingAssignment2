## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<-NULL
        }
        get <- function() x
        setinver <- function(inverse) i <<- inverse
        getinver <- function() i
        list(set = set, get=get,
             setinver = setinver,
             getinver = getinver)
}


## "cacheSolve" computes the inverse of the special "matrix" returned by "makeCacheMatrix"
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getinver()
        if(!is.null(i)) {       ## if the inverse has already been calculated
                message("getting cached data")
                return(i)       ## retrieve the inverse from the cache
        }
        data <- x$get()
        i <- solve(data, ...)   ## compute the inverse
        x$setinver(i)
        i        
}