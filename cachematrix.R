## The function create a special matrix that can store the corresponding inverse.
## This is particularly useful if otherwise repeated computations are needed
## to calculate the inverse.

# makeMatrix() creates a special "matrix" object that can cache its inverse.

makeMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get     <- function() x
    setinverse <- function(minv0) minv <<- minv0
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve() computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then the
# `cachesolve` should retrieve the inverse from the cache.
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    mat    <- x$get()
    minv   <- solve(mat)
    x$setinverse(minv)
    minv
}
