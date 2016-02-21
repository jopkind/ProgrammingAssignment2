## The first function "makeCacheMatrix" stores the inverse of a matrix in an environment different from the current environment with the `<<-` operator. The second function "cacheSolve" calculates the inverse of the object created with the makeCacheMatrix function, however if this has already been calculated previsouly it will get the inverse from the cache which saves computating time.

## This function will:
1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse
4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}



## This function will:
1.  try to get the inverse from cache
2.  if it has been calculated before it will get the value from cache
3.  if not it will calculate the inverse of the matrix
4.  and sets the inverse of the matrix in cache via setinverse


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}