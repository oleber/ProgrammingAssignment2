## Calculats and caches the inverse of an matrix

## Converts a Matrix x in an object that is able of caching the inverse when calculated via cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m

    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve,
         dump = function() list(x=x,m=m)
         )
}


## Calculate and caches the inverse of the CacheMatrix x (matrix wrapped via makeCacheMatrix). If the result is not on x cache,
## the result is calculated, cached in x and returned. If the result is on x cache, the result is served from the cache. 
## Remember: Calling set on x will clean the cache.
## return: the inverse of x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
      message("getting cached data")
    } else {
      m <- solve(x$get(), ...)
      x$setSolve(m)
    }
    m
}
