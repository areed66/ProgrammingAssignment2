## Put comments here that give an overall description of what your
## functions do

## This function caches the inverse of a matrix.  Takes the 
# get() gets the matrix that was cached
# getInv() gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function() i <<- solve(x)
  getInv <- function() i
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function checks to see if the cache has already been calculated, if so it gets the cache from the makeCacheMatrix, otherwise it calculates the inverse of the data.  The cache will save compute time.

cacheinv <- function(x, ...) {
        i <- x$getInv()
        if (!is.null(i)) {
                message("getting cache")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv()
        i
}

m <- makeCacheMatrix(matrix(c(1,2,3,4),ncol=2,nrow=2))
cacheinv(m)
m$get()
m$getInv()
