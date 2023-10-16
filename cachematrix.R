## R module to create a matrix that caches its inverse
## ===================================================
##
## Test 1:
## m <- makeCacheMatrix(matrix(c(0, 1, 1, 0), nrow=2, ncol=2))
## cacheSolve(m)
##
## Expected:
## 0 1
## 1 0
##
## Test 2:
## m <- makeCacheMatrix(matrix(c(3, 2, 0, 0, 1, 4, 1, 0, 4), nrow=3, ncol=3))
## cacheSolve(m)
##
## Expected:
##  0.2   0.2  -0.05
## -0.4   0.6   0.1
##  0.4  -0.6   0.15



## Create a CacheMatrix that caches its inverse when computed.
## Note: The inverse of the matrix has to exist!
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function (mat) {
        x <<- mat
        inverse <<- NULL
    }
    get <- function () x

    get_inverse <- function () inverse
    set_inverse <- function (mat) {
        inverse <<- mat
    }
    list(set = set,
         get = get,
         get_inverse = get_inverse,
         set_inverse = set_inverse)
}

## Return the inverse of the CacheMatrix x.
cacheSolve <- function(x) {
    # compute inverse if not cached yet
    if (is.null(x$get_inverse())) {
        inverse <- solve(x$get())
        x$set_inverse(inverse)
    }

    x$get_inverse()
}
