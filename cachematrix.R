## These functions implement a cached matrix inverse calulation. 
## 
## The inverse calculation (solve function) is computationally
## expensieve. When the same matrix is solved multiple times, it
## returns the cached value instead of calling solve() function.
##
## It assumes the matrix is invertible.
##
## Usage:
##
##  original.matrix <- matrix(c(1, 0, 0, 1), 2, 2)
##  cache.matrix <- makeCacheMatrix(original.matrix)
##  cacheSolve(cache.matrix)

## Make a cache matrix. The returned matrix exposes four functions.
##
##  set      - set a new matrix
##  get      - get the matrix
##  setsolve - store the calculated inverse for the future use.
##  getsolve - retrieve the calculated inverse.
makeCacheMatrix <- function(x = matrix()) {
    # The variable s keeps the inverse of the matrix for future use.
    s <- NULL

    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s

    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function solves the matrix. When cached inverse is available,
## it simply returns it. Otherwise, it invokes solve() function to
## calculate the inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }

    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

