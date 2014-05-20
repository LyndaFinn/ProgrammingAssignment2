## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse


## Function 1: `makeCasheMatrix` creates a list of functions to:
##
## 1.  set the value of the vector (set)
## 2.  get the value of the vector (get)
## 3.  set the value of the inverse (setinv)
## 4.  get the value of the inverse (getinv)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Set Function
        set <- function(y) {
            x <<- y
            m <<- NULL
    }
    ## Get Function
    get <- function() x
    ## Setinv Function
    setinv <- function(solve) m <<- solve
    ## Getinv Function
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the mean in the cache via the `setmean`
## function.


cacheSolve <- function(x, ...) {
    m <- x$getinv()
    # If a cached result exists, exit and return the cache with message
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    # Otherwise invert the matrix, and print and return it
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

###### Sample Test cases
# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# amatrix$get()         # Returns original matrix
# cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# amatrix$getinv()  # Returns matrix inverse
# cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
# 
# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# amatrix$get()         # Returns matrix
# amatrix$getinv()  # Returns matrix inverse
#####