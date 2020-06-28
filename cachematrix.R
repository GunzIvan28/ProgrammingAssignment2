## The goal of these functions is to create a special object that stores a matrix and caches its inverse




## The makeCacheMatrix function creates a 'special matrix' containing function instructions that:
#       - set the value of the matrix
#       - get the value of the matrix
#       -set the value of the inverse
#       -get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function takes the 'special matrix' output from the makeCacheMatrix function 
# and computes its inverse.
# Incase the inverse is already pre-computed(the matrix is still the same), the cacheSolve function
# retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}



## TESTING THE FUNCTIONS

# First create a a matrix on assumption that it is always invertible
C <- matrix(c(1:4),2,2)

# Next we compute the inverse 
C_inv <- makeCacheMatrix(C)


# CacheSolve then retrieves the inverse from the cache list, changes the call matrix to the inverse,  
# computes the inverse and returns the original function
cacheSolve(C_inv)


