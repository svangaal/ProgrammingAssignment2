## This is my solution to the Programming in R week 2 assignment
## It is intended to check for a previously calculated solution
## to a matrix inversion, and if none is available, to calculate it

## This function prepares a list of four functions that can be used 
## to: 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function takes as input the matrix that is to be inverted
## First it checks to see if the inversion has already been performed and 
## the result chached. If so, it uses that value. 
## If not, it calculates the value, caches the value, and returns the value.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}