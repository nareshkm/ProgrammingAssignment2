
## 1. makeCacheMatrix - Creates a special matrix that actually
##                      has list containing functions to
##                              1.  Set the value of matrix 
##                              2.  get the value of matrix 
##                              3.  Set the value of inverse 
##                              4.  get the value of inverse 

## 2. cacheinverse -    Fetches the inverse of matrix if exists in cache,
##                      else computes the inverse


## makeCacheMatrix 
##      Input  -  A non-singular square matrix that is invertible
##      Output -  Returns a LIST with 4 functions
##                      1. set()
##                      2. get()
##                      3. setinverse()
##                      4. getinverse()

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        ## The below function setinverse, sets the inverse of matrix
        ## to global environment using <<- assignment operator in R
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
        
}


## cacheSolve 
##      Input  -  A list returned from makeCacheMatrix function
##      Output -  Returns the INVERSE value of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }        
        data <- x$get()        
        ##Compute inverse of matrix using solve function
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
