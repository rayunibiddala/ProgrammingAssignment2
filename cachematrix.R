## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. Following functions help achieve caching of matrix inverse result. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated ("and the matrix has not changed"), 
## then the cacheSolve should retrieve the inverse from the cache

## Usage: 
## > source("cachematrix.R")
## > f <- makeCacheMatrix(matrix(4:7, nrow=2,ncol=2))
## > cacheSolve(f)
##  [,1] [,2]
##  [1,] -3.5    3
##  [2,]  2.5   -2
## > cacheSolve(f)
##  getting cached data> 
##  [,1] [,2]
##  [1,] -3.5    3
##  [2,]  2.5   -2
## > f <- makeCacheMatrix(matrix(7:10, nrow=2,ncol=2))
## > cacheSolve(f)
##  [,1] [,2]
##  [1,]   -5  4.5
##  [2,]    4 -3.5



## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inverse to NULL
    m <- NULL
    
    ## Setter to initialize matrix
    set <- function(y){
        x<<- y
        m <<- NULL
    }
    
    ## Getter to retrieve matrix 
    get <- function() x
    
    ## Setter to result of matrix inverse
    setinverse <- function(solve) m <<- solve
    
    ## Getter to retrieve result of matrix inverse
    getinverse <- function() m 
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve: This function calculates and returns inverse of matrix. 

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()    
    data <- x$get()
    
    ## Check if inverse previously calculated and matrix has not changed before returning cached result 
    if (!is.null (m) && 
        !is.null(m$original) && 
        identical(m$original, data))
    {
        message("getting cached data")
        return (m$inverse)
    }
    
    ## store original matrix and inverse matrix
    m <- list(original=data,inverse=solve(data, ...))
    x$setinverse(m)
    
    ## return inverse 
    m$inverse
}
