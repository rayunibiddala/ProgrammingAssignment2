## Put comments here that give an overall description of what your
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache

## Write a short comment describing this function

## 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y){
        x<<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) m <<- solve
    
    getinverse <- function() m 
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()    
    data <- x$get()
    
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
