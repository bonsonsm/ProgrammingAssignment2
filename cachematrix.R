##  There are two functions in the class.
##  1. makeCacheMatrix helps create a matrix object and cache its inverse
##  2. cacheSolve helps compute the inverse of the matrix


##  This function creates a special "matrix" object 
##  that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##  Global variable that would hold the matrix inverse
    matinverse<<- NULL
    ##  Setting the matrix
    set <- function(y) {
        x <<- y
        matinverse <<- NULL
    }
    ##  Retrieving the matrix
    get <- function() x
    ##  Setting the inverse of the Matrix
    setinverse <- function(x) matinverse <<- solve(x)
    ##  Getting the inverse of the matrix
    getinverse <- function() matinverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. 
##  If the inverse has already been calculated 
##  (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    tempm <<- x$getinverse()
    if(!is.null(tempm)) {
        ##  Returning the Cached data in the Global Variable
        print("getting cached data")
        return(tempm)
    }
    ##  Calculating data for the first time
    data <- x$get()
    ##  Setting the inverse
    x$setinverse(data)
    ##  returning the Global Variable
    matinverse
}
