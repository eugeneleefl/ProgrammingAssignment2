##The following 2 functions are provided below: 
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

##makeCacheMatrix is a function that creates a special matrix which is a function that 
##sets the value of the matrix
##gets the value of the matrix
##sets the value of the inverse of the matrix using solve function
##gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##cacheSolve calculates the inverse of the matrix above and sets the value of the inverse 
##in the cache via the setinverse function. 
##It will first check to see if the inverse has already been calculated.
##If the inverse was calculated, it will be retrieved from the cache and skips computation.
##otherwise, it will proceed to solve the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
