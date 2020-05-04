## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        # This function sets The value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        #T his function gets the value of the matrix.
        get <- function() x
        
        # This function sets the value of the inverse.
        setInverse <- function(inv) inverse <- inv
        
        # This function gets the value of the inverse.
        getInverse <- function() inverse
        
        #Returned list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solution <- x$getInverse()
        
        if(!is.null(solution)) {
                message("getting cached data")
                return(solution)
        }
        
        my_matrix <- x$get()
        shape <- dim(my_matrix)
        
        if(shape[1] != shape[2]) {
                message("Inverse must be perform on square matrix.")
                return
        } 
        
        solution <- solve(my_matrix, ...)
        x$setInverse(solution)
        
        solution
}
