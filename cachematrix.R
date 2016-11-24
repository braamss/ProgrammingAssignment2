## Two functions that are caching the inverse of a matrix 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        set_inv <- function(inv_matrix) inv <<- inv_matrix
        get_inv <- function() inv
        list(set=set,
             get=get,
             set_inv=set_inv,
             get_inv=get_inv)

}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$set_inv(inv)
        inv

        ## Return a matrix that is the inverse of 'x'
}
