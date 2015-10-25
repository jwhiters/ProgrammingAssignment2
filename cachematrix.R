
makeCacheMatrix <- function(x = matrix()) {

    ## The first function makeCacheMatrix returns a list to:
    ##      1. set the matrix
    ##      2. get the matrix
    ##      3. set the inverse
    ##      4. get the inverse
    ##  this list is used as the input to cacheSolve()
    
    inv = NULL
    set = function(y) {
        # use '<<-' to assign a value to an object in an environment 
        # different from the current environment
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
    
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)) {
            # get it from the cache and skips the computation.
            message("getting cache data")
            return(inv)
        }
        
        # otherwise calculate the inversee
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via setinv function.
        
        x$setinv(inv)
        
        return(inv)
}
