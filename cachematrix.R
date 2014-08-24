#------------------------------------------------------
# OBJECTIVE:
# Desired functionality is to run a processor and/or time
# intensive calculation(s) and cache it for later recall. 
# On subsequent calls, the cached value should be returned
# in lieu of recalculating the value.
#
# PROCESS:
# On first run, return the inversion of a matrix and store
# in cache.  On subsequent calls, return the cached value.
#
# ASSUMPTIONS:
# Assume that the matrix supplied is always invertible.
#
# FUNCTIONS:
# makeCacheMatrix
# cacheSolve
#
# EXAMPLE CALL:
# a <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# a$get()
# cacheSolve(a)
# a$getInverse()
#------------------------------------------------------


#------------------------------------------------------
# makeCacheMatrix
# 
# DESCRIPTION:
# Function creates a special "matrix" object that can 
# cache its inverse.
# 
# PARAMETERS:
# x     - Matrix of any size.  Must be invertible.
#------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    # Clear out already exsiting values
    m <- NULL
    
    # Set the value of x to the new value passed in
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    # Return the value of x that was set
    get <- function() { x }
    
    # Set the value of m to be the value passed in
    setInverse <- function(inverse) { m <<- inverse }
    
    # Return the value of m that was set
    getInverse <- function() { m }
    
    # Put all the functions in a list to be called
    # after the makeCacheMatrix object is instantiated
    list ( set = set
           , get = get
           , setInverse = setInverse
           , getInverse = getInverse
    )
}


#------------------------------------------------------
# cacheSolve 
# 
# DESCRIPTION:
# Function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
#
# PARAMETERS:
# The list object generated from running the makeCacheMatrix
# function
#------------------------------------------------------
cacheSolve <- function(x, ...) {
    
    # Check to see if the cached value already exists  
    m <- x$getInverse()

    # If it does, print a messsage and return that value
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    # If it does not, get the current value for the matrix
    data <- x$get()
    
    # Computing the inverse of a square matrix can be done 
    # with the solve function in R. For example, if X is a
    # square invertible matrix, then solve(X) returns its inverse.
    m <- solve(data)
    
    # Store the inverted matrix in the setInverse property of the list
    x$setInverse(m)
    
    # print out the inverse matrix
    m     
}

