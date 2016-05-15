## Put comments here that give an overall description of what your
## functions do

# the function "makeCacheMatrix" use scoping rules and store a matrix in memory

makeCacheMatrix <- function(mat = matrix()) {
        inverse <- NULL
        set <- function( Y ) {
                mat <<- Y
               inverse <<- NULL
        }
        #get and return the matrix
        get <- function() mat
        #set and get inverse matrix
        setinverse <- function(mat_inverse) inverse <<- mat_inverse
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Compute the inverse of the special matrix returned by "makeCacheMatrix"

cacheSolve <- function(mat, ...) {
        # Return a matrix that is the inverse of 'x'
        inverse <- mat$getinverse()
        if( !is.null(inverse) ) {
        message("getting cached data")
        return(inverse)
        }
         #get data
        data <- mat$get()
        #calculate and set the inverse
        inverse <- pseudoinverse(data, ...)
        mat$setinverse(inverse)
        #return matrix
        inverse
}

