## R code for Programming Assignment 2 about caching the Inverse of a Matrix

## This function creates a set of functions that are returned and subsequently 
## accessible as elements of a list. These can be used to setup a cached matrix,
## view the matrix, set the inverse matrix of the matrix and finally view the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        InvMatrix <- NULL
        ## function which takes in a user passed matrix
        set <- function(y) {
                x <<- y
                InvMatrix <<- NULL
                print(x)
        }
        ## function can be called to view the matrix that is passed in
        get <- function() x
        ## Take whatever is passed in and put it in the cached inverse matrix
        setInv <- function(Inv) InvMatrix <<- Inv
        ## function can be called to view the cached inverse matrix
        getInv <- function() InvMatrix
        ## return the functions as elements of a list
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        InvMatrix <- x$getInv()
        if(!is.null(InvMatrix)) {
                message("getting cached matrix")
                return(InvMatrix)
        }
        message("computing inverse matrix")
        ## get the matrix passed in by user
        data <- x$get()
        ## inverse the matrix using the Solve function
        InvMatrix <- solve(data)
        ## cache the inverse matrix
        x$setInv(InvMatrix)
        ## return the inverse matrix
        InvMatrix
}
