## The first function, creates a special "matrix", which is
## really a list containing a function to (a) set the value of 
## the matrix, (b) get the value of the matrix, (c) set the value 
## of the inverse and (d) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {  # creating a matrix
    m <- NULL  # setting it to NULL
    set <- function(y) {  # function to assign the new parameter
        x <<- y  # sets x as y in parent environment
        m <<- NULL  # reset it to NULL if a new matrix is set
    }
    get <- function() x  # retrieve the matrix
    setInverse <- function(inverse) m <<- inverse  # assign in 
    # parent environment
    getInverse <- function() m  # give the value of m when called
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)  #necessary for $ to work
}
## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets it from
## the cache and skips the computation. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache via
## the setInverse function.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()  # call the getInverse function
    if(!is.null(m)) {  #check if it exists in the cache and return it
        message("getting inversed matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)  # calculate and set the inverse
    m
}