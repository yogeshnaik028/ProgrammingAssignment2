## makeCacheMatrix takes as it argument a matrix and returns a  list 
## of  getter and setter functions for the original and the resultant matrix
## usage : cr<- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## makeCacheMatrix use two variables x and m set in an alternative environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve takes as it argument the list produced by makeCacheMatrix
## with getter and setter functions 
## and returns an inverse matrix
## usage : cacheSolve(cr)
## sample code
## c<-rbind(c(1, -1/4), c(-1/4, 1))
## cr<-makeCacheMatrix(c)
## cacheSolve(cr)

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
      # return the cached matrix if it is not null
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
      # else take inverse the matrix, store and return it
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}

