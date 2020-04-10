## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvert <- function(inverse) inv <<- inverse
        getinvert <- function() inv
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
        
}


## Write a short comment describing this function
## This function takes the data from the special "matrix" from above and
## inverts the data from the chace

cacheSolve <- function(x, ...) {
        
        inv <- x$getinvert()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinvert(inv)
        inv
}


### For convenices nesting of the functions from above
invertMatrix<-function(x) cacheSolve(makeCacheMatrix(x))

# Return a matrix that is the inverse of 'x'
my_matrix<-cbind(c(2,2),c(1,4))##just a small example 

invertMatrix(my_matrix)# gives the inverted matrix