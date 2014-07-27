## These functions allow the time consuming computation of inverting
## a matrix to be calculated and the result cached. In future
## iterations of the function the cahce is checked to see if the
## inverse of the matrix had been previously calculated. If it had
## the result is retrieved thus saving time.

## This function creates a special "matrix" object that can cache 
## its inverse. The function takes an argument 'x' that is a 
## matrix object

makeCacheMatrix <- function(x = matrix()) {

        ## initializes the matrix inverse object
        i <- NULL
  
        ## this function sets the value of the matrix 
        set <- function( matrix ) {
                x <<- matrix
                i <<- NULL
        }
 
        ## this function gets and returnsthe value of 
        ## the matrix object
        get <- function() {    
                x
        }
  
        ## this function sets the value of the matrix inverse
        setInverse <- function(inverse) {
                i <<- inverse
        }
  
        ## this function gets and returns the inverse of the matrix
        getInverse <- function() {
                i
        }
  
        ## Creates a list of functions 
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
     
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {

        ## imports the MASS package in order to use ginv
        library('MASS')
        
        ## calls the getInverse function in the makeCacheMatrix
        ## function on 'x' and stores the retruned values in
        ## 'i'
        i <- x$getInverse()
  
        ## If the inverse has already been calculated the result
        ## is returned
        if( !is.null(i) ) {
                message("getting cached data")
                return(i)
         }
  
         ## gets the matrix to be evaluated (by calling the 'get'
         ## function in the makeCacheMatrix function and stores 
         ## it in the variable 'data'
         data <- x$get()
  
         ## calculate the inverse of the input using using
         ## the 'ginv' pseudoinverse function from the
         ## MASS package
         i <- ginv(data)
  
         ## sets the inverse of the matrix by calling the 
         ## setInverse function in the makeCacheMatrix function
         x$setInverse(i)
  
         ## returns the inverted matrix
         i
}
