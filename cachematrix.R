## There are two functions, one stored in the "makeCacheMatrix" variable, while the second function is stored in
## "cacheSolve" variable. Simply put, these two functions in conjunction can calculate the inverse of a matrix
## and store the resulting inverse in cache. If the cache exists for a certain matrix's inverse, the functions
## will simply print out the result from the cache rather than performing the inverse calculation again.

## The function in makeCacheMatrix is a special function that will result in a list that has 4 objects and 2 variables.
## The 4 objects are functions, while the 2 variables are "invers" which stores the matrix inverse, and "x" which is
## the actual matrix inputted by the user.
## Functions in makeCacheMatrix are: 1) set: which sets values into the variable "x" and "invers", 2) get: which gets
## the value of x in its parent environment, 3) setInv: which sets the result of the calculation performed by the 
## cacheSolve function and store it into the setInv variable, and 4) getInv: which gets the value of the "invers"
## variable. makeCacheMatrix can't calculate the matrix inverse by itself, and acts more as a storage.

makeCacheMatrix <- function(x = matrix()) {
    invers <- NULL                                           ##initializes the "invers" variable
    set <- function(y) {                                     ##set function which will set values into "x" and "invers"
      x <<- y                   ##assigns value for all variable named x in this environment and its parent environment
      invers <<- NULL           ##assigns null value for "invers" variables in this envt. and its parent environment
    }
    get <- function() {
      x                     ##"get" variable will run a funct. that gets the value of "x" variable & store it in "get"
    }       
    setInv <- function(invResult) {
      invers <<- invResult  ##"setInv" will store the inverse value calculated by cacheSolve into the "invers" var.
    }  
    getInv <- function() {
      invers                ##"getInv" will get the value of "invers" variable
    }                         
    list(set = set, get = get, setInv = setInv, getInv = getInv)   ##creates the output list as a result
}


## cacheSolve is the function that does the actual matrix inversion calculation. First, this function will evaluate
## whether the inverse calculation for a certain matrix has been performed or not, and whether the resulting inverse
## has ever been stored in cache. If matrix inverse calculation has never been performed, cacheSolve will get the 
## matrix stored in makeCacheMatrix and calculate its inverse. Then, cacheSolve will set the inverse value into the 
## makeCacheMatrix object's "invers" variable by calling the "setInv" element of the makeCacheMatrix object. If the 
## inverse calculation has been done before, cacheSolve will simply let the user know that the inverse has been cached
## and it will print out the cached inverse.

cacheSolve <- function(x, ...) {
    invers <- x$getInv()    ##calls the getInv element of a makeCacheMatrix object and store the result in "invers"
    if(!is.null(invers)){   ##enters this branch of the funct. only if "invers" gets filled by non-Null value
      message("Getting cached data!")  ##prints out message that the subsequent value was obtained from cache
      return(invers)        ##terminates the whole function and return the value of "invers" var.
    }
    matrix1 <- x$get()      ##calls the get element of a makeCacheMatrix object and store the result in "matrix1"
    invers <- solve(matrix1, ...) ##calculates the inverse of the matrix in "matrix1"
    x$setInv(invers)        ##calls the setInv element of a makeCacheMatrix & sends the "invers" var. into the mCM obj.
    invers                  ##prints the output, which is stored in the "invers" var
}
