## This function, really a pair of functions, caches the inverse of a matrix. 

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to set the values of the matrix, get the matrix, set the inverse matrix and get the inverse matrix.



makeCacheMatrix <- function(x = matrix()) 
{
    inv<- NULL
        
    set<- function(y)
    {
        x<<- y
        inv<<- NULL
    }
    
    get<- function() {x}
        
    setInv<- function(inverse)
    {
        inv<<- inverse
    }
    
    getInv<- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
}


## cacheSolve is a function that takes in a matrix. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
     inv<- x$getInv()    #returns the inverse of x and stores it in the variable inv
    
    if(!is.null(inv))    #check if inverse of x has already been calculated
    {
        message("getting cached data")
        return(inv)
    }
    
    theMatrix<- x$get()      
    inv<- solve(theMatrix, ...)          #computes the inverse of x (since it has not already been done) and caches it. 
    x$setInv(inv)
    inv 
}
