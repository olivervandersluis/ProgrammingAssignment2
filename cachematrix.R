## the functions below 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x)
{
    #set the default values for the matrix and it's inverse
    CacheinverseMatrix <<- NULL
    Cachematrix <<- NULL
    
    #this function sets the new value of the matrix
    #it is used to compare a matrix in cacheSolve to the existing value.
    #if the value in cacheSolve is identical to the cached one
    setMatrix <- function(x) Cachematrix <<- x
    
    getMatrix <- function() Cachematrix
   
    
    setInverse <- function(inverse) CacheinverseMatrix <<- inverse
    getInverse <- function() CacheinverseMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}



## This function is called to find the inverse of a matrix.
## if the inverse has already been computed (the parameter 'matrix' is identical to the cached matrix)
## The inverse matrix could also be found in the cache
## if both matrices are different, the inverse is computed and stored in the cache

## storing in the cache is done by the funtion "makeCacheMatrix" above.

cacheSolve <- function(x = matrix)
{   
    ## if the function is called by default the inverse is set to NULL and the variable matrix is set to the parameter x
        
    matrix <- x
    inverse <- NULL
    
    ## in order to call the functions to get/set the matrix and it's inverse, the main funtion is stored in a variable
    mtrxfn <- makeCacheMatrix()
    
    ## get the chached matrix
    GetMatrix <- mtrxfn$getMatrix()
    
    if(identical(GetMatrix, x))
    {
        return (mtrxfn$getInverse(matrix))
    }
    else
    {
        inverse <- solve(matrix)
        mtrxfn$setMatrix(matrix)
        mtrxfn$setInverse(inverse)
        return(inverse)
    }
  
    
}

