## the functions below 
## functions do

## this functions contains 4 functions which get and set the matrix and its inverse in the cache
## The function "makeCacheMatrix" (and its functions) is called by the cacheSolve in order to retrieve the cached
makeCacheMatrix <- function(x = matrix)
{

    setMatrix <- function(x) Cachematrix <<- x
    
    getMatrix <- function()
    {
        if (exists("Cachematrix"))
        {
            return (Cachematrix)
        }
        else 
        {
            return(NULL) 
        }
    }  
    
    setInverse <- function(inverse)  CacheinverseMatrix <<- inverse
                                     
    
    getInverse <- function()  CacheinverseMatrix

    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function is called to find the inverse of a matrix.
## if the inverse has already been computed (the parameter 'matrix' is identical to the cached matrix)
## The inverse matrix could also be found in the cache
## if both matrices are different, the inverse is computed and stored in the cache

## storing in the cache is done by the funtion "makeCacheMatrix" above.

cacheSolve <- function(x,...)
{
    
    matrix <- x
    inverse <- NULL
    
    mtrxfn <- makeCacheMatrix()
    
    GetMatrix <- mtrxfn$getMatrix()
    
    if(identical(GetMatrix, x))
    {
        return (mtrxfn$getInverse())
    }
    else
    {
        inverse <- solve(matrix)
        mtrxfn$setMatrix(matrix)
        mtrxfn$setInverse(inverse)
        return(inverse)
    }
    
    
}

