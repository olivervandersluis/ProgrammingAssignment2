## the functions below 
## functions do

## this functions contains 4 functions which get and set the matrix and its inverse in the cache
## The function "makeCacheMatrix" (and its functions) is called by the cacheSolve in order to retrieve the cached
##matrix inverse, or  to set the matrix and its inverse
makeCacheMatrix <- function(x = matrix)
{
    ##this function sets the matrix to the cache
    setMatrix <- function(x) Cachematrix <<- x
    
    ##this function gets the matrix from the cache, if it exists else it returns NULL
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
    
    ##this function sets the inverse matrix in the cache
    setInverse <- function(inverse)  CacheinverseMatrix <<- inverse
                                     
     ##this function gets the inverse matrix from the cache
    getInverse <- function()  CacheinverseMatrix

    ## this list is created in order to call the functions within "makeCacheMatrix"
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## this function is used to compute the inverse of a matrix.
## if the inverse of a matrix already exists in the cache, the cached inverse is used
## in the other scenario in which no inverse is cached or the inverse of a different matrix is cached, the 
## inverse is computed with the solve() fucntion
cacheSolve <- function(x,...)
{
    
    ##the default valie of the variable matrix is set, as is the inverse
    
    matrix <- x
    inverse <- NULL
    
    ##mtrxfn is a variable that contains the functions in "makeCacheMatrix"
    mtrxfn <- makeCacheMatrix()
    
    ##the cached matrix is retrieved; if no cached matrix exists, NULL is returned
    GetMatrix <- mtrxfn$getMatrix()
    
    ##this statement checks if the retrieved matrix from the cache(see "GetMatrix <- mtrxfn$getMatrix()")
    ## is idetentical to the matrix from which the inverse should be computed
    if(identical(GetMatrix, x))
    {
        ## if both the parameter matrix and the cached matrix are identical, the inverse matrix from the cache
        ## is called
        return (mtrxfn$getInverse())
    }
    else
    {
        ## if both are not identical, the inverse matrix is computed with the solve() funtion.
        ## both the matrix and its inverse are set to the cache. 
        
        inverse <- solve(matrix)
        mtrxfn$setMatrix(matrix)
        mtrxfn$setInverse(inverse)
        return(inverse)
    }
    
    
}

