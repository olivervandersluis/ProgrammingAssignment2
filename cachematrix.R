## 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix)
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



## This function is called to 

cacheSolve <- function(x,...)
{
        ## Return a matrix that is the inverse of 'x'
        ## if the function is called by default the inverse is set to NULL and the variable matrix is set to the parameter x
        
    matrix <- x
    inverse <- NULL
    
    mtrxfn <- makeCacheMatrix()
    
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

