#Pair of functions which together calculate the inverse of matrix if it has not previously been computed and caches
#the value in a list to call later or returns the cached inverse matrix from the list if the inverse has been computed.

#The makeCacheMatrix function creates a list of functions which sets a matrix if specified, returns the argument 
#matrix, sets xInv to inverse matrix of function argument and returns the value of the inverse of the argument matrix.

makeCacheMatrix <- function(x = matrix())
{
     xInv <- NULL #Clears xInv of any previous values
     
     set <- function(y) #If called, function sets a new matrix and clears the global value of xInv
     {
          x <<- y
          xInv <<- NULL
     }
     
     get <- function() x #Returns the argument matrix
     
     setInv <- function(solve) xInv <<- solve #Sets xInv to inverse of argument matrix
     getInv <- function() xInv #Returns the value of the inverse matrix specified in the function argument
     
     list(set = set, get = get, setInv = setInv,
          getInv = getInv) #Creates list of defined functions
}


#The cacheSolve function checks to see if inverse of matrix has been calculated and returns the value if so,
#otherwise the function computes the inverse and stores the inverse matrix in x$setInv.

cacheSolve <- function(x)
{
     xInv <- x$getInv()
     
     if(!is.null(xInv)) #Prints cached inverse if it has been previously calculated.
     {
          message("Getting cached inverse...")
          return(xInv)
     }
     
     Matrix <- x$get() #If no cached matrix present, sets Matrix to the argument matrix from makeCacheMatrix function
     xInv <- solve(Matrix) #Computes inverse of Matrix
     x$setInv(xInv) #Passes xInv to setInv in the list created by makeCacheMatrix function
     
     xInv #Prints inverse matrix
}