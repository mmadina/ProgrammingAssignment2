## Reference : Original Program is from the content provided by Coursesra
## I just enhanced the program by making necessary changes to the base program

## Function takes matrix as input and creates a list with the options to 
## get,set,inverse the Matrix, Similar to the Vector Function provided as an example

## For Execution try below Steps :
## Create a 2x2 Matrix x <- matrix(1:4 ,ncol=2,nrow=2) 
## pass this matrix to the function and 
## assign it to variable "y" y <- makeCacheMatrix(x)
## Execute y$setinverse(solve(x)) This will calulcate the inverse of the matrix
## Execute y$getinverse() This will display the inverse on the R prompt

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
  x <<- y
  m <<- NULL
  }
  
  get <- function () x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## This Function will Cache the Inverse of matrix 
## and display's it from cache, 
## Execute cacheSolve(Y) since the Inverse is already calculated 
## it displays the message "getting cached Inverse Matrix" and gives the result


 cacheSolve <- function(x=matrix(), ...) 
   {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   
   if(!is.null(m))
     {
       message("getting cached Inverse Matrix")
    
       return(m)
     }

       mtrx <- x$get()
       
      m <- solve(mtrx, ...)
      
   x$setinverse(m)
  
   m
 }
