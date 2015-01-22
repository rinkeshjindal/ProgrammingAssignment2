# makeCacheMatrix function  
makeCacheMatrix <- function(x = matrix()) { 

  inv <- NULL 

  set <- function(y) {   # set the value of the matrix
	x <<- y 
	inv <<- NULL 
    } 

    get <- function() x  # get the value of the matrix
    
    setinverse <- function(inverse) {
    	inv <<- inverse  # set the value of inverse of the matrix
    } 
    
    getinverse <- function() inv   # get the value of inverse of the matrix
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 


# This function assumes that the matrix is always invertible. 
cacheSolve <- function(x, ...) { 

    inv <- x$getinverse() # if the inverse has already been computed. 

    if(!is.null(inv)) { 
        message("getting cached data.") 
        return(inv)       #retun already computed data. Skips the computation. 
    }
 
    data <- x$get() 

    inv <- solve(data)    # if the inverse was NOT already been computed.
 
    x$setinverse(inv)
 	  
    inv 		  # return the computed result.
} 
