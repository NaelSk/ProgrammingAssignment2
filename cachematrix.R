## Put comments here that give an overall description of what your
## functions do
#1st "makeCacheMatrix" creates a special ""matrix"", which is really a list containing a function.
#2nd "cacheSolve" calculates the inverse of the special ""matrix"" created with the "makeCacheMatrix" function.


## Write a short comment describing this function "makeCacheMatrix".
#This function, "makeCacheMatrix" creates a special ""matrix"", which is really a list containing a function to:
#1.set the value of the Matrix 
#2.get the value of the Matrix
#3.set the value of the inverse Matrix
#4.get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        RevMatrix <- NULL
        set <- function(y) {                                 #1.set the value of the Matrix  
                x <<- y
        InvMatrix <<- NULL
        }
        get <- function() x                                  #2.get the value of the Matrix
        setInvMatrix <- function(solve) InvMatrix <<- solve  #3.set the value of the inverse Matrix
        getInvMatrix <- function() InvMatrix                 #4.get the value of the inverse Matrix
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)

}


## Write a short comment describing this function
###The following function calculates the inverse of the special ""matrix"" created with the above function.

cacheSolve <- function(x, ...) {      # Return a matrix that is the inverse of 'x'
                                        
        InvMatrix <- x$getInvMatrix()                 
        if(!is.null(InvMatrix)) {      #First checks to see if inverse of the special ""matrix"" has already been calculated
                message("getting cached data")   #If so, it gets the inverse from the cache and skips the computation.
                return(InvMatrix)
        }else{data <- x$get()                   
        InvMatrix <- solve(data, ...)     #Otherwise, it calculates the inverse of the data
        x$setInvMatrix(InvMatrix)         # sets the value of the inverse in the cache via the setInvMatrix function.
        return(InvMatrix)
        }
        
}
