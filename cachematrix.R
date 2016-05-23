## Creates a vector which is a list that contains functions. 

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        #creates a function to set the value of the matri .
        
        set <- function(y) {            
                x <<- y
                
                # initialize the value of the inverse.
                inv <<- NULL
        }
        
        #creates a fuction to get the value of the matrix.
        
        get <- function() x
        
        #creates a fuction to set the value of the inverse matrix.
        
        setinv <- function(solve) inv <<- solve
        
        #creates a fuction to get the value of the inverse matrix.
        getinv <- function() inv
        
        #make a list which contains the above vector.
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'.
        inv <- x$getinv()
        
        # if the inverse matrix is already calculated, return the cashed value.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #get the value of matrix.
        data <- x$get()
        
        #set the value of inverse matrix.
        inv <- solve(data, ...)
        
        #get the value of inverse matrix.
        x$setinv(inv)
        
        #print or return the value of inverse matrix.
        inv

}

#make a matrix object to use as an input to cacheSolve.
a <- makeCacheMatrix(matrix(c(-1,-2,1,1), 2,2))

#trigger cacheSolve.
cacheSolve(a)

#make a matrix itself to use as an input to cacheSolve.
cacheSolve(makeCacheMatrix(matrix(c(-1,-2,1,1), 2,2)))

#trigger cacheSolve second time to see "getting cached data" 
#and confirm that the cached value of inverse is called.

cacheSolve(a)

#try non-invertible matrix to invert.

c<- makeCacheMatrix(matrix(c(0,0,0,0), 2,2))