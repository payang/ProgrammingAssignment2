## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly.
## The following pair of functions caches the inverse of a matrix.

## Assumption: *** The matrix supplied is always invertible ***


# This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #embedded functions
    m <- NULL
    set <- function(y) {        #assigns source matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x         #read source matrix
    setmatrix <- function(solve) m <<- solve #store inverse in cache
    getmatrix <- function() m   #peek inside cache
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix) #create list
}


#This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not changed), then
#the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getmatrix()      #attempt to read cache
    if(!is.null(m)) {       #check content of cache, if not null then proceeds
        message("getting cached data") #output confirmation cache is used
        return(m)           #exit function and outputs inverse matrix
    }
    matrix <- x$get()       #load source matrix
    m <- solve(matrix, ...) #calculate inverse matrix
    x$setmatrix(m)          #call function to set cache
    m                       #output inverse matrix from cache or new calculation
}



# # To test functions uncomment and run code below
# cat("\014")                 #clear console in RStudio
# m1 <- matrix(rnorm(16,2,10),4,4)         #assign matrix to m1
# m1                          #display source matrix
# m2 <- makeCacheMatrix(m1)   #create cached matrix 
# m2$get()                    #confirms source matrix used
# m2$getmatrix()              #peek inside cache
# m2$set(m1)                  #sets value of matrix to be inverted
# m2$getmatrix()              #peek inside cache
# cacheSolve(m2)              #resolve inverse and display
# cacheSolve(m2)              #resolve inverse with confirmation cache was used 
# m2$getmatrix()              #peek inside cache, now shows value
# round(m1 %*% m2$getmatrix())       #check multiplication returns identity matrix