## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
#set matrix and its value
#get matrix and its value
#set inverse of matrix
#get inverse of matrix
makeCacheMatrix <- function(samplematrix = matrix()) {
        
        m <- NULL
        set <- function(y) {
                samplematrix <<- y
                m <<- NULL
        }
        
        
        get <- function() samplematrix
        setinverse <- function(matrix_inverse)
                {
            matrix_inverse <- solve(samplematrix)
                m <<- matrix_inverse
        }
        getinverse<- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


        
        cacheSolve <- function(samplematrix, ...) {
                m <- samplematrix$getinverse()
                print(m)
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- samplematrix$get()
                m <- solve(data, ...)
                samplematrix$setinverse(m)
                m
        }
        