## The purpose of the below functions is to cache the inverse...
## ...of a matrix, ands avoid repeated computations unless...
## ...necessary, since computing the inverse of a matrix is...
## ...an operation which can consume a lot of time.

## This function creates a special "matrix" object that can...
## ...cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse){
        m <<- inverse
    }
    
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## This function computes the inverse of the special "matrix"... 
## ...returned by makeCacheMatrix above. If the inverse has... 
## ...already been calculated (and the matrix has not changed), ...
## ...then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


## For this assignment, I have assumed that the matrix supplied...
## ...is always invertible - i.e. the matrix isn't singular.



##########SAMPLE COMPUTATIONS USING THE ABOVE FUNCTIONS##########

#####Computations on a 2X2 MATRIX
mat <- matrix(1:4, 2, 2)
im <- makeCacheMatrix(mat)
cacheSolve(im)
im$get()
im$getinv()
im$set(matrix(1:4,2,2,byrow = TRUE))
cacheSolve(im)


####COMPUTATIONS ON A SINGULAR MATRIX####
im$set(matrix(1:9,3,3))
cacheSolve(im)
## Error, since the matrix is not invertible.


####COMPUTATIONS ON A 3X3 MATRIX####
# 1. Using our CacheMatrix Functions
im$set(matrix(c(1:8,12),3,3))
cacheSolve(im)

# 2. Directly using the Solve Function
a <- matrix(c(1:8, 12), 3, 3)
solve(a)



