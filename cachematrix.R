## Programming Assignment 2: Lexical Scoping

## The following code is developed to create a 2 functions that cache the
## inverse of a matrix

## Assumptions
## The supplied matrix to test the code will always be invertible


## The function "makeCacheMatrix will create an special "matrix" 
## objetc that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- solve
        getMatrix <- function() m
        list(set = set, get =get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## The function "chacheSolve" will compute the inverse of the special "matrix"
## returned by the function "makeCacheMatrix". If the inverse has already been
## calculated -and the matrix has not changed-, then the cachesolve should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        m <- x$getMatrix()
        if (!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m<-solve(data)
        x$setMatrix(m)
        m
}

## Test code

## First create a square matrix invertible
## testMatrix <- rbind(c(3,2,5),c(2,3,2),c(5,2,4))
## print (testMatrix)

## Calculate the inverse with cache solve
## print("Inverse computed:")
## testCache <- makeCacheMatrix(testMatrix)
## testInverse <- cacheSolve(testCache)
## print(testInverse)

## Test the cache
## print("Inverse from cache:")
## testInverse2 <- cacheSolve(testCache)
## print(testInverse2)
