## Put comments here that give an overall description of what your
## functions do

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    theInverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        theInverse <<- NULL
    }
    setInverse <- function(solve) theInverse <<- solve
    getMatrix <- function() x
    getInverse <- function() theInverse
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInv <- x$getInverse()
    if(!is.null(matrixInv)) {
        message("retrieved cached data")
        return(matrixInv)
    } else {
        message("calculating the inverse")
        theMatrix <- x$getMatrix()
        matrixInv <- solve(theMatrix)
        x$setInverse(matrixInv)
        return(matrixInv)
    }
}

## 'testCacheMatrix': This function tests the operation of the makeCacheMatrix 
## and cacheSolve functions, while providing an example of their usage
testCacheMatrix <- function() {
    #test matrices to be used for comparison
    myMatrix <- matrix(c(5,25,7,49), nrow = 2, ncol = 2)
    myInverse <- solve(myMatrix)
    
    #construct the special matrix object
    matrixCacheObj <- makeCacheMatrix(
        matrix(c(5,25,7,49), nrow = 2, ncol = 2))
    
    #test1: does the matrix in the matrixCacheObj == myMatrix?
    if (!identical(myMatrix, matrixCacheObj$getMatrix())) {
        message("Test 1: does the matrix in the matrixCacheObj == myMatrix? NO")
        return(NULL)
    }
    
    #using the cacheSolve function, calculate the inverse of the matrixCacheObj
    i <- cacheSolve(matrixCacheObj)
    
    #test2: Does the matrix returned by cacheSolve return the same inverse as
    # was returned by myInverse?
    if (!identical(myInverse, i)) {
        message("Test 2: do the matrix inversions match? NO")
        return(NULL)
    }
    
    #test3: does the inverse matrix stored in matrixCacheObj not equal NULL?
    if (is.null(matrixCacheObj$getInverse())) {
        message("Test 3: does the inverse matrix stored in matrixCacheObj not equal NULL? NO")
        return(NULL)
    }
    
    #test4: does running cacheSolve a second time return the same inverse?
    i2 <- cacheSolve(matrixCacheObj)
    if (!identical(i2, i)) {
        message("Test 4: does running cacheSolve a second time return the same 
                inverse? NO")
        return(NULL)
    }
    
    #test5: if I create a new matrix, does it show that the inverse is null
    #before calculating it
    matrixCacheObj2 <- makeCacheMatrix(
        matrix(c(10,100,14,196), nrow = 2, ncol = 2))
    
    if (!is.null(matrixCacheObj2$getInverse())) {
        message("Test 5: Is the new matrix inverse NULL? NO")
        return(NULL)
    }
    
    #test6: does the new inverse equal the old inverse?
    i3 <- cacheSolve(matrixCacheObj2)
    if (identical(i3, i)) {
        message("Test 6: does the new inverse differ from the old inverse? NO")
        return(NULL)
    }
    
    
    message("TEST PASSED")
}
## 'testCacheMatrixPerformance': a silly test that generates a 1000 x 1000 
## matrix using rnorm to populate. This then calculates the time to generate
## an inverse and then compares that to the time to fetch the inverse. Fetching
## shoul take as close to 0 as possible.
testCacheMatrixPerformance <- function() {
    #create a large matrix
    largeMatrix <- matrix(rnorm(1000000, mean = 5), nrow = 1000, ncol = 1000)
    myLargeMatrix <- makeCacheMatrix(largeMatrix)
    
    #calculate the inverse
    s <- proc.time()
    i <- cacheSolve(myLargeMatrix)
    e <- proc.time() - s
    message("Time to calulate the inverse: ", e[["elapsed"]])
    
    #retrieve the already calculated inverse
    s <- proc.time()
    i2 <- cacheSolve(myLargeMatrix)
    e2 <- proc.time() - s
    message("Time to fetch the cached inverse: ", e2[["elapsed"]])
    
    if (identical(i, i2)) {
        message("The calculated and cached inverses are identical")
        message("Time difference in calculating vs. retrieving cached data is ",
                round(e[["elapsed"]] - e2[["elapsed"]],2), "s")
    } else {
        message("Something is wrong, the inverses are not identical")
    }
}