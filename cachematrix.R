##The first function called makeCacheMatrix, initialize the 
##functions list  set, get, setinv and getinv.
##In the "set" function, there is a validation to check if the 
##matrix that is being set is the same one as the previous one,
##in case it is the same one, the matrix is not set and the inv is not 
##set to NULL.
makeCacheMatrix <- function(x = matrix()) {
        ##The inv cached variable holds the value of the matrix inverse.
        ##When the fuction is first initialized, the inv is set to NULL.
        inv <<- NULL
        ##Every time the matrix is set, I'm checking if the matrix is the first one
        ##being set, and if not it also checks if the previous one was equal or different.
        set <- function(y){
                if (identical(x,y) == FALSE) {
                        x <<- y
                        inv <<- NULL
                }
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##cacheSolve function, returns the inverse of the matrix
##from cache if the invers has already been calculated,
##and calculates it if the inverse has not been calculated
##or if the one in cache is for a different matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
