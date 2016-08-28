
###1 makeCacheMatrix: This function creates a special "matrix" object than can cache its inverse.
        
        #Set the value of the matrix
        makeCacheMatrix <-function (x=matrix()){
                m <- NULL
                set <-function(y){
                        x <<-y
                        m <<- NULL
                }
        #Get the value of the matrix
        get <-function()x
        
        #Set the value of the inverse matrix
        setinvmatrix <-function(solve) m<<-solve
        
        #Get the value of the inverse matrix
        getinvmatrix <-function()m
        list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
        }
        
###2 CacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.
        
        CacheSolve <- function (x, ...) {
                m <- x$getinvmatrix()
                if(!is.null(m)){
                        message ("getting cached data")
                        return (m)
                }
                data <-x$get()
                m <- solve(data, ...)
                x$setinvmatrix(m)
        }