## The makeCacheMatrix and cacheSolve functions together
## provide a way to compute the inverse of a matrix, only
## when none has been computed before else use a cached value.

## The makeCacheMatrix function creates a special matrix which can cache 
## the inverse of itself.
makeCacheMatrix <- function(m= matrix()) {
    inv <- NULL
    set <- function(y){
        m <<- y
        inv <<-NULL
    }
    get <- function() m
    getinv <- function() inv
    setinv <- function(i) inv <<- i
    list(set=set,get=get,getinv=getinv,setinv=setinv)
}


## The cacheSolve function computes the inverse of the matrix only
## when none has been computed before else uses the cached value.
cacheSolve <- function(cm, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- cm$getinv()
	    if(!is.null(inv)){
	        message("getting cached inverse")
	        return(inv)
	    }
	    m<-cm$get()
	    inv<-solve(m)
	    cm$setinv(inv)
	    inv
}
