## Generates a matrix object and caches its inverse
## Retrieves the inverse matrix object if it has been cached
## else generates the inverse matrix de novo

## Establish a matrix object and cache it 
makeCacheMatrix <- function(x = matrix()) {
		mat <- NULL
		setmat <- function(y) {
				x <<- y
				mat <<- NULL
		}
		get <- function() x
		setminv <- function(solve) mat <<- solve
		getminv <- function() mat
		list(setmat=setmat, get=get, 
		setminv=setminv, getminv=getminv)
}


## Retrieve cached inverse matrix or
## if empty generate inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getminv()
        if (!is.null(mat)) {
        	message("getting cached matrix")
        	return(mat)
        }
        cm <- x$get()
        mat <- solve(cm, ...)
        x$setminv(mat)
        mat
}
