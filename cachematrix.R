## Use lexical scoping to build an object that caches the inverse of a matrix


## Get and set functions for matrix object

makeCacheMatrix <- function(x = numeric()) {
	m <- NULL
      set <- function(y) {
      	x <<- y
            m <<- NULL
	}
	get <- function() x ## Get original matrix
	setinverse <- function(inverse) m <<- inverse ## Cache matrix inverse
	getinverse <- function() m ## Retrieve cached matrix inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Check to see if matrix inverse is already cached
## If cached, retrieve cache
## Else if not cached, calculate inverse and cache the result

cacheSolve <- function(x, ...) {

      m <- x$getinverse() ## Check if inverse is already cached
      if(!is.null(m)) {
		message("getting cached data")
            return(m) ## If cached, return cache
      }
      data <- x$get() ## Get original matrix
      m <- solve(data, ...) ## Else solve for inverse
      x$setinverse(m) ## Cache result
      m
}

