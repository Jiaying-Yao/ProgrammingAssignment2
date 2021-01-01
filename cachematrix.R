#cache the value of the inverse so that when we need it again, it can be looked up 
#in the cache rather than recomputed.

makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(y){#set value of matrix
                x <<- y #assign y to x in a different environment than the current one
                inv <<- NULL
        }
        get <- function() x #get the matrix
        setInv <- function(new_inv) inv <<- new_inv
        getInv <- function() inv
        #return a list of 4 functions
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}

cacheSolve <- function(x, ...){#x is of a makeCacheMatrix object
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached inverse matrix")
                return(inv)
        }
        #if not cached, calculate then cache
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInv(inv)
        inv
}