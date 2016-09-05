## [Put comments here that describe what your functions do]

makeCacheMatrix <- function(x = matrix()) {
    #create a null vector so first run ensures a new inverse matrix is created and cached
    m<-NULL
    
    #now a set of functions used by cache solve that will be carried in the list within the environment of the  instantiated objet.
    #first the seter fucntions called by 'objectname'$set('new matrix')
    
    set<- function(y){
        #if time experiment with an if function to establish Y is differnet to x if not use cache
        # at the moment any matrix by $set will reset m ... even if it's the same as the original
        # so not really caching to save processing time.
        x <<- y # ensure x is available in parent environment as in nested fucntion
        m <<- NULL #ensure m is available in the parent environment as in nested function
    }
    
    ## get just provides the matrix for cachSolve to operate on
    get <- function()x
    
    ## when called in cacheSolve this function will take the inverse matrix and allocate
    ## it to m the <<- make sure that it becomes available in the environment of the instatiating object
    setImatrix <- function(matrix) m <<- matrix
    
    ## getImatrix will carry the results m (if they exist) to allow cached data to be output
    getImatrix <- function() m
    
    ## now wrap it all up in a list to allow functions to be called by cacheSolve using
    ## the instantiated object e.g b$set('matrix in here')
    list (set = set, get = get,
          setImatrix = setImatrix,
          getImatrix = getImatrix)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## first check to see if there is already a chached copy and output that
    m <- x$getImatrix()
    if(!is.null(m)){
        message ("Here is one I made earlier")
        return(m) ## it seem that return returns the value and halts the function
    }
    ## so looks no invers matrix was stored in m so we need to calculate it
    data <- x$get() ## gets the matrix to be inverted
    ## test in here to see if matrix is invertable
    m <- solve(data)  ## inverts it
    x$setImatrix(m) ## makes m available in parent environments
    m ## returns it to the consol
    
        
}