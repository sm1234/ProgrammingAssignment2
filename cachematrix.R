## There are two functions defined in this R source file
## (1)makeCacheMatrix : This function exposes four items that are used to
##			(i) set the matrix
##			(ii) get the matrix
##			(iii) set the Inverse of a matrix
##			(iv) get the Inverse of a matrix

## (2)cacheSolve : This function takes an argument of type makeCacheMatrix (a list that has
## four different methods descibed above). It first checks the cache for inverse of matrix defined in
## that variable. If found, it returns that else, it would compute the inverse of that matrix and store that
## in the cache


## Accepts a matrix, and returns a list that has four tags (setM,getM,setInv,getInv)

makeCacheMatrix <- function(x = matrix()) {

	inverseX<-NULL ## This is the variable that stores inverse of the matrix. Set that initially to NULL

	##Use to get back the matrix for which inverse has to computed
	getMatrix<-function()
	{
		return(x)
	}

	##Use to set the matrix for which inverse has to computed
	setMatrix<-function(y)
	{
		##Store the variable passed to x (defined in the environment other than currently executing
		## environment. This is done by using <<-)
		x<<-y

		##Reset inverse variable (inverseX) because matrix has been updated above
		inverseX<<-NULL
	}

	##Returns inverse variable (inverseX)
	getInverse<-function()
	{
		return(inverseX)
	}

	##Sets inverse variable (inverseX)	
	setInverse<-function(y)
	{
		inverseX<<-y
	}

	##Return the list with for tags
	return(list(setM=setMatrix,
				getM=getMatrix,
				setInv=setInverse,
				getInv=getInverse))


}


## This function takes an argument of type makeCacheMatrix and returns inverse of the 
## matrix stored in that variable

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x$getM'

        ##Get the currently stored inverse value
        m <- x$getInv()
        ##If the current inverse value is NULL, then we need to compute the inverse
        ##else use cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ##This would be executed only if nothing was found in cache
        ## In this case, get the matrix
        data <- x$getM()

        ##do an inverse of the matrix
        m <- solve(data, ...)

        ##Store the result of the inverse operation in cache
        x$setInv(m)
        
        ##return the inverted value
        m        
}
