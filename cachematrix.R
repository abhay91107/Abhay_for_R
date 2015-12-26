# Assinment 2: Matrix Data workings
# Name: Abhay Kumar Jha
# user ID: 14791813
# Email:abhay_91107@yahoo.com.au"

# The following function, 'makeVector' creates a special vector. 
# The purpose of this special vector is to (1) set and obtain the value of the vector, and (2) set and obtain the value of the mean.
makeVector = function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# The following function calculates the mean of the special vector created from the special function 'makeVector'.
# The following function checks to see if the mean has already been calculated. It is done in the following steps:
# (1) It first checks to see if the mean has already been calculated. In this event, it gets the eman from the function 'catchemean'.
# (2) If the mean is not calculated in the function, 'catchemean', it calculates the mean of the data and sets the value of the mean in the function 'catchemean' via the setmean function.

cachemean = function(x, ...) {
  m = x$getmean()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#Obtaining the source of the data for this assignment
source("ProgrammingAssignment2/cachematrix.R")

#Testing the functions, (1) makeVector and (2)cachemean below:

#Setting the vector variable 'myVector' to the values created using the function, 'makeVector'
myVector <- makeVector(c(1,2,3))

#Running and obtaining the ouput of the values of the vector variable, 'myVector'.
myVector$get()

#Running and obtaining the inverse ouput of the values of the vector variable, 'myVector'.
myVector$getmean()

#Running the casheSolve function for the vector, 'myVector'. 
cachemean(myVector)

#Running the casheSolve function for the vector, 'myVector'.
cachemean(myVector)

#Running and obtaining the inverse ouput of the values of the vector variable, 'myVector'.
myVector$getmean()







#----------------------------------------------------------TESTING THE MATRIX FUNCTION AND VARIABLES------------------------------------------------------------------


# The following function, 'makeCacheMatrix' creates a special matrix. 
# The purpose of this special matrix is to (1) set and obtain the value of the matrix, and (2) set and obtain the value of the inverse.

makeCacheMatrix <- function(x = matrix()) { 
  c_Inverse <- NULL 
  set <- function(y) { 
    x <<- y 
    c_Inverse <<- NULL 
  } 
  get <- function() x 
  setInverse <- function(inverse) c_Inverse <<- inverse 
  getInverse <- function() c_Inverse 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
} 


# The following function checks to see if the matrix inverse has already been calculated.It is done in the following steps:
# (1) It first checks to see if the matrix inverse has already been calculated. In this event, it gets the matrix inverse from the function 'makeCacheMatrix'.
# (2) If the matrix inverse is not calculated in the function, 'makeCacheMatrix', it calculates the matrix inverse of the data and sets the value of the inverse in the function 'makeCacheMatrix' via the setInverse function.


cacheSolve <- function(x, ...) { 
  inverse_Function <- x$getInverse() 
  if(!is.null(inverse_Function)) { 
    message("getting cached data") 
    return(inverse_Function) 
  } 
  data <- x$get() 
  inverse_Function <- solve(data, ...) 
  x$setInverse(inverse_Function) 
  inverse_Function 
} 

# Obtaining the source of the data for this assignment
source("ProgrammingAssignment2/cachematrix.R")


# Testing the functions, (1) makeCacheMatrix and (2)cacheSolve below:

# Setting the matrix variable 'myMatrix' to the values created using the function, 'makeCacheMatrix'
myMatrix <- makeCacheMatrix(matrix(85:88, 2, 2))

# Running and obtaining the ouput of the values of the matrix variable, 'myMatrix'.
myMatrix$get()

# Running and obtaining the inverse ouput of the values of the matrix variable, 'myMatrix'.
myMatrix$getInverse()

# Running the casheSolve function for the matrix, 'myMatrix'. 
cacheSolve(myMatrix)

# Running the casheSolve function for the matrix, 'myMatrix'.
cacheSolve(myMatrix)

# Running and obtaining the inverse ouput of the values of the matrix variable, 'myMatrix'.
myMatrix$getInverse()

# Setting the matrix variable 'myMatrix' to the values created using the function, 'makeCacheMatrix'
myMatrix$set(matrix(c(7, 9, 11, 39), 2, 2))

# Running and obtaining the ouput of the values of the matrix variable, 'myMatrix'.
myMatrix$get()

# Running and obtaining the inverse ouput of the values of the matrix variable, 'myMatrix'.
myMatrix$getInverse()

# Running the casheSolve function for the matrix, 'myMatrix'. 
cacheSolve(myMatrix)

# Running the casheSolve function for the matrix, 'myMatrix'. 
cacheSolve(myMatrix)

# Running and obtaining the inverse ouput of the values of the matrix variable, 'myMatrix'.
myMatrix$getInverse()

