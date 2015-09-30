## Put comments here that give an overall description of what your
## functions do

## This function provide get and set methods so any function caller
## can store a matrix as a cache and can also retrieve a cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setreverse <- function(reverse) m <<- reverse
  getreverse <- function() m
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
}


## This function return a reverse of a matrix. Before calculating reverse, 
## it check whether the matrix has a cached reverse. If there is a cached reverse matrix, 
## the cached reverse matrix is returned directly; otherwise, calculate its reverse and store it 
## in cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getreverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setreverse(m)
  m
}

## following is the testing I did. And I can see the cached data is retrieved at the second time. 
##
## > x <- stats::rnorm(16)
## > dim(x) <- c(4, 4)
## > x
##             [,1]       [,2]       [,3]       [,4]
## [1,]  0.08861144 -2.8391800 -0.4943875 -0.9616395
## [2,] -1.35368867 -1.5895502  0.8448781  0.5952274
## [3,]  1.16084696 -0.6797727  0.5097315  0.5748735
## [4,] -0.30052878 -1.4622331 -0.3780627  1.8224042
## > m <- makeCacheMatrix(x)
## > m
## $set
## function (y) 
## {
##     x <<- y
##     m <<- NULL
## }
## <environment: 0x00000000133723e8>
## 
## $get
## function () 
## x
## <environment: 0x00000000133723e8>
## 
## $setreverse
## function (reverse) 
## m <<- reverse
## <environment: 0x00000000133723e8>
## 
## $getreverse
## function () 
## m
## <environment: 0x00000000133723e8>
## 
## > m$get()
##             [,1]       [,2]       [,3]       [,4]
## [1,]  0.08861144 -2.8391800 -0.4943875 -0.9616395
## [2,] -1.35368867 -1.5895502  0.8448781  0.5952274
## [3,]  1.16084696 -0.6797727  0.5097315  0.5748735
## [4,] -0.30052878 -1.4622331 -0.3780627  1.8224042
## > r <- cacheSolve(m)
## > r
##             [,1]        [,2]       [,3]        [,4]
## [1,]  0.05830237 -0.28956365  0.5101137 -0.03557321
## [2,] -0.23970970 -0.09887263 -0.1122211 -0.05879578
## [3,] -0.19966479  0.54370441  0.5325275 -0.45092588
## [4,] -0.22414113 -0.01429030  0.1045537  0.40213793
## > r <- cacheSolve(m)
## getting cached data
## > r
##             [,1]        [,2]       [,3]        [,4]
## [1,]  0.05830237 -0.28956365  0.5101137 -0.03557321
## [2,] -0.23970970 -0.09887263 -0.1122211 -0.05879578
## [3,] -0.19966479  0.54370441  0.5325275 -0.45092588
## [4,] -0.22414113 -0.01429030  0.1045537  0.40213793
## 





