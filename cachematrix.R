## cachematrix.R script - two functions working together

## This script will solve for the inverse of a selected matrix, 
## cache that solution in the parent environment, 
## and then display the solved inverse matrix when cacheSolve is called.  
## If you call that specific matrix again, the function 
## will first check for the solution in the parent environment 
## and if found, will save you some memory and time by retrieving 
## the already solved invserve matrix rather than recaculating it.
## The message: "getting cached matrix" will display when a cached 
## matrix is retrieved.

## You have a matrix?  Any matrix!  Give it a symbol.
## Good.  Type it right into makeCacheMatrix like this: 
## makeCachedMatrix(yourspecialname) and hit that enter button.
## Though you should probably assign this output a name too, before
## hitting that enter button. 
makeCacheMatrix <- function(x = matrix()) {
  ##What's m?  Why is it NULL?  Means m symbol is reserved, 
  ## super special.  It's going to be your cached inverse matrix.
  m <- NULL
  set <- function(y) {
    ## Here's the <<- operator.  It is setting x and m to a different
    ## environment, outside of the makeCacheMatrix funtion/environment.
    ## You know, so they exist after this function 
    ## is done running. 
    x <<- y
    ## Here's m<<- NULL, NULL again, what the heck!  You need this, 
    ## because symbol m won't always be the same thing, right?
    ## Needs to be clear for a matrix different than your previous one.
    m <<- NULL
  }
  get <- function() x
  ## Here we see the solve funtion, finally!  I wish I had this
  ## function handy throughout HS algebra.
  setsolve <- function(solve) m <<- solve
  getsolution <- function() m
  ## This list call is just going to list what you've done so far,
  ## so you can see the environment that your cache is in.  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolution = getsolution)
  ## You don't actually get to see the solution here, that's for 
  ## the next funtion, cacheSolve!
}

## This function is called cacheSolve.  But it's only going to
## solve an assigned matrix for it's inverse once! If it's already
## seen that symbol/ matrix object pair, it's going to be like:
## 'Pfft, you mean this inverse matrix?  I like, totally already
## did that, here.'  You'll know when you see the much more polite
## message:'getting cached matrix'. 

## Start by calling your output from the first function; I hope you
## gave it a name.  Ex: cacheSolve(outputFrom1stThing)
cacheSolve <- function(x, ...) {
  ## Yeah, go get that inverse matrix solution
    m <- x$getsolution()
    ## But wait, if cached m is not NULL
    if(!is.null(m)) {
      ## display this cranky, "I alreay did that" message
      message("getting cached matrix")
      ## and give them the thing.
      return(m)
    }
    ## Oh, it's the first time I've seen this matrix? 
    data <- x$get()
    ## Well, I better do as you asked
    m <- solve(data, ...)
    ## and solve for that inverse matrix.  I cached it for later
    ## don't worry
    x$setsolve(m)
    m
  }
## After this you can finally see your solved, inverse matrix.

##  I may or may not know what I'm talking about, in truth,
## being fairly new to [R].  But I hope I understood the steps
## correctly and explained them well enough.  If any [R] gurus
## happen to peer-review this, I would appreciate any time 
## you might take to correct my views on this bit of code.
## Thanks for reading!
