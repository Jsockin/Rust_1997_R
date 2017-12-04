#
#   Calculates EV(x) using the inclusive value.
#

calcEV <- function(x,beta,theta,gridX,gridEV,aVals){
  
    delta <- rep(0,length(aVals))  
    currLoc <- x+1
    
    # Calculate delta_0(x_t) (no replacement)
    if (currLoc < length(gridX)) {
        delta[1]       <- -cost(x,theta) + beta * ( gridEV[currLoc+1]*(1-theta[3]) + gridEV[currLoc]*theta[3] ) 
    } else {
        delta[1]       <- -cost(x,theta) + beta * gridEV[currLoc]*theta[3]
    }
    
    # Calculate delta_1(x_t) (replacement)
    delta[2]           <- -theta[4] + beta * ( gridEV[2]*(1-theta[3]) + gridEV[1]*theta[3] ) 
    
    # Calculate inclusive value
    result <- log(exp(delta[1])+exp(delta[2]))
    return(result)
}
