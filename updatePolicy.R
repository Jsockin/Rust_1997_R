#
#   Updates the policy function P(a|x) given EV(x).
#

updatePolicy <- function(beta,theta,gridX,probEV,aVals){
  
    policyA <- matrix(0,length(gridX),length(aVals))
    delta <- rep(0,length(aVals))
    
    for (currLoc in 1:length(gridX)){
      
        x <- currLoc - 1
        
        # Calculate delta_0(x_t) (no replacement)
        if (currLoc < length(gridX)) {
            delta[1]     <- -cost(x,theta) + beta * ( probEV[currLoc+1]*(1-theta[3]) + probEV[currLoc]*theta[3] ) 
        } else {
            delta[1]     <- -cost(x,theta) + beta * probEV[currLoc]*theta[3] 
        }
        
        # Calculate delta_1(x_t) (replacement)
        delta[2]       <- -theta[4] + beta * ( probEV[2]*(1-theta[3]) + probEV[1]*theta[3] ) 
        
        # Clean for instances of Inf
        expDelta = c(exp(delta[1]),exp(delta[2]))
        expDelta[expDelta==Inf] <- 10^10
        
        # Calculate P(a|x)
        policyA[currLoc,1]  <- expDelta[1] / (expDelta[1]+expDelta[2])
        policyA[currLoc,2]  <- expDelta[2] / (expDelta[1]+expDelta[2])
    }
    
    return(policyA)
    
}

