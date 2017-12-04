#
#   Caculate likelihood given theta and parameters.
#

calcLikelihood <- function(thetaHat,beta,gamma,epsilon,probTransition,gridX,aVals,probA,xSamples,aSamples){
    
    thetaPrime    <- c(thetaHat[1],thetaHat[2],probTransition,thetaHat[3])
    
    policyResults <- policyFunctionIteration(beta,gamma,epsilon,thetaPrime,gridX,aVals,probA)
    
    policyA       <- policyResults[[2]]
    
    # Negate likelihood for minimizing negative 
    likelihood    <- -1 * logLikelihood(xSamples,aSamples,policyA)
    
    print(thetaPrime)
    print(likelihood)
    
    return(likelihood)
}