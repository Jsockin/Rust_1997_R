#
#   Calculates EV(x) using bayesian probabilities
#   over the action space.
#

calcProbEV <- function(beta,gamma,theta,gridX,policyA){
  
    unitVec     <- matrix(1,1,length(gridX))
    identityMat <- diag(length(gridX))
    
    p0          <- policyA[,1]
    p1          <- policyA[,2]
    
    e0          <- gamma - log(p0)
    e1          <- gamma - log(p1)
    
    f0          <- matrix(0,length(gridX),length(gridX))
    for (x in 1:length(gridX)){
      
        f0[x,x] <- theta[3]
        
        if (x < length(gridX)){
            f0[x,x+1] <- 1 - theta[3]
        }
    }
    
    f1          <- matrix(0,length(gridX),length(gridX))
    f1[,1]      <- 1 - theta[3]
    f1[,2]      <- theta[3]
    
    costVec     <- cost(gridX,theta)
    R           <- matrix(theta[4],length(gridX),1)
    
    M           <- identityMat - beta * ( p0 %*% unitVec * f0 + p1 %*% unitVec * f1 )
    EV          <- solve(M) %*% ( p0 * (-costVec + e0) + p1 * (-R + e1) )
    
    return(EV)
}

