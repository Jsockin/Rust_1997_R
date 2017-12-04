#
#   Run Policy Function Iteration
#

policyFunctionIteration <- function(beta,gamma,epsilon,theta,gridX,aVals,policyA){
    
    # Parameters for looping
    error             <- 1
    startPolicy       <- proc.time()
    iterationsPolicy  <- 1
    
    # Solve EV till convergence
    while (error > epsilon){
      
      prevPolicyA <- policyA
      
      # Update EV
      probEV <- calcProbEV(beta,gamma,theta,gridX,policyA)
      
      # Update policy function
      policyA <- updatePolicy(beta,theta,gridX,probEV,aVals)
      
      # Adjust policyA to avoid issues with log(0) 
      policyA[policyA==0] <- 10^-20
      
      # Calculate new error
      error <- max(abs(policyA-prevPolicyA))
      iterationsPolicy <- iterationsPolicy + 1
    
    }
    
    endPolicy  <- proc.time()
    timePolicy <- endPolicy - startPolicy
    
    return(list(probEV,policyA,iterationsPolicy,timePolicy))
}