#
#   Simulate data (mileage and replacement decision).
#

simulate <- function(totalT,totalN,totalGridPoints,theta,policyA){
   
    aSamples      <- matrix(0,totalT,totalN)
    xSamples      <- matrix(0,totalT,totalN)
    xSamples[1,]  <- floor(runif(totalN,0,totalGridPoints))
    
    for (n in 1:totalN){
      for (t in 1:(totalT-1)){
        
        x                 <- xSamples[t,n]
        drawA             <- runif(1,0,1)
        drawTransition    <- runif(1,0,1)
        
        # Replacement Decision
        if (drawA > policyA[x+1,1]){
          aSamples[t+1,n] <- 1
          xSamples[t+1,n] <- 0
        } else{
          xSamples[t+1,n] <- x
        }
        
        # Transition or Not
        if (drawTransition > theta[3]){
          xSamples[t+1,n] <- xSamples[t+1,n] + 1 
        }
        
      }
    }
    
    return(list(xSamples,aSamples))
  
}