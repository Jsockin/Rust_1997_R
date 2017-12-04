#
#   Estimate log-likelihood given policy function
#

logLikelihood <- function(xSamples,aSamples,policyA){
    
    NN          <- dim(xSamples)[2]
    TT          <- dim(xSamples)[1]
    likeMatrix  <- matrix(0,TT,NN)
      
    for (n in 1:NN){
        for (t in 1:TT){
            gridPos   <- xSamples[t,n] + 1
            likeMatrix[t,n]  <- aSamples[t,n]*log(policyA[gridPos,2]) + (1-aSamples[t,n])*log(policyA[gridPos,1]) 
        }
    }
        
    likelihood  <- sum(likeMatrix)
    return(likelihood)
}