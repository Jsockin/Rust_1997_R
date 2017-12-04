#
#   Run Value Function Iteration
#

valueFunctionIteration <- function(beta,epsilon,theta,gridX,aVals,gridEV){

    # Parameters for looping
    error         <- 1
    startVFI      <- proc.time()
    iterationsVFI <- 1
    
    # Solve EV till convergence
    while (error > epsilon){
      
      prevGridEV <- gridEV
      
      # Update EV
      for (gridPos in 1:length(gridX)){
        x <- gridPos-1
        gridEV[gridPos] <- calcEV(x,beta,theta,gridX,prevGridEV,aVals)
      }
      
      # Calculate new error
      error <- max(abs(gridEV-prevGridEV))
      iterationsVFI <- iterationsVFI + 1
    }
    
    endVFI  <- proc.time()
    timeVFI <- endVFI - startVFI
    
    return(list(gridEV,iterationsVFI,timeVFI))

}