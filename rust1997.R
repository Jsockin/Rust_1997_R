# 
#   This program will explore Rust (1997).
#
#       c(x_t;theta) = theta_1 * x + theta_2 * x^2
#       x_t+1 = x_t + 1  w.p 1-theta_3
#             = x_t      w.p. theta_3
#       replacement cost = theta_4

#-------------------------------------
# Step 0: Housekeeping / Load Libraries / Import Functions
#-------------------------------------

rm(list=ls())
set.seed(1)

path <- "C:/Users/jsock/Dropbox/Penn/Homework/Empirical_IO/HW_5"

#install.packages("stats")
library("stats")

source(paste(path,"/cost.R",sep=""))
source(paste(path,"/calcEV.R",sep=""))
source(paste(path,"/calcProbEV.R",sep=""))
source(paste(path,"/updatePolicy.R",sep=""))
source(paste(path,"/logLikelihood.R",sep=""))
source(paste(path,"/valueFunctioniteration.R",sep=""))
source(paste(path,"/policyFunctioniteration.R",sep=""))
source(paste(path,"/simulate.R",sep=""))
source(paste(path,"/calcLikelihood.R",sep=""))

#-------------------------------------
# Step 1: Parametrization / Grid
#-------------------------------------

beta            <- 0.75
gamma           <- 0.577216 
epsilon         <- 10^-6
theta           <- c(0.5,0.05,0.4,150)

totalGridPoints <- 90 
gridX           <- 0:totalGridPoints
aVals           <- c(0,1)

#-------------------------------------
# Step 2: Value Function Iteration on EV(x)
#-------------------------------------

gridEV        <- matrix(0,length(gridX),1)

valueResults  <- valueFunctionIteration(beta,epsilon,theta,gridX,aVals,gridEV)

valueEV       <- valueResults[[1]]
valueIter     <- valueResults[[2]]
valueTime     <- valueResults[[3]][3]

#-------------------------------------
# Step 3: Policy Function Iteration on P(a|x)
#-------------------------------------

policyA       <- matrix(0.5,length(gridX),length(aVals))

policyResults <- policyFunctionIteration(beta,gamma,epsilon,theta,gridX,aVals,policyA)

policyEV      <- policyResults[[1]]
policyA       <- policyResults[[2]]
policyIter    <- policyResults[[3]]
policyTime    <- policyResults[[4]][3]

#-------------------------------------
# Step 4: Plot Results
#-------------------------------------

# Plot EV(x)
jpeg(file=paste(path,"/Value_fun_iter.jpg",sep=""))
plot(gridX,valueEV,main='EV(x)',xlab='x',ylab='',col='black',lty=1,type='l')
dev.off()

# Plot P(a|x)
jpeg(file=paste(path,"/Policy_fun_iter.jpg",sep=""))
plot(gridX,policyA[,1],main='P(a|x)',xlab='x',ylab='',col='blue',lty=2,type='l')
par(new=TRUE)
plot(gridX,policyA[,2],main='P(a|x)',xlab='x',ylab='',col='red',lty=3,type='l')
legend(50,0.6,c('a = 0 (do not replace)','a = 1 (replace)'),lty=c(2,3),col=c('blue','red'),cex=0.75)
dev.off()

# Plot EV(x) for both approaches
jpeg(file=paste(path,"/Compare_EV.jpg",sep=""))
plot(gridX,valueEV,main='EV(x)',xlab='x',ylab='',col='black',lty=1,type='l',ylim=c(-160,0))
par(new=TRUE)
plot(gridX,policyEV,main='EV(x)',xlab='x',ylab='',col='red',lty=2,type='l',ylim=c(-160,0))
legend(50,-10,c('Value function iteration','Policy function iteration'),lty=c(1,2),col=c('black','red'),cex=0.75)
dev.off()

#-------------------------------------
# Step 4: Simulate Data and Calculate Log Likelihood
#-------------------------------------

totalT        <- 100
totalN        <- 100
simulations   <- simulate(totalT,totalN,totalGridPoints,theta,policyA)

xSamples     <- simulations[[1]]
aSamples     <- simulations[[2]]

likelihood <- logLikelihood(xSamples,aSamples,policyA)

#-------------------------------------
# Step 5: Maximize Log Likelihood to Estimate Theta
#-------------------------------------

theta0     <- c(theta[1],theta[2],theta[4])
policyA0   <- matrix(0.5,length(gridX),length(aVals))

likelihoodFunction <- function(x) calcLikelihood(x,beta,gamma,epsilon,theta[3],gridX,aVals,policyA0,xSamples,aSamples)
optimResults       <- nlminb(theta0,likelihoodFunction,lower=c(-Inf,-Inf,0),upper=c(Inf,Inf,Inf))
thetaStar <- optimResults$par
