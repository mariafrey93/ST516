Exercise 2.11 a

nsim<-5000 #number of random numbers
n=25;p=.2; #parametre to binomial
y=seq(0,n,by=1) #sequence used to generate 
cp=pbinom(y,n,p) #make cdf of binomial
X=rep(0,nsim) # A vector to store 
for(i in 1:nsim){
  u=runif(1)
  X[i]=sum(cp<u) #checks to see what interval the uniform random variable fell in and assigns the correct Binomial value to X
}
hist(X,freq=F) #histogram
lines(1:n,dbinom(1:n,n,p),lwd=2) #Desnity function
system.time(rbinom(5000,25,.2))  #Calculate time

#Another method whee it is make as a function
MYbinom<-function(s0,n0,p0){
  cp=pbinom(seq(0,n0,by=1),n0,p0) #make cdf of binomial
  X=rep(0,s0) #Vector to store 
  for (i in 1:s0){
    u=runif(1)
    X[i]=sum(cp<u) #checks to see what interval the uniform random variable fell in and assigns the correct Binomial value to X
  }
  return(X)
}
system.time(MYbinom(5000,25,.2)) #calculate time 

#Exercise 2.11 b

original<-function(s0,alpha){
  U=rep(0,s0)
  for (i in 1:s0){
    u=runif(1)
    while (u > alpha) u=runif(1)
    U[i]=u
    u=runif(1)
  }
  return(U)
  
  
  
}
par(mfrow=c(1,2))
hist(original(1000,.1))

system.time(original(1000,.1))

Trans<-function(s0,alpha){
  U=rep(0,s0)
  for (i in 1:s0) {
    U[i]=alpha*runif(1)
  }
  return(U)
}
hist(Trans(1000,.1))
system.time(Trans(1000,.1))