#Problem 3

#Output 15 plots in one graph
par(mfrow=c(3,5))
for(i in 1:15){
  samples <- rnorm(10)
  qqnorm(samples, frame = FALSE)
  qqline(samples, col = "red", lwd = 2)
}
par(mfrow=c(1,1))

#Problem 4
pop1<-c(rep(0:9, each=1))
hist(pop1, freq=FALSE, breaks=seq(-.5, 11.5, by=1))
sampling.dist<-function(sample.size,pop, numExp=10000){
  estimate=rep(0, times=numExp)
  for (i in 1:numExp){
    #Take a sample from pop
    sample<-sample(pop,sample.size, replace=TRUE)
    #Calculate the estimated mean
    estimate[i]<-mean(sample)
  }
  return(estimate)
}

#For sample.size=2
xbar.n2=sampling.dist(sample.size=2, pop=pop1, numExp=10000)
hist(xbar.n2, freq=FALSE, main="Approx. Sampling Distribution of X-Bar with n=2")
qqnorm(xbar.n2)
mean(xbar.n2); var(xbar.n2)


#For sample.size=10
xbar.n10=sampling.dist(sample.size=10, pop=pop1, numExp=10000)
hist(xbar.n10, freq=FALSE, main="Approx. Sampling Distribution of X-Bar with n=10")
qqnorm(xbar.n10)
mean(xbar.n10); var(xbar.n10)
