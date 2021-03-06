#Ex 1: 
AA<-c(0.10, 0.12, 0.08, 0.14, 0.08, 0.11, 0.13, 0.09, 0.12, 0.15)
BB<-c(0.14, 0.13, 0.10, 0.19, 0.01, 0.28, 0.46, 0.24, 0.07, 0.10)

(xbar.AA<-mean(AA)); (s.AA<-sd(AA))
(xbar.BB<-mean(BB)); (s.BB<-sd(BB))

Ex1.combo<-c(AA, BB)
Ex1.labels<-c(rep("AA", length(AA)),rep("BB", length(BB)))
Ex1.df<-data.frame(Ex1.combo, Ex1.labels)

stripchart(Ex1.combo~Ex1.labels, method="stack")
qqnorm(AA)
qqnorm(BB)

na=length(AA)
nb=length(BB)
df<-(var(AA)/na+var(BB)/nb)^2/((var(AA)/na)^2/(na-1)+(var(BB)/nb)^2/(nb-1))
df
t.test(AA, BB, var.equal = FALSE, alternative = "two.sided")

#By Hand in R
(Ex1.tobs<-(xbar.AA-xbar.BB-0)/sqrt(s.AA^2/10+s.BB^2/10))
p.val<-2*pt(Ex1.tobs, df)
p.val

#Bootstrap for Two Sample Test
boottwo <- function(dat1, dat2, nboot) { 
  bootstat <- numeric(nboot)
  truediff <- mean(dat1) - mean(dat2)
  n1 <- length(dat1)
  n2 <- length(dat2) 
  for (i in 1:nboot) {
    samp1 <- sample(dat1, size = n1, replace = T) 
    samp2 <- sample(dat2, size = n2, replace = T) 
    bootmean1 <- mean(samp1)
    bootmean2 <- mean(samp2)
    bootvar1 <- var(samp1)
    bootvar2 <- var(samp2)
    bootstat[i] <- (bootmean1 - bootmean2 - truediff)/sqrt((bootvar1/n1) + (bootvar2/n2))
  }
  return(bootstat) 
}

n.boot<-10000

set.seed(1)
Ex1.boot<-boottwo(AA, BB, n.boot)
(m.low<-sum(Ex1.boot<= Ex1.tobs))  #521
(m.up<-sum(Ex1.boot >= Ex1.tobs))   #9479
(pval<-2*min(m.low, m.up)/n.boot)  #0.1042


#Question 2
AA.2<-c(5.42, 3.54, 2.14, 9.56, 17.16, 1.98, 1.30, 2.01, 2.23, 2.04, 2.10, 2.12, 2.09, 2.07)
BB.2<-c(0.31, 0.27, 0.93, 0.92, 0.18, 0.90, 0.64, 2.42, 2.87, 2.15, 2.53, 2.75, 2.34, 2.38)

(xbar.AA2<-mean(AA.2)); (s.AA2<-sd(AA.2))
(xbar.BB2<-mean(BB.2)); (s.BB2<-sd(BB.2))

Ex2.combo<-c(AA.2, BB.2)
Ex2.labels<-c(rep("AA2", length(AA.2)),rep("BB2", length(BB.2)))
Ex2.df<-data.frame(Ex2.combo, Ex2.labels)

stripchart(Ex2.combo~Ex2.labels, method="stack")
boxplot(Ex2.combo~Ex2.labels)
qqnorm(AA.2)
qqnorm(BB.2)

(Ex2.t<-(xbar.AA2-xbar.BB2-0)/sqrt(s.AA2^2/10+s.BB2^2/10))
set.seed(1)
Ex2.boot<-boottwo(AA.2, BB.2, n.boot)
hist(Ex2.boot)
(m.low2<-sum(Ex2.boot<= Ex2.t))  #9883
(m.up2<-sum(Ex2.boot >= Ex2.t))  #117
(pval2<-2*min(m.low2, m.up2)/n.boot)


#Exercise 3
AA.3<-c(2.19, 2.07, 2.28, 2.08, 2.12, 1.88, 1.99)
BB.3<-c(1.99, 1.90, 1.74, 1.83, 1.87, 1.79, 1.94, 1.71)

(xbar.AA3<-mean(AA.3)); (s.AA3<-sd(AA.3))
(xbar.BB3<-mean(BB.3)); (s.BB3<-sd(BB.3))

Ex3.combo<-c(AA.3, BB.3)
Ex3.labels<-c(rep("AA3", length(AA.3)),rep("BB3", length(BB.3)))
Ex3.df<-data.frame(Ex3.combo, Ex3.labels)

stripchart(Ex3.combo~Ex3.labels, method="stack")
boxplot(Ex3.combo~Ex3.labels)
qqnorm(AA.3)
qqnorm(BB.3)

#T.test Equal Variance
t.test(AA.3, BB.3, var.equal=TRUE, alternative="two.sided")

#T.test Non Equal Variance
t.test(AA.3, BB.3, var.equal=FALSE, alternative="two.sided")

#Bootstrap
(Ex3.tobs<-(xbar.AA3-xbar.BB3-0)/sqrt(s.AA3^2/7+s.BB3^2/8))
set.seed(1)
Ex3.boot<-boottwo(AA.3, BB.3, n.boot)
(m.low3<-sum(Ex3.boot<=Ex3.tobs))
(m.up3<-sum(Ex3.boot>=Ex3.tobs))
pval3<-min(m.low3,m.up3)/n.boot
pval3