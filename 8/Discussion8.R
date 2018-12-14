# 1) a
# Ho: mu = 2000
# Ha: mu > 2000

# b
# One sample t-hypothesis test at 5% level
# Assumptions: normal distribution, independent observation
# mean = 2001.98
# SD = 11.281
# tobs = xbar - mu / S / sqrt(n)
(2001.98 - 2000) / (11.281 / sqrt(75)) # = 1.520016
# P(t74 > 1.520016) = 0.066386
# 1.52 in t table, 0.05 < pvalue < 0.1
# Fail to reject because pvalue is greater than alpha

# c
# if tobs is 1.52, then pvalue in Z test is P(Z > 1.52) = 0.0643
# Very close to t test because n is large



# 2) a
# Data is not normal and n is smaller than 30 so either bootstrap or sign test should be used

# b i
# Assumptions: independent observation

# b ii
# Ho: mu = 145 Ha: mu > 145
lettuce<-c(145, 142, 144, 141, 142, 155, 143, 157, 152, 143, 103, 151, 150, 148, 150, 162, 149, 158, 144, 151)
hist(lettuce, freq=FALSE, main="Number of Heads of Lettuce")
qqnorm(lettuce)
mean(lettuce); sd(lettuce)

#Bootstrapping Function:
  # (The bootstrap() function, below, is the same as the one we used for the bootstrap CI)
  # Create a new function, bootstrap(x, n.boot), having two inputs:
  #   - x is a data vector
  #   - n.boot is the desired number of resamples from x
  # It returns a vector of n.boot t-hat values.
  bootstrap = function(x, n.boot) {
    n = length(x)
    x.bar <- mean(x)
    t.hat <- numeric(n.boot) # create vector of length n.boot zeros
    for(i in 1:n.boot) {
      x.star <- sample(x, size=n, replace=TRUE)
      x.bar.star <- mean(x.star)
      s.star <- sd(x.star)
      t.hat[i] <- (x.bar.star - x.bar) / (s.star / sqrt(n))
    }
    return(t.hat)
  }

# b iii
#Find T_obs
n=length(lettuce)
x.bar=mean(lettuce)
s=sd(lettuce)
mu.0=145
(t.obs=(x.bar-mu.0)/(s/sqrt(n)))

#Use bootstrap function to get an approximate sampling
# distribution of T for the smoke data.
B=6000
set.seed(1)
lettuce.boot<-bootstrap(lettuce, B)

# Plot the approximate sampling distribution.
hist(lettuce.boot, freq=FALSE, xlab = "Bootstrap t-hat values",main = "Approximate Sampling Distribution of T")

summary(lettuce.boot > t.obs)   #2173 above, 3827 below

# b iv
# sum() counts the TRUE values by first converting TRUE / FALSE values to 1 / 0.
m.below = sum(lettuce.boot < t.obs) # This "<" depends on H_A.
m.above = sum(lettuce.boot > t.obs) # This ">" depends on H_A.
p.value = m.above / B
print(p.value)

# b v
# p value is larger than alpha we fail to reject null

# c
# P(T19 > 0.5668403) = 0.2887292
# pvalue > 0.25 in t table (goes to far to the left so > 0.25)
# fail to reject because greater than 0.05

# d
# P(B >= b)
# B~Bin(n,0.5)
# n = 19 because one lettuce = 145 exactly
# 11 are greater than 145
# pvalue = P(B >= 11)
1 - pbinom(10, 19, 0.5) # pvalue = 0.3238
binom.test(11, 19, alternative = "greater") # p-value = 0.3238
# Fail to reject because pvalue is greater than 0.05


# 3 a
# Ho: pi = 0.60
# Ha: pi =/= 0.60
# pi^ = 86 / 125 = 0.688 

# Check conditions: 
# n * pi > 5
# 125 * 0.60 > 5

# n * (1 - pi) > 5
# 125 * (0.40) > 5

# Z = (0.688 - 0.60) / sqrt(0.60 * (1 - 0.60) / 125) = 2.008316
# 2 * P(Z > 2.0083126) = 2 * 0.02230486 = 0.0445
# Evidence suggests pass rate may be different from 60% because pvalue is less than 0.05

# b
# p +- Z 0.25 * sqrt(p * (1 - p) / n) 
# = 0.688 +- 1.96 * sqrt(0.688 * (1 - 0.688) / 125)
# = 0.688 +- 1.96







