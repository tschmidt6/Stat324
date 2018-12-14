
#Mixed 1
p.hat=189/270
SE=sqrt(p.hat*(1-p.hat)/270)
qnorm(.05)  #1.645

lower=p.hat-1.645*SE
upper=p.hat+1.645*SE

SE_Hyp<-sqrt(.50*.50/270)
Z.obs<-(p.hat-.50)/SE_Hyp

#Mixed #2
#a/b
S.B.mean<-214.4
S.G.mean<-216.3
S.B.s<-12.9
S.G.s<-18.0
(S.pool.var<-(7*(12.9^2)+7*(18.0^2))/(14))
sqrt(245.205)
(Obs_diff<-(S.B.mean-S.G.mean))
qt(.975, df=14)
S.SE.diff<-sqrt(S.pool.var/8+S.pool.var/8)
(t.obs<-(Obs_diff-0)/S.SE.diff)
2*pt(t.obs, df=14)

Ank_Obs_diff<-(71.4-72.5)
Ank_Obs_SE<-sqrt(4.1^2/8+9.3^2/8)
#df=$\frac{[(s_1^2/n_1)+(s_2^2/n_2)]^2}{(s_1^2/n_1)^2/(n_1-1)+(s_2^2/n_2)^2/(n_2-1)}$ 

#We know $s_1^2/n_1=9.3^2/8=10.81125$ and $s_2^2/n_2=4.1^2/8=2.10125$, so df=$\frac{(10.81125+2.10125)^2}{10.81125^2/7+2.10125^2/7}=$

(10.81125+2.10125)^2/(10.81125^2/7+2.10125^2/7)
Ank.t.obs<-(Ank_Obs_diff-0)/Ank_Obs_SE
2*pt(-0.306, df=9)

#Mixed Practice 2d:
GS_Obs_diff<-23.9-22.2
GS_pool_var=(7*2.5^2+7*4.1^2)/14
GS_SE<-sqrt(GS_pool_var/8+GS_pool_var/8)
CI.low=GS_Obs_diff-2.145*GS_SE
CI.high=GS_Obs_diff+2.145*GS_SE

#Mixed Practice 3a
608-392
obs_G=392/608
obs_W=413/527
(prop_obs_diff=obs_W-obs_G)
(prob_SE<-sqrt(.645*(1-.645)/608+0.784*(1-.784)/527))
qnorm(.80)
(pooled=(392+413)/(608+527))
SE_diff<-sqrt(pooled*(1-pooled)/608+pooled*(1-pooled)/527)

#Mixed Practice 4 a
bottom<-c(0.43, 0.266, 0.567, 0.531, 0.707, 0.716)
top<-c(0.415, 0.238, 0.390, 0.410, 0.605, 0.609)
diff<-bottom-top
diff
qqnorm(diff)
mean(diff); sd(diff)
se_avg<-sd(diff)/sqrt(length(diff))
qt(.975, df=5)
mean(diff)-2.57*se_avg

#Mixed Practice 4 b
qnorm(.975)
1.96*0.061/sqrt(15)
(0.0259-0.20)/(0.061/sqrt(15))
wilcox.test(bottom, top, paired=TRUE, alternative="greater")

#Bootrap Function 
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

set.seed(1)
water.boot<-bootstrap(diff, n.boot=10000)
hist(water.boot)
t.obs=(mean(diff)-0)/(sd(diff)/sqrt(length(diff)))
m.up<-sum(water.boot >= t.obs)
m.low<-sum(water.boot <= t.obs)
p.value=m.up/10000

