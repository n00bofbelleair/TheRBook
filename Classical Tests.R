# ch 8 : Classical Tests (p279-322)
# started 10/17 AM, done 10/20 AM

setwd('RBookFiles')

# SINGLE SAMPLE

# data summaries
das<-read.table('das.txt',header=T)
attach(das)
par(mfrow=c(2,2))
plot(y)
y2<-y
y2[50]<-2.179
plot(y2)
boxplot(y2)
hist(y2)
# to summarize data
summary(y2)
# to find quantiles two ways
fivenum(y2)
quantile(y2)

# test for normality with a plot
par(mfrow=c(1,1))
qqnorm(y2)
qqline(y,lty=2)

# use shapiro to test for normality
x<-exp(rnorm(30))
shapiro.test(x) # fails normality test - very low p value
shapiro.test(y2) # no compelling evidence to reject the null hypothesis, as p value is not low

# is the speed of light the hypothesized 299,990?  test value for SoL is 990 (299k has been subtracted)
light<-read.table('light.txt',header=T)
attach(light)
hist(speed)
summary(speed)
# strong negative skew, so mean is less than median
wilcox.test(speed,mu=990)
# we reject the null and accept the alt hypothesis.  such a low p value indicates that the speed of light is sig less than 299,990

# bootstrap in hypothesis testing
a<-numeric(10000)
for(i in 1:10000) a[i]<-mean(sample(speed,replace=T))
hist(a)
max(a)
# max value of sampling 10000 times with replacement is 984, which is less than the 990 hypothesized to be the mean, 
# so clearly 990 is unlikely to be the true mean, and p must be less than 0.0001 (since in 10000 tries it never occurs)


# write a skew function
skew<-function(x){
  m3<-sum((x-mean(x))^3)/length(x)
  s3<-sqrt(var(x))^3
  m3/s3
}

sdata<-read.table('skewdata.txt',header=T)
attach(sdata)
hist(values)
skew(values) # 1.319
# is skew sig dif from zero?  do a t test
skew(values)/sqrt(6/length(values)) # 2.949
# prob of getting this t value when skew is zero?
1-pt(2.949,length(values)-2) # 0.0032
# conclude that data shows significant non-normality
# we can rerun tests with sqrt or log of values to see if those transformations reduce the skew (log is ideal here)

#write a kurtosis function
kurt<-function(x){
  m4<-sum((x-mean(x))^4)/length(x)
  s4<-var(x)^2
  m4/s4 - 3
}
# find kurtosis of data
kurt(values)
# find kurt divided by se of kurt
kurt(values)/sqrt(24/length(values)) # not sigdif from normal





# TWO SAMPLES - p289

# comparing two variances
# F test
ftdata<-read.table('f.test.data.txt',header=T)
attach(ftdata)
var.test(gardenB,gardenC)
# we reject the null bc the p-value is so small

refs<-read.table('refuge.txt',header=T)
attach(refs)
tapply(B,T,var)
which(T==9) # 31 - need to omit this for tests to work

bartlett.test(B[-31],T[-31]) # p>0.05, no sigdif bw the 8 variances
fligner.test(B[-31],T[-31]) # p<0.05, sigdif bw the 8 variances
# try plotting
plot(T,B) # shows well behaved variances
model<-lm(B~T)
plot(model) # shows some pattern in residuals, however
# so interpret how you will
detach(refs)
detach(ftdata)

ozone<-read.table('gardens.txt',header=T)
attach(ozone)
y<-c(gardenA,gardenB,gardenC)
garden<-factor(rep(c("A","B","C"),c(10,10,10)))

var.test(gardenB,gardenC) # very low p
bartlett.test(y~garden) # even lower p
fligner.test(y~garden) # high p
# this is because first two tests are very sensitive to outliers, while last test is not



# 10/19 AM
# comparing two means

# use student's t test for independent samples w normal errors and constant variances
# to find the crit value of a t test with 18 deg of freedom and 95% CI:
qt(0.975,18)
t.data<-read.table('t.test.data.txt',header=T)
attach(t.data)
# use notched boxplot to graphically test these two samples
ozone<-c(gardenA,gardenB)                                                                          
label<-factor(c(rep("A",10),rep("B",10)))
boxplot(ozone~label,notch=T,xlab="Garden",ylab="Ozone")
# due to notches not overlapping, we conclude that the medians are significantly different
# now t test the means
t.test(gardenA,gardenB)
# t stat is larger than crit value of 2.1, so we reject the null

# Wilcoxon rank-sum test
# use if errors are non-normal
wilcox.test(gardenA,gardenB)
# again p value is tiny so we reject the null

# tests on paired samples
streams<-read.table('streams.txt',header=T)
attach(streams)
t.test(down,up) # high p value, suggesting null is true
t.test(down,up,paired=T) # low p value now - we can in fact reject the null

# the sign (binomial) test
binom.test(1,9)
# as p is less that 0.05 we would conclude that whatever was working 8 times out of 9 was a successful improvement
# we can do this on two samples as well, see page 300

# binomial test to compare two proportions
prop.test(c(4,196),c(40,3270))

# chi squared contingency tables
# pearson's chi squared
(count<-matrix(c(38,14,11,51),nrow=2))
chisq.test(count) # p basically zero, so values are not independent
# to extract frequencies expected under independence:
chisq.test(count,correct=F)$expected

# chisq w unequal probabilities
chisq.test(c(10,3,2,6)) # p > 0.05
chisq.test(c(10,3,2,6),p=c(.2,.2,.3,.3)) # p < 0.05

# test for a fair die
die<-ceiling(runif(100,0,6))
table(die)
chisq.test(table(die))

# fisher's exact test (for when one or more expected frequency is less than 4 or 5)
x<-as.matrix(c(6,4,2,8))
dim(x)<-c(2,2)
fisher.test(x)
# or do
table<-read.table('fisher.txt',header=T)
attach(table)
fisher.test(tree,nests)


# 10/20 AM
# correlation and covariance
data<-read.table('twosample.txt',header=T)
attach(data)
plot(x,y)
var(x) # approx 200
var(y) # approx 977
var(x,y) # approx 415 (this is the covar)
cor(x,y) # correlation coefficient(r) is approx 0.94

# cor matrix
pollute<-read.table('Pollute.txt',header=T)
attach(pollute)
cor(pollute) # correlation matrix
cor(Pollution,Wet.days) # correlation of two variables in the matrix

paired<-read.table('paired.txt',header=T)
attach(paired)
names(paired)
cor(Upstream,Downstream)
cor.test(Upstream,Downstream) # cor is highly significant

detach(data)
prod<-read.table('productivity.txt',header=T)
attach(prod)
plot(x,y,xlab="Productivity",ylab="Mammal species")
cor.test(x,y,method="spearman")
# p val basically zero, so increasing productivity should equal increasing species richness

library(lattice)
xyplot(y~x|f,
       panel=function(x,y) {
       panel.xyplot(x,y,pch=16)
       panel.abline(lm(y~x)) })
# but here we show negative corr by each region between productivity and species richness

# experimenting
wings<-read.table('wings.txt',header=T)
attach(wings)
wings$location<-c(LocationA+LocationB)
sum(location)
s<-1:377
locsum<-1:15
locsum[1:15]<-0
locsum[1]=1
for(i in 2:15) {
  locsum[i]<-location[i]+locsum[i-1]
}
wings$locsum<-locsum
s<-rep(Size,times=location)
s

# power analysis - see p318
# bootstrapping - see p319-322





# DONE !!







