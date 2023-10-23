# Mathematics - ch7 (p195-277)
# started 10/12 AM, done 10/13 PM

# to draw smooth functions, generate a series of 100+ regularly spaced values between min(x) and max(x)
x<-seq(0,10,0.1)
y<-exp(x)
plot(y~x,type="l",main="Exponential")
z<-log(x)
plot(z~x,type="l",main="Logarithmic")

# trig
xtrig<-seq(0,2*pi,2*pi/100)
y1<-cos(x)
y2<-sin(x)
par(mfrow=c(2,2))
y3<-tan(x)
plot(y1~xtrig,type="l", main="cos")
plot(y2~xtrig,type="l", main="sin")
plot(y3~xtrig,type="l", main="tang")
plot(y3~xtrig,type="l", ylim=c(-3,3), main="tang restricted")

#power laws
xp<-seq(0,1,0.01)
p<-xp^0.5
plot(xp,p,type="l",main="0<b<1")
p<-xp
plot(xp,p,type="l",main="b=1")
p<-xp^2
plot(xp,p,type="l",main="b>1")
p<-1/xp
plot(xp,p,type="l",main="b<0")

# see p199-207 for function types not entirely relevant to me

# Continuous Probability Distributions - p208

# Central Limit Theorem - p213
hist(runif(10000)*10,main="")

means<-numeric(10000)
for (i in 1:10000) {
  means[i]<-mean(runif(5)*10)
}
hist(means,ylim=c(0,2000))
mean(means) # 5.02522
sd(means) # 1.291854

xv<-seq(0,10,0.1)
yv<-dnorm(xv,mean=mean(means),sd=sd(means))*5000
lines(xv,yv)


# craps : p215
# bell curve gets more normal the more dice we throw in 10,000 goes
par(mfrow=c(2,2))
a<-sample(1:6,replace=T,10000)
b<-sample(1:6, replace=T, 10000)
c<-sample(1:6, replace=T, 10000)
d<-sample(1:6, replace=T, 10000)
e<-sample(1:6, replace=T, 10000)
hist(a,breaks=0.5:6.5,xlab="one die")
hist(a+b,breaks=1.5:12.5,xlab="two dice")
hist(a+b+c,breaks=2.5:18.5,xlab="three dice")
hist(a+b+c+d+e,breaks=4.5:30.5,xlab="five dice")
lines(seq(1,30,0.1),dnorm(seq(1,30,0.1),mean(a+b+c+d+e),sd(a+b+c+d+e))*10000)


# hypothesis testing distributions - p221 - chi square, F, and t tests
# ayo
# plus other CPDs thru page 242

# Discrete Probability Distributions - p242-258

# Matrix algebra - p258
a<-matrix(c(1,0,4,2,-1,1),nrow=3)
a
b<-matrix(c(1,-1,2,1,1,0),nrow=2)
b
(c<-a%*%b) # multiply matrices
(d<-b%*%a)

(ym<-diag(1,3,3))
diag(ym)<-1:3
ym
diag(ym)
# extract diag of a var-cov matrix
M<-cbind(X=1:5,Y=rnorm(5))
diag(var(M))
# det
A<-matrix(c(1,2,4,2,1,1,3,1,2),nrow=3)
det(A)
# inv
library(MASS)
ginv(A)
ginv(ginv(A))

# eigenvalues and eigenvectors (see p264-7)
L<-matrix(c(0,0.7,0,0,6,0,0.5,0,3,0,0,0.3,1,0,0,0),nrow=4)
L
n<-matrix(c(45,20,17,3),ncol=1)
n
L%*%n
fun<-function(x) L%*%x

structure<-numeric(160)
dim(structure)<-c(40,4)

for (i in 1:40) {
  n<-fun(n)
  structure[i,]<-n
}
matplot(1:40,log(structure),type="l")

sum(structure[40,])/sum(structure[39,])
# 2.164035 is pop growth rate by year 40
structure[40,]/sum(structure[40,])
# approximate stable age structure is : 0.709769309 0.230139847 0.052750539 0.007340305
eigen(L)
# dominant value is 2.1694041
eigen(L)$vectors[,1]/sum(eigen(L)$vectors[,1])
# dominant vector is : 0.710569659+0i 0.229278977+0i 0.052843768+0i 0.007307597+0i

# more matrices through p274

# Calculus - p274-277




# DONE !!





