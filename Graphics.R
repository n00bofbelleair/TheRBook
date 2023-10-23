# R Book ch 5 : Graphics (p135-181)
# started 10/10, finished 10/11

# create a plot of some data
data1<-read.table('RBookFiles/scatter1.txt',header=T)
attach(data1)
names(data1)
plot(xv,ys,col="red",xlab="Explanatory Var",ylab="Response Var")
# add a regression line through the data
abline(lm(ys~xv))

# add more points
data2<-read.table('RBookFiles/scatter2.txt',header=T)
attach(data2)
names(data2)
points(xv2,ys2,col="blue")
abline(lm(ys2~xv2))

# note that lower values from blue dataset are missing from plot - this can be fixed by plotting all data at once with type "n"
plot(c(xv,xv2),c(ys,ys2),type="n")
points(xv,ys,col="green")
points(xv2,ys2,col="purple")
abline(lm(ys~xv), col="red")
abline(lm(ys2~xv2),col="orange")
# use legend and locator(1) to add a legend at a chosen spot (locator(1) allows you to select the top left corner of the legend's locale)
legend(locator(1),c("treatment","control"),pch=c(1,1),col=c(3,6)) # 3 is green, 6 is purple for col


# show a chart of all plotting characters
plot(0:10,0:10,type='n', main="Plotting Characters 0-25")
k<--1
for(i in c(2,5,8)){
  for(j in 0:9) {
    k<-k+1
    points(i,j,pch=k,cex=2)
  }
}



setwd("I:/My Drive/R work/TheRBook/RBookFiles")


# identifying individuals in scatterplots with color and symbols
data<-read.table('sleep.txt',header=T)
attach(data)
Subject<-factor(Subject)
plot(Days,Reaction,col=as.numeric(Subject),pch=as.numeric(Subject))

pgr<-read.table('pgr.txt',header=T)
attach(pgr)
names(pgr)
plot(hay,pH)
text(hay,pH,labels=round(FR,2),pos=1,offset=0.5,cex=0.7)
# labels are centered on x value (pos 1) and offset half a char below the point (offset 0.5)
# they show value of FR rounded to 2 sig-dig (FR,2) at 70% character expansion (cex 0.7) (size of text)

plot(hay,pH,pch=16,col=ifelse(FR>median(FR),"red","black"))
# plots same points but those w FR above median are red

# adding text to scatterplots
map.places<-read.csv('map.places.csv', header=T)
attach(map.places)
map.data<-read.csv('bowens.csv',header=T)
attach(map.data)
nn<-ifelse(north<60,north+100,north)
plot(c(20,100),c(60,110),type='n')
for (i in 1:length(wanted)){
  ii <- which(place == as.character(wanted[i]))
  text(east[ii], nn[ii], as.character(place[ii]), cex=0.6)
}


# drawing maths
curve(x^3-3*x,-2,2) # draw specified curve for x and y -2 to 2

plot(0:10,0:10,xact="n",yaxt="n",type="n")
rect(6,6,9,9)
arrows(1,1,3,8)
arrows(1,9,5,9,code=3) # code = 3 makes arrow double-headed. 1 is head at start, 2 is head at end
arrows(4,1,4,6,code=3,angle=90) # arrow head is by default at 30* angle.  use 90 to get straight line

# draw an arrow from first click to second click
click.arrows<-function(){
  coos<-c(unlist(locator(1)),unlist(locator(1)))
  arrows(coos[1],coos[2],coos[3],coos[4])
}
click.arrows()

# draw a 6 sided polygon by clicking
locations<-locator(6)
locations$x # list the 6 x coords
locations$y # list the 6 y coords
polygon(locations,col="green") # draw polygon from coords clicked


# draw normal curve
xv<-seq(-3,3,0.01)
yv<-dnorm(xv)
plot(xv,yv,type='l')
# fill area to left of x=-1 in purple
polygon(c(xv[xv<=-1],-1),c(yv[xv<=-1],yv[xv==-3]),col="purple")


# draw two smooth Ricker curves (see page 149)
xr<-0:100
yA<-482*xr*exp(-0.045*xr)
yB<-518*xr*exp(-0.055*xr)
plot(c(xr,xr),c(yA,yB),xlab="Stock",ylab="Recruits",type="n")
lines(xr,yA,lty=1,col="blue")
lines(xr,yB,lty=2,col="red")
legend(locator(1),c("yA = (482xe^-0.045x)","yB = (518xe^-0.055x)"),pch=c(1,1),col=c(4,2)) 

# fitting non-linear parametric curves through a scatterplot
info<-read.table('plotfit.txt',header=T)
attach(info)
plot(x,y,pch=16)
# start list
(model<-nls(y~a*x*exp(-b*x),start=list(a=500,b=0.05)))
# use xr and model with predict to add dashed line to plot
lines(xr,predict(model,list(x=xr)),lty=2)
# compare with theoretical model by evaluating y across xr
yr<-480*xr*exp(-0.047*xr)
lines(xr,yr) # add line to plot using lines


# fitting non-parametric curves through a scatterplot
jaws<-read.table('jaws.txt',header=T)
attach(jaws)

# make four graphs using lowess, loess, gam, and lm
par(mfrow=c(2,2)) # set up for four graphs
# lowess
plot(age,bone)
text(45,20,"lowess",pos=2)
lines(lowess(age,bone))
# loess
plot(age,bone)
text(45,20,"loess",pos=2)
mjaws<-loess(bone~age)
xjaw<-0:50
yjaw<-predict(mjaws,data.frame(age=xjaw))
lines(xjaw,yjaw)
# gam
plot(age,bone)
text(45,20,"gam",pos=2)
library(mgcv)
mjaws<-gam(bone~s(age))
yjaw<-predict(mjaws,list(age=xjaw))
lines(xjaw,yjaw)
# lm
plot(age,bone)
text(45,20,"ploynomial (lm)",pos=2)
mjaws<-lm(bone~age+I(age^2)+I(age^3))
yjaw<-predict(mjaws,list(age=xjaw))
lines(xjaw,yjaw)

# now drop back to 1x1 plot
par(mfrow=c(1,1))
# and detach info for next exercise
detach(info)

# draw lines between ordered points
smooth<-read.table('smoothing.txt',header=T)
attach(smooth)
sequence<-order(x)
plot(x,y,pch=16)
lines(x[sequence],y[sequence])


# continue work, 10/10, PM
setwd("I:/My Drive/R work/TheRBook/RBookFiles")

weather<-read.table('SilwoodWeather.txt',header=T)
attach(weather)
names(weather)
month<-factor(month)
plot(month,upper,xlab="Month",ylab="Daily Temp Max")

trial<-read.table('compexpt.txt',header=T)
attach(trial)
names(trial)
class(clipping) # is character right now
clipping<-factor(clipping) # make factor
plot(clipping,biomass,xlab="treatment",ylab="yield")

par(mfrow=c(1,2))
boxplot(biomass~clipping)
boxplot(biomass~clipping,notch=T) # notches that do not overlap suggest stat sig medians
# notch = +-1.58 * IQR/sqrt(n)
# with small sample or large var, notches may appear above/below 75th/25th percentiles, as they do here
# plot means with a bar chart ig
means<-tapply(biomass,clipping,mean)
par(mfrow=c(1,1))
barplot(means,xlab="treatment", ylab="yield")


# boxplot ordering and mean/median differences
data<-read.table('box.txt',header=T)
attach(data)
plot(response~factor(fact))
# because factor labels are unordered, this is not very helpful to look at
# calc an index to rank the mean values of response across the different factor levels
index<-order(tapply(response,fact,mean))
ordered<-factor(rep(index,rep(20,8)))
boxplot(response~ordered,notch=T,names=as.character(index),xlab="ranked treatments",ylab="response")
# boxes are ranked from lowest to greatest mean yield here
model<-aov(response~factor(fact))
summary(model)
# very compelling evidence (p is basically 0) that there are sig difs between mean yields of the 8 crop cultivars
plot(TukeyHSD(model))
TukeyHSD(model)
# Tukey's Honest SigDif says a comparison with an interval not overlapping the center line has sigdif means


# plotting for single samples
hist(rpois(1000,1.7),breaks=seq(-0.5,9.5,1),main="",xlab="random numbers from a Poisson dist w mean 1.7")
# we can see with the breaks used that the mode is 1 and that 2's are more frequent than 0's





# 10/11

# overlaying histos w smooth density functions
# create 158 random ints from a neg binom dist w mu=1.5 and k=1
y<-rnbinom(158,mu=1.5,size=1)
bks<- -0.5:(max(y)+0.5)
hist(y,bks,main="")
# estimate the parameters of the distribution
mean(y) # 1.544304
var(y) # 3.701846
mean(y)^2/(var(y)-mean(y)) # 1.105366 <- alpha or 1/r (see wikipedia)
# generate the probability density for each count bw 0 and 11 with dnbinom, using alpha as size and mean as mu
xs<-0:11
ys<-dnbinom(xs,size=1.105,mu=1.5443)
lines(xs,ys*158)

# density estimation for continuous variables
library(MASS)
attach(faithful)
(max(eruptions)-min(eruptions))/(2*(1+log(length(eruptions),base=2))) # 0.192573
# the rule of thumb for bandwidth gave us around 0.2, but 0.6 looks much better
par(mfrow=c(1,2))
hist(eruptions,15,freq=F,main="",col=27)
lines(density(eruptions,width=0.6,n=200))
truehist(eruptions,nbins=15,col=27)
lines(density(eruptions,n=200))
# NB: we asked for 15 bins but got 18.  Also note how bar heights differ between the charts

par(mfrow=c(1,1))

# index plots - use to find a data entry mistake (in this case, an incorrect decimal placement)
response<-read.table('das.txt',header=T)
plot(response$y)
which(response$y > 15) # find error point's index
response$y[50] # 21.79386 is value of error point
response$y[50]<-2.179386 # replace with the correct value
plot(response$y) # replot

# time series plots using plot.ts and ts.plot
data(UKLungDeaths)
ts.plot(ldeaths,mdeaths,fdeaths,xlab="year",ylab="deaths",lty=c(1:3))
# upper line is total deaths, middle is male deaths, bottom is female deaths
# deaths clearly peak in midwinter, and less women die of lung cancer than men

# plot a time series using plot
data(sunspots)
plot(sunspots) # works bc class is time series
class(sunspots) # ts

# pie charts
pie.data<-read.csv('piedata.csv',header=T)
pie.data
pie(pie.data$amounts,labels=as.character(pie.data$names))

# stripchart function - for samples too small to box-and-whisker
data("OrchardSprays")
with(OrchardSprays, stripchart(decrease~treatment, ylab="decrease", vertical=T,log="y"))

# plots w multiple variables
# pairs checks every variable against every other variable in x y combos to easily look for dependencies
ozonedata<-read.table('ozone.data.txt',header=T)
attach(ozonedata)
names(ozonedata)
pairs(ozonedata,panel=panel.smooth)
# note strong relationships between temp x ozone and ozone x wind

# coplot : see p171-2
coplot(ozone~wind|temp,panel=panel.smooth, overlap=0.25)

# interaction plots : see p172-3
yields<-read.table('splityield.txt', header=T)
attach(yields)
interaction.plot(fertilizer,irrigation,yield)

# Trellis Plots
library(lattice)
# panel plot
panels<-read.table('panels.txt',header=T)
attach(panels)
xyplot(weight ~ age | gender)
# b&w plot for 3 way ANOVA
daph<-read.table('daphnia.txt',header=T)
attach(daph)
trellis.par.set(col.whitebg())
bwplot(Growth.rate~Water+Daphnia|Detergent)
# design plots - need to set things as factors
plot.design(Growth.rate~as.factor(Water)*as.factor(Detergent)*as.factor(Daphnia),fun="sd") # default function is mean

# effect sizes
library(effects)
?effects
model<-lm(Growth.rate~Water*Detergent*Daphnia)
d.effects<-allEffects(model)
plot(d.effects,"Water:Detergent:Daphnia")

# bubble plots - from scratch
ddd<-read.table('pgr.txt',header=T)
attach(ddd)
bubble.plot<-function(xv,yv,rv,bs=0.1){
  r<-rv/max(rv)
  yscale<-max(yv)-min(yv)
  xscale<-max(xv)-min(xv)
  plot(xv,yv,type="n",xlab=deparse(substitute(xv)),ylab=deparse(substitute(yv)))
  for (i in 1:length(xv)) bubble(xv[i],yv[i],r[i],bs,xscale,yscale)
}
bubble<-function(x,y,r,bubble.size,xscale,yscale) {
  theta<-seq(0,2*pi,pi/200)
  yv<-r*sin(theta)*bubble.size*yscale
  xv<-r*cos(theta)*bubble.size*xscale
  lines(x+xv,y+yv)
}
bubble.plot(hay,pH,FR)

# finally, sunflower plots - a petal for every value at a point
nums<-read.table('longdata.txt',header=T)
attach(nums)
sunflowerplot(xlong,ylong)




# DONE !!!




