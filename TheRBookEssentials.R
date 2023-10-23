# working through stuff in The R Book 

# packages used in this book
install.packages("akima")
install.packages("chron")
install.packages("lme4")
install.packages("mcmc")
install.packages("odesolve")
install.packages("spdep")
install.packages("spatstat")
install.packages("tree")

# use this to pull up the bacteria data to be edited
library(MASS)
attach(bacteria)
fix(bacteria)

par(ask=TRUE)

# create a function to round numbers since one is not naturally included in R
rounded<-function(x) floor(x+0.5)
rounded(pi)
rounded(pi*4)

# find mean of number set that has an NA value in it
x<-c(1:8,NA)
mean(x,na.rm=T)

which(is.na(x))
# the ninth value in x is an NA value, so this returns 9

# counts of each number are labeled by number and then printed (p16)
counts<-c(25,12,7,4,6,2,1,0,2)
names(counts)<-0:8
counts
# to remove the names, use as.vector
as.vector(counts)
# find 5 number summary
quantile(counts)

# calc number of not infected cases in the bacteria dataframe
with(bacteria,tapply((y=="n"),trt,sum))

# plot brain weight against body weight for mammals on log-log axes
with(mammals,plot(body,brain,log="xy"))

data()
# find all available data sets, including those in installed packages
data(package = .packages(all.available = TRUE))

# p23 - find every 25th value in a 1000-long vector of normal random numbers w mean 100 and sd 10
xv<-rnorm(1000,100,10)
xv[seq(25,length(xv),25)]
# find value closest to 108 like this - first find index of the sought out value, then search it up
which(abs(xv-108)==min(abs(xv-108)))
xv[651]
# alternatively, we can combine these to skip the index finding step
xv[which(abs(xv-108)==min(abs(xv-108)))]

# find mean of xv
mean(xv)
# length of xv is 1000; confirm like this
length(xv)
# find trimmed mean by removing the fifty most extreme values on each end
trim.mean<- function(x) mean(sort(x)[-c(50,length(x))])
trim.mean(xv)
mean(xv) - trim.mean(xv)
# mean is now nearly 0.0107 closer to 100, the true mean of an infinite vector created with mean 100 and sd 10

# drop all multiples of seven from a vector of 1 thru 50 (long form)
vecseven<-1:50
(multiples<-floor(50/7))
(subscripts<-7*(1:multiples))
vecseven[-subscripts]
# short form
vecsev<-1:50
vecsev[-(1:50*(1:50%%7==0))]

# logical arithmetic
six <- 0:6
six<4
all(six>0)
any(six<0)
sum(six<4)
# multiply (six<4) by another vector (7 random numbers between 0 and 1 here)
(six<4)*runif(7)

(treatment<-letters[1:5])
(ttwo <- factor(1+(treatment=="b")+2*(treatment=="c")+2*(treatment=="d")))



# evaluation of combos of true and false and NA
combos<-c(NA,FALSE,TRUE)
names(combos)<-as.character(combos)
outer(combos, combos, "&") # logical and
#        <NA> FALSE  TRUE
#<NA>     NA FALSE    NA
#FALSE FALSE FALSE FALSE
#TRUE     NA FALSE  TRUE
outer(combos, combos, "|") # logical or
#      <NA> FALSE TRUE
#<NA>    NA    NA TRUE
#FALSE   NA FALSE TRUE
#TRUE  TRUE  TRUE TRUE

# rep() function examples
rep(9,5)
rep(1:4,2)
rep(1:4,each=2)
rep(1:4,each=2,times=3)
rep(1:4,1:4)
rep(1:4,c(4,1,4,2))

# generate factor labels
# useful for encoding long vectors of factor levels
gl(4,3,24)
# above returns 1 thru 4 repeated 3 times per number for a vector of length 24 (ie the pattern occurs exactly twice)
# if total length isn't a multiple of the pattern length then the vector will be truncated
# to get text for factor levels, do this:
gl(3,2,24,labels=c("a","b","c"))

# create a vector of 18 random numbers w mean 10 and sd 2
x<-rnorm(18,10,2)
x.values<-seq(min(x),max(x),(max(x)-min(x))/100) #see p29
plot(x.values)
# produces sequence same length as x starting at 88 and going down to exactly 50 with even increments
seq(88,50,along=x)

# sequence (spelled out in full) can produce a vector consisting of sequences
sequence(5:1)
sequence(c(5,2,4))


attach(faithful)
names(faithful)
View(faithful)
ranks<-rank(eruptions) # 1 is assinged to lowest data value, and length(eruptions) is assigned to highest value
sorted<-sort(eruptions) # values of eruptions in order
ordered<-order(eruptions) # returns an integer vector containing the permutation that will sort the input into ascending order (p31-2)
view<-data.frame(eruptions, ranks, sorted, ordered)
# eruption-ranked list of waiting values, plotted (one by index and the other with a set x axis, but these are the same)
plot(waiting[order(eruptions)])
plot(1:272,waiting[order(eruptions)])






# 10/3

# take 4 numbers at random from 1:10 without replacement where the probability of selection (p) is 5 times greater for 5 and 6 than for 1 or 10
# then do this 5 times over
p<-c(1,2,3,4,5,5,4,3,2,1)
x<-1:10
sapply(1:5,function(i) sample(x,4,prob=p))

# Matrices
# create a 3x3 identity matrix
X<-matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
X
class(X)
attributes(X)
# use a vector to make a 2x4 matrix
vector<-c(1,2,3,4,4,3,2,1)
V<-matrix(vector,byrow=T,nrow=2)
V
# convert the vector into a 4x2 matrix, then transpose it to a 2x4
dim(vector)<-c(4,2)
vector
(vector<-t(vector))

# naming rows and columns of matrices
dtrial<-matrix(rpois(20,1.5),nrow=4)
dtrial
dimnames(dtrial)<-list(paste("Trial.",1:4,sep=""),NULL) # before comma affects row names, after affects col names
drug.names<-c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(dtrial)<-drug.names
dtrial

# mean of rightmost column
mean(dtrial[,5])
# var of bottom row
var(dtrial[4,])
# other calcs
rowSums(dtrial)
colSums(dtrial)
rowMeans(dtrial)
colMeans(dtrial)
# also can use apply (rows are margin 1, cols are margin 2)
apply(dtrial,2,mean) # =colMeans(dtrial)

# sum groups of rows within columns using rowsum
group=c("A","B","B","A")
rowsum(dtrial,group)

# shuffle the elements of each column of a matrix independently
apply(dtrial,2,sample)

# add rows or columns to matrix
dtrial<-rbind(dtrial,apply(dtrial,2,mean))
dtrial<-cbind(dtrial,apply(dtrial,1,var))
dtrial
colnames(dtrial)<-c(1:5,"variance")
rownames(dtrial)<-c(1:4,"mean")
dtrial

dtrial[dtrial==0.29375]<-NA
dtrial

# open sweepdata to practice sweep function
matdata<-read.table("H:/My Drive/R Work/TheRBook/RBookFiles/sweepdata.txt")
# create a vector of the four column means
(cols<-apply(matdata,2,mean))
# then sweep to express all the data in matdata as departures from the relevant column means
sweep(matdata,2,cols)
# sweep can also be used to make all rows be the row index # or cols be the col index #
sweep(matdata,1,1:10,function(a,b) b)
sweep(matdata,2,1:4,function(a,b) b)


# Arrays
# an array can be turned to a matrix by adding dimensions (values will be added column-wise)
array<-1:25
dim(array)<-c(5,5)
array
# create 3 matrices of 4 rows and 2 columns containing the first 24 lowercase letters
A<-letters[1:24]
dim(A)<-c(4,2,3)  
A
# to select letters a thru p, select matrices 1 and 2
A[,,1:2]
# now select c,g,k,o,s,w (the third rows of all three tables) - these will become one matrix
A[3,,]
# do the same but keep the letters in their separate matrices
A[3,,,drop=F]
# suppose we want all rows of col 2 in table 1, first col of table 2, and second col of table 3
# we want all rows in each case (so subscript 1 is blank) but want different columns in different tables
cs<-c(2,1,2)
ts<-c(1,2,3)
# use sapply to concatenate the columns from each table together now
sapply(1:3,function(i) A[,cs[i],ts[i]])

# character strings
pets<-c("cat","dog","gerbil","terrapin")
length(pets) # is 4
nchar(pets) # returns 3 3 6 8
# character strings are not factors when first defined, but they can become factors if made into a dataframe (p44)

# find number in alphabet of a letter
which(letters=="n") # 14
which(LETTERS=="P") # 16
noquote(letters) # shows character string without quotes on each item
a<-"123"
b<-"abc"
c(a,b) # displays a vector of two strings
paste(a,b,sep=" ") # converts two strings into one
# if an argument in paste is a vector then multiple objects will be produced
d<-c(b,a,"tehehe")
e<-paste(d,"do rei mi")
e

# substrings of expanding length 
phrase<-"the five boxing wizards jump quickly"
q<-character(23)
for (i in 1:23) q[i]<-substr(phrase,1,i)
q
# split a string by character
strsplit(phrase,split=character(0))
# use a table to count occurences of each character
table(strsplit(phrase,split=character(0)))
# count words in phrase thusly
words<-1+table(strsplit(phrase,split=character(0)))[1]
words # counts number of blanks plus 1 

nchar(phrase) # 36
strsplit(phrase,"i") # split where letter i occurs and remove letter i
strsplit(phrase,"i")[[1]][2] # find characters between 1st and 2nd i
nchar(strsplit(phrase,"i")[[1]][2]) # find number of characters in that string
PHRASE<-toupper(phrase) # make a version of phrase that is uppercase all across the board
PHRASE



# match function
# matches where items in second vector appear in first vector
second<-c(0,1,2,3)
first<-c(9,3,0,2,5,7,2,0,8,3,7,1)
match(first,second)
# can use with is.na to assign values - ex to give drug A to patients in first vector who were also in the second vector
drug<-c("A","B")
drugassignments<-drug[1+is.na(match(first,second))]
drugassignments



# Writing Functions in R
# start with a simple one - finding the arithmetic mean (which is already included in R)
arith.mean<-function(x) sum(x)/length(x)
y<-c(3,3,4,5,5)
arith.mean(y)
arith.mean(y)==mean(y) # TRUE, so this worked

# median of a sample
med<-function(x){
  odd.even<-length(x)%%2
  if(odd.even==0) (sort(x)[length(x)/2]+sort(x)[1+length(x)/2])/2
  else sort(x)[ceiling(length(x)/2)]
}
med(y) # =4
z<-c(1,2,3,4,5,6)
med(z) # =3.5
median(y) # =4
median(z) # =3.5

# geometric mean is good for samples with multiplicative change rates
insects<-c(1,10,1000,10,1)
mean(insects) # basically useless (204.4) since it isn't close to any values in the data set
geometric<-function(x) exp(mean(log(x)))
geometric(insects) # =10

# harmonic mean - the recip of the average of the recips
harmonic<-function(x) 1/mean(1/x)
harmonic(c(1,2,4,1)) # =1.454545

# variance - sum of squares divided by degrees of freedom
v<-c(13,7,5,12,9,15,6,11,9,7,12)
variance<-function(x) sum((x-mean(x))^2)/(length(x)-1)
variance(v) # 10.25455
variance(v)==var(v) # TRUE


# variance ratio, or p value
var.ratio<-function(x,y) {
  v1<-var(x)
  v2<-var(y)
  if(var(x)>var(y)) {
    vr<-var(x)/var(y)
    df1<-length(x)-1
    df2<-length(y)-1
  } else {
    vr<-var(y)/var(x)
    df1<- length(y)-1
    df2<- length(x)-1
  }
  2*(1-pf(vr,df1,df2))
}
# test p value finder
a<-rnorm(10,15,2)
b<-rnorm(10,15,4)
var.ratio(a,b) # 0.2216362
var.test(a,b) # p-value = 0.2216


# Using Variance

# there is no built in standard error function, but one is easy to write
se<-function(x) sqrt(var(x)/length(x))
# also write a confidence interval function for 95% CI
ci95<-function(x) {
  t.value<-qt(0.975,length(x)-1)
  standard.error<-se(x)
  ci<-t.value*standard.error
  cat("95% CI = ", mean(x)-ci,"to ",mean(x)+ci,"\n")
}
# now test it with a sample size of 150
x<-rnorm(150,25,3)
ci95(x)
se(x)

# generate a data set from which progressively larger samples will be taken
xv<-rnorm(30)
# find se of sets with sample size 2 thru 30, then graph
sem<-numeric(30)
sem[1]<-NA
for(i in 2:30) sem[i]<-se(xv[1:i])
plot(1:30,sem,ylim=c(0,0.8), lines(2:30,1/sqrt(2:30)),
     ylab="se of mean", xlab="sample size n", pch=16)



# Loops and Repeats
# basic for loop that repeats a print command 5 times, modifying variable values each time
j<-k<-0
for(i in 1:5) {
  j<-j+1
  k<-k+i*j
  print(i + j + k)
}
# use a for loop to calculate factorial x
fac1<-function(x) {
  f <- 1
  if (x<2) return (1)
  for (i in 2:x) {
    f <- f*i
    f}}
sapply(0:5,fac1)
# above not working

fac2<-function(x){
  f<-1
  t<-x
  while(t>1){
    f<-f*t
    t<-t-1
  } 
  return(f)
}
sapply(0:5,fac2)
# working

fac3<- function(x) {
  f <- 1
  t <- x
  repeat {
    if (t<2) break
    f <- f*t
    t <- t-1
  }
  return(f)
}
sapply(0:5,fac3)
# working
# see p60 for more factorial nonsense

# convert a number to binary
binary<-function(x) {
  i<-0
  string<-numeric(32)
  while(x>0) {
    string[32-i]<-x%%2
    x<-x%%2
    i<-i+1} 
  first<-match(1,string)
  string[first:32]}
# not working

# fibonacci sequence
fibo<-function(n) {
  a<-1
  b<-0
  while(n>0)
  {swap<-a
  a<-a+b
  b<-swap
  n<-n-1}
b }
sapply(1:10,fibo) # first ten Fibonacci digits
sapply(20,fibo) # 20th digit



# ifelse

# use ifelse to create a new two-level factor from a continuous variable
data<-read.table('worms.txt',header=T)
attach(data)
ifelse(Area>median(Area),"big","small")

# to represent log of 0 with NA, we can also use ifelse
y<-log(rpois(20,1.5))
ifelse(y<0,NA,y)

# how long to find mean(x)
x<-runif(100000000)
system.time(mean(x))
mean(x)


# use switch - here to calculate 4 measures of central tendency in one function
central<-function(y,measure) {
  switch(measure,
         Mean=mean(y),
         Geometric=exp(mean(log(y))),
         Harmonic=1/mean(1/y),
         Median=median(y),
        stop("Measure not included"))
}
central(rnorm(100,10,2),"Harmonic")
central(rnorm(100,10,2),4) # 4 indicates 4th option in switch (median)

# example of a function with optional arguments 
charplot<-function(x,y,pc=16,co="red"){
  plot(y~x,pch=pc,col=co)
}
charplot(1:10,c(rnorm(10,10,2)))
# as you can see, this function has default arguments for the color and style of plot points, but requires x and y arguments
charplot(1:10,1:10,18,"limegreen") # here we use all 4 possible arguments


# Variable number of arguments (...) function to return UNLIMITED MEANS AND VARS
many.means<-function(...) {
  data<-list(...)
  n<-length(data)
  means<-numeric(n)
  vars<-numeric(n)
  for (i in 1:n) {
    means[i]<-mean(data[[i]])
    vars[i]<-var(data[[i]])
  } 
  print(means)
  print(vars)
  invisible(NULL)
}
x<-rnorm(100)
y<-rnorm(200)
z<-rnorm(300)
many.means(x,y,z)

a<-c(1,9,2,8,3,7)
b<-c(9,2,8,3,7,2)
# find median values of both parallel maxima and minima in same function
parboth<-function(a,b) {
  c<-pmax(a,b)
  d<-pmin(a,b)
  answer<-list(median(c),median(d))
  names(answer)[[1]]<-"median of parallel maxima"
  names(answer)[[2]]<-"median of parallel minima"
  return(answer)
}
parboth(a,b)

# we can set up a function to make one or both plots depending on arguments used
plotx2<-function(x,y=z^2){
  z<-1:x
  plot(z,y,type="b")
}
par(mfrow=c(1,2))
plotx2(12)
plotx2(12,1:12)





# apply, sapply, and lapply

# use a sample matrix X here for apply
(X<-matrix(1:24,nrow=4))
apply(X,2,sum) # we can use apply to sum the rows or columns (coluns summed here, hence the 2)
# we can also take the square roots (1 or 2 will determine the shape of the matrix)
apply(X,1,sqrt)
apply(X,2,sqrt)
# we can shuffle the rows or columns using sample without replacement
apply(X,1,sample)
apply(X,2,sample)
# we can also supply a function within apply
apply(X,1,function(x) x^2+x)

# use sapply for vectors
# here we create sequences of 1 thru 3 up to 1 thru 7
sapply(3:7,seq)

# GOOD EXAMPLE
  # use sapdecay data - we intend to use non-linear least squares to estimate the decay rate
  # at laptop: setwd('G:/My Drive/R Work/TheRBook/RBookFiles/')
  sapdecay<-read.table('sapdecay.txt',header=T)
  attach(sapdecay)
  names(sapdecay)
  # write a function to calculate sum of squares of differences b/w observed and predicted y, when provided with a specific value of a
  sumsq<-function(a,xv=x,yv=y){
    yf<-exp(-a*xv)
    sum((yv-yf)^2)
  }
  # a should be close to the negative of the value of x when linearly regressing log(y) against x
  lm(log(y)~x)
  # generate a range of values on either side of 0.058
  a<-seq(0.01,0.2,.005)
  a
  # use sapply to apply the sum of squares function for each value of a and plot the deviance against the parameter value for a
  plot(a,sapply(a,sumsq),type="l") #plot shows that a estimate is indeed near 0.06
  # to extract the min value of a we use min with subscripts
  a[min(sapply(a,sumsq))==sapply(a,sumsq)] # 0.055
  # now we can use this value of "a" to generate a smooth exp func to fit thru our scatter of data points
  plot(x,y)
  xv<-seq(0,50,0.1)
  lines(xv,exp(-0.055*xv))
  #streamline this procedure using optimize
  # write a function showing how sum squares depends on value of a
  fa<-function(a) sum((y-exp(-a*x))^2)
  optimize(fa,c(0.01,0.1))
  # value of a to minimize sum squares is 0.05538411
  # or we can minimize the sum of the absolute values of the residuals
  fb<-function(a) sum(abs(y-exp(-a*x)))
  optimize(fb,c(0.01,0.1))

  
  
  
  
  
# 10/5

# lists and lapply
# lapply uses
a<-c("a","b","c","d")
b<-c(1,2,3,4,4,3,2,1)
c<-c(T,T,F)
list.object<-list(a,b,c)
class(list.object)
list.object
lapply(list.object,length)
lapply(list.object,class)
lapply(list.object,mean)

# looking for runs of numbers within vectors
poisson<-rpois(150,0.7) # 150 rand nums from a Poisson dist w mean 0.7
poisson
rle(poisson) # run lengths and their values for poisson 
max(rle(poisson)[[1]]) # find longest run length (5)
which(rle(poisson)[[1]]==5) # then find position of this longest run length (29)
rle(poisson)[[2]][29] # then find its value (0)

# runs test
n1<-25
n2<-30
y<-c(rep(1,n1),rep(0,n2))
len<-numeric(10000)
for (i in 1:10000) len[i]<-length(rle(sample(y))[[2]])
quantile(len,c(0.025,0.975))


# Sets: union, intersect, and setdiff
setA<-c("a","b","c","d","e")
setB<-c("d","e","f","g")
union(setA,setB) # all unique values
intersect(setA,setB) # all values in both
setdiff(setA,setB) # A minus values also in B
setdiff(setB,setA) # B minus values also in A
setequal(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA)),union(setA,setB)) # TRUE
setA %in% setB # F F F T T
setB %in% setA # T T F F
setA[setA %in% setB] # d e


# pattern matching
wf<-read.table('RbookFiles/worldfloras.txt',header=T)
attach(wf)
names(wf)
Country # list 161 countries
as.vector(Country[grep("R",as.character(Country))]) # find countries w uppercase R
as.vector(Country[grep("^R",as.character(Country))])# find countries STARTING WITH uppercase R
as.vector(Country[grep("y$",as.character(Country))]) # countries ending in y
as.vector(Country[grep("^[C-E]",as.character(Country))]) # countries starting with uppercase C thru E, inclusive
as.vector(Country[-grep("[A-T a-t]$",as.character(Country))]) # countries not ending in uppercase or lowercase a-t, inclusive (use - on grep)
as.vector(Country[grep("^.{5}y",as.character(Country))]) # countries with lowercase y as their 6th character
as.vector(Country[grep("^.{,3}$",as.character(Country))]) # countries with 4 or less characters in name
as.vector(Country[grep("^.{15,}$",as.character(Country))]) # countries with more than 15 characters in name


# subing text w character strings
text<-c("arm","leg","head","foot","hand","hindleg","elbow")
gsub("h","H", text) # change all h to be H
sub("o","O",text) # change first o in string to O
gsub("^.","O",text) # make first letter of every string a capital O

# for fun - piglatinish
library("stringr")
first<-str_sub(text,nchar(text)-1,nchar(text))
second<-str_sub(text,1,nchar(text)-2)
secay<-paste(second,"ay",sep="")
pig<-paste(first,secay,sep="")
pig

# continue in book work
gsub("(\\w)(\\w*)", "\\U\\1\\L\\2",text,perl=T) # capitalize first letter of each word
gsub("(\\w*)", "\\U\\1",text,perl=T) # cap all letters
# where does o appear in text
text[grep("o",text)] # words with o
grep("o",text) # positions with o

# number of o's per string in text
freq<-as.vector(unlist (lapply(gregexpr("o",text),length)))
present<-ifelse(regexpr("o",text)<0,0,1)
freq*present

# charmatch returns position of match for one match, or 0 for multiple matches, or NA for no matches
charmatch("med",c("mean","median","mode"))

# using %in% and which
stock<-c("car","van")
requests<-c("truck","suv","van","sports","car","waggon","car")
which(requests %in% stock) # positions 3,5,7
which(sapply(requests, "%in%", stock)) # shows what exactly from requests is in stock as well as its position in requests

# more on pattern matching: p84-87

# Testing and Coercing in R - p87-88
# turn letters into numbers thusly
as.numeric(factor(c("a","b","c")))

# Dates and Times
Sys.time() # full time and date
substr(as.character(Sys.time()),1,10) # just date
substr(as.character(Sys.time()),12,19) # just time
unclass(Sys.time()) # seconds since 1 Jan 1970
date() # more readable date and time format
date<-as.POSIXlt(Sys.time())
date$wday # day of week - 0 is sunday
date$mday # month of year - 0 is January
date$yday # day of year - 0 is Jan 1
unlist(unclass(date)) # view all components of date

as.numeric(difftime("2023-10-05","2000-09-20")) # I have been alive for 8415 days according to this
# practice by writing code to calculate days alive at any point that code is run
paste("I have been alive for", as.numeric(difftime(substr(as.character(Sys.time()),1,10),"2000-09-20")), "days!")

# convert excel dates to R format
excel.dates<-c("20/09/2000","11/09/2001","01/03/2020")
(Rdate<-strptime(excel.dates,format="%d/%m/%Y"))

time<-read.table('RBookFiles/times.txt',header=T)
time
attach(time)
duration<-as.difftime(paste(hrs,min,sec,sep=":"))
duration
tapply(duration,experiment,mean)

# for funzies make prime finder
prime<-function(n) {
  if (n==2) {
    TRUE
  } else if (any(n %% 2:(n-1)==0)) {
    FALSE
  } else {
    TRUE
  }
  }
sapply(730:739,prime)


# DONE !!




