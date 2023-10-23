# Ch 6 - Tables - p183-93
# 10/11 PM

# Summary tables
daph<-read.table('Daphnia.txt', header=T)
attach(daph)
# simple tables
tapply(Growth.rate,Detergent,mean)
tapply(Growth.rate,Water,mean)
tapply(Growth.rate,Daphnia,mean)
# find se's of means with an anonymous function inside of tapply
tapply(Growth.rate,list(Daphnia,Detergent),function(x)sqrt(var(x)/length(x)))
# 3D table - first var is rows, second is cols, third is separate tables
tapply(Growth.rate,list(Daphnia,Detergent,Water),mean)
# ftable (flat table) does 3D more prettily - note that order is alphabetical for axes
ftable(tapply(Growth.rate,list(Daphnia,Detergent,Water),mean))

# NB : need to add as.factor in new R version - not in textbook
# create a new dataframe with summary parameters estimated from the larger dataframe
# convert to factor and then to number before using tapply
dets<-as.vector(tapply(as.numeric(as.factor(Detergent)),list(Detergent,Daphnia),mean))
clones<-as.vector(tapply(as.numeric(as.factor(Daphnia)),list(Detergent,Daphnia),mean))
means<-as.vector(tapply(Growth.rate,list(Detergent,Daphnia),mean))
detergent<-levels(as.factor(Detergent))[dets]
daphnia<-levels(as.factor(Daphnia))[clones]
(daphMeans<-data.frame(means,detergent,daphnia))
# alternatively, do it with as data frame table and then change the names later
adft.daphmeans<-as.data.frame.table(tapply(Growth.rate,list(Detergent,Daphnia),mean))
names(adft.daphmeans)<-c("detergents","daphnia","means")
adft.daphmeans


# Tables of Counts
cells<-rnbinom(10000,size=0.63,prob=0.63/1.83)
table(cells)
gender<-rep(c("m","f"),c(5000,5000))
table(cells,gender)
tapply(cells,gender,mean)


# expanding a table to a dataframe
countTable<-read.table('tabledata.txt',header=T)
attach(countTable)
countTable
# create a new df with a separate row for each case
dbtable<-as.data.frame(lapply(countTable,function(x)rep(x,countTable$count)))
# vector of counts is redundant, so remove it
dbtable<-dbtable[,-1]
dbtable

# converting from a dataframe to a table
table(dbtable) # simple as
# turn it back into a dataframe, why the hell not
frame<-as.data.frame(table(dbtable))
names(frame)[4]<-"count"
frame

# calculating tables of proportions
counts<-matrix(c(2,2,4,3,1,4,2,0,1,5,3,3),nrow=4)
counts
prop.table(counts,1) # row props
prop.table(counts,2) # col props
prop.table(counts) # full table props
sum(prop.table(counts)) # =1

# we can scale values within a column so that they have a mean of 0, or find means and sd's of columns, using the scale function
scale(counts)
apply(counts,2,sd) # another way to find SDs of columns

# use expand.grid to generate tables of combos of factor levels
expand.grid(height=seq(60,80,5), weight=seq(100,300,50), sex=c("male","female"))

# model.matrix can be used for dummy variable table creation - see p192-3



# DONE!!


