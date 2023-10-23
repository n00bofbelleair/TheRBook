# Dataframes chapter 4 - p107-134
# started 10/6, finished 10/9

# need to read in strings as factors to use summary later
worms<-read.table('RBookFiles/worms.txt', header=T, stringsAsFactors = T)
attach(worms)
names(worms)
worms
summary(worms)

by(worms,Vegetation,mean) # not working
aggregate(worms, list(Vegetation = Vegetation), mean)

worms[3,] # show all of row 3 only
worms[,c(1,5)] # concat together columns 1 and 5

worms[sample(1:20,8),] # select 8 random rows - by default there is no replacement
worms[order(Slope),] # order by slope

# order by vegetation, then within that order by worm density, and show columns 4,7,5,3 in that order
worms[order(Vegetation,Worm.density),c(4,7,5,3)]

worms[Damp==T,] # select rows where damp is true
# select rows with above meadian worm density and soil pH less than 5.2
worms[Worm.density > median(Worm.density) & Soil.pH < 5.2,]
worms[,sapply(worms,is.numeric)] # show only numeric columns
worms[!(Vegetation=="Grassland"),] # select rows that are not grassland


data<-read.table('RBookFiles/worms.missing.txt', header=T, stringsAsFactors = T)
data
na.omit(data)
new.frame<-na.exclude(data)
complete.cases(data)

detach(worms)
# read in first column as row names (second example)
worm<-read.table('RBookFiles/worms.txt', header=T)
worms<-read.table('RBookFiles/worms.txt', header=T, row.names=1)
worms

# create data frame to bind together a few equal length objects
x<-runif(10)
y<-letters[1:10]
z<-sample(c(rep(T,5),rep(F,5)))
new<-data.frame(y,z,x)
new

# create table of counts of random int from a poisson dist, then convert to data frame
a<-rpois(1500,1.5)
table(a)
short<-as.data.frame(table(a))
short
# expand short so that there's a row for every distinct count, but drop col2 (Freq) before printing
long<-as.data.frame(lapply(short,function(x) rep(x, short$Freq)))
long[,1]

# elim duplicate rows
dups<-read.table('RBookFiles/dups.txt', header=T)
dups
unique(dups) # show only unique rows
dups[duplicated(dups),] # show duplicated row(s)

# dates in dataframes
nums<-read.table('RBookFiles/sortdata.txt', header=T)
attach(nums)
names(nums)
# extract d-m-y from date to dates
dates<-strptime(date,format="%d/%m/%Y")
dates<-sub(" ", "", dates)
# bind dates to nums dataframe
nums<-cbind(nums,dates)
# order first 4 columns by new 5th column
nums[order(as.character(dates)), 1:4]

# what veg types do our fields have?
unique(worms$Vegetation)
# read in herbicides for difference veg types
herbi<-read.table('RBookFiles/herbicides.txt', header=T)
# use match to decide what herbicides to use for each field and attach it to the worm dataframe
recs<-data.frame(worms,hbRec=herbi$Herbicide[match(worms$Vegetation,herbi$Type)])
recs

(lifeforms<-read.table('RBookFiles/lifeforms.txt', header=T))
(flowering<-read.table('RBookFiles/fltimes.txt', header=T))
merge(flowering,lifeforms) # combines complete rows 
(both<-merge(flowering,lifeforms,all=T)) # combines all rows, with NA where relevant
# seeds has genus and species values of both, but with headers 'name1' and 'name2'
(seeds<-read.table('RBookFiles/seedwts.txt',header=T))
# to merge, use by function to indicate which headers are the same as each other
merge(both,seeds,by.x=c("Genus","species"),by.y=c("name1","name2"))

(frame<-read.table('RBookFiles/sales.txt',header=T))
people<-rowMeans(frame[,2:5])
people<-people-mean(people)
people
(new.frame<-cbind(frame,people))
seasons<-colMeans(frame[,2:5])
seasons<-seasons-mean(seasons)
seasons
new.row<-new.frame[1,]
new.row[1]<-"seasonal effects"
new.row[2:5]<-seasons
new.row[6]<-0
(newnewframe<-rbind(new.frame,new.row))
# grand mean
gm<-mean(unlist(newnewframe[1:5,2:5]))
gm<-rep(gm,4)
newnewframe[1:5,2:5]<-sweep(newnewframe[1:5,2:5],2,gm)
newnewframe
# best per-season performance is shared by Jane and Robert who both sold 4.55 units more than the overall average in summer

aggregate(worm[,c(2,3,5,7)],by=list(Veg=worm$Vegetation),mean)


# DONE!!
