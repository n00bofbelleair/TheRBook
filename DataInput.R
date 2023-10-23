# done on 10/5 in PM

# use scan function for data input in console
x<-scan()

# read in a file from the directory
data<-read.table('RBookFiles/regression.txt',header=T)

# browse to find a file to use
dataset<-read.table(file.choose(), header=T)

# attach in murders.txt and stop regions from being converted to factors using as.is
murder<-read.table('RBookFiles/murders.txt', header=T, as.is="region")
attach(murder)
table(region)
detach(murder)

rm(dataset)
# setwd to file folder of txt stuff
setwd('RBookFiles')
daph<-read.table('daphnia.txt',header=T)

murders<-scan('murders.txt',skip=1,what=list("","","",""))
class(murders) # list
murder.frm<-as.data.frame(murders)
murder.names<-scan('murders.txt',nlines=1,what="character",quiet=T)
murder.names
names(murder.frm)<-murder.names
murder.frm[,2]<-as.numeric(murder.frm[,2])
murder.frm[,3]<-as.numeric(murder.frm[,3])
summary(murder.frm)
murds<-read.table('murders.txt',header=T)
summary(murds)
# not rly sure what happened here; p102-3


line.number<-length(scan('rt.txt',sep="\n"))
(my.list<-sapply(0:(line.number-1), function(x) scan('rt.txt',skip=x,nlines=1,quiet=T)))
unlist(lapply(my.list,length))


# DONEdundundun 