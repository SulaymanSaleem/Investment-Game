rm(Data)
#reading the file
setwd("E:/Sulayman's Stuff/My Projects")
#setting up the initial dataset
Data1<-read.delim("Online Investment/Stocks/stock1.txt", header=TRUE, sep = ",")
Data2<-read.delim("Online Investment/Stocks/stock1.txt", header=TRUE, sep = ",")
Data3<-read.delim("Online Investment/Stocks/stock1.txt", header=TRUE, sep = ",")
#Only interested if the stock went up or down, not by how much.

s1<-Data1[1:100,1:7]
s2<-Data2[1:100,1:7]
s3<-Data3[1:100,1:7]
#For this reason we only want to calculate the percentage change in the stock.
#If the percentage change is less than one, than the stock went down.
s1$ratio <- s1$Close/s1$Open
s2$ratio <- s2$Close/s2$Open
s3$ratio <- s3$Close/s3$Open

rm(Data1,Data2,Data3)

#Now I'm going to create a new column to indicate whether the stock went up or down on the day.
#If it went up, the row calue will be 1, if down then 0.
s1$ind<-0
s2$ind<-0
s3$ind<-0
for (i in 1:nrow(s1)) {
  if(s1[i,8]>1){
    s1[i,9]=1
  }
  
  if(s2[i,8]>1){
    s2[i,9]=1
  }  
  
  if(s3[i,8]>1){
    s3[i,9]=1
  }
  
}
### Dropping columns 1:7, they're not needed
s1 <- s1[ -c(1:7) ]
s2 <- s2[ -c(1:7) ]
s3 <- s3[ -c(1:7) ]
### All functions are below ###

normalise <-function(v) {
  
  x<-0
  for(i in 1:length(v)){
  a<-v[i]
  x[i]<-a/sum(v)
  }
  x
}

wealth <-function(vector,portfolio){
  w=0
  for(i in 1:length(vector)){
    w<-vector[i]*portfolio[i]
  }
  w
}
