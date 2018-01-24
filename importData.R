#packages
library(tidyverse)
library(dplyr)
library(recommenderlab)

#set working directory
setwd("O:/Documents/Personal/Projects/books")
#setwd("C:/Users/LauraAcer/Documents/Data Science/Books")

#read in data
books<-read_csv("goodreads_library_export.csv")
books<-as.data.frame(books)
books<-books[c("Book Id","Title","My Rating","Exclusive Shelf")]
colnames(books)<-c("id","btitle","rrating","shelf")
books<-books[order(books$id),]
books<-subset(books,books$shelf=="read" | books$shelf=="to-read" | books$shelf=="to-acquire")
books$ruser<-"Me:21580571"
books$rtitle<-paste(books$btitle,books$id,sep=":")
books$n<-ave(books$rrating,books$id,FUN=seq_along)
books<-books[c("ruser","rtitle","btitle","rrating","id","n")]

#booklist
booklist<-books[c("id","btitle")]
save(booklist,file="booklist.Rda")

#API
#packages
#install.packages("devtools")
#devtools::install_github("famguy/rgoodreads")

#goodreads API
library(devtools)
library(rgoodreads)
Sys.setenv(GOODREADS_KEY="7U8VDuR3phc4vD1WQF1g")

#API request results in the form of an nxm matrix outputting reviews systematically
#at fixed intervals to avoid losing data if system crashes under weight of processing
n<-50000
m<-10
ruser<-matrix(rep(0,n),nrow=n,ncol=m)
rtitle<-matrix(rep(0,n),nrow=n,ncol=m)
btitle<-matrix(rep(0,n),nrow=n,ncol=m)
rrating<-matrix(rep(0,n),nrow=n,ncol=m)
id<-matrix(rep(0,n),nrow=n,ncol=m)
for (i in 1:n) {
  #pull data from goodreads API
  for (j in 1:m){
    ri<-j+(i-1)*m
    print(paste0("request number: ",ri))
    tryCatch({
      #reviews
      c1<-review(ri)
      c2<-book(gsub(".*:","",c1$book))
      c2$id<-as.numeric(c2$id)
      if (c2$id %in% booklist$id) {
        id[i,j]<-c2$id
        ruser[i,j]<-c1$user
        rtitle[i,j]<-c1$book
        btitle[i,j]<-c2$title
        rrating[i,j]<-as.numeric(as.character(c1$rating))
        rbooks<-data.frame(ruser[,j],rtitle[,j],btitle[,j],rrating[,j],id[,j])
        colnames(rbooks)<-c("ruser","rtitle","btitle","rrating","id")
        rbooks<-subset(rbooks,id!=0)
        rbooks$n<-ave(rbooks$rrating,rbooks$id,FUN=seq_along)
        #saves data to file
        nam<-paste("rbooks",j,sep="")
        assign(nam,rbooks)
        rdaname<-paste("rbooks",j,".Rda",sep="")
        save(rbooks,file=rdaname)
        csvname<-paste("rbooks",j,".csv",sep="")
        write.csv(rbooks,csvname)
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
reviews<-eval(parse(text=paste0("rbind(",toString(paste0("rbooks",1:m)),")")))
reviews<-rbind(reviews,books)
reviews<-reviews[order(reviews$id),]

#saves data
save(reviews,file="reviews.Rda")
write.csv(reviews,"reviews.csv")

