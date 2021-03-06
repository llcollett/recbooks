#packages
library(tidyverse)
library(dplyr)
library(recommenderlab)

#set working directory
setwd("O:/Documents/Personal/Projects/books")
#setwd("C:/Users/LauraAcer/Documents/Data Science/Books")

#data
load("reviews.Rda")
load("booklist.Rda")
colnames(booklist)<-c("id_r","title")

#data management
reviews$help<-1
reviews$rating<-as.numeric(reviews$rrating)
reviews$user<-as.numeric(gsub(".*:","",reviews$ruser))
reviews<-subset(reviews,rating!=0)
reviews<-reviews[order(reviews$user,reviews$id),]
reviews$user<-ave(reviews$user,reviews$help,FUN=function(x) as.numeric(factor(x)))
reviews<-reviews[order(reviews$id,reviews$user),]
reviews$id_r<-reviews$id
reviews$id<-ave(reviews$id,reviews$help,FUN=function(x) as.numeric(factor(x)))
ids<-unique(reviews[c("id_r","id")])
booklist<-merge(x=booklist,y=ids,by="id_r")
books<-reviews[c("user","id","rating")]
books<-books[order(books$user,books$id),]

#subset of books
subbooks<-books %>% 
  group_by(user) %>% 
  filter(n()>=3)
subbooks$help<-1
subbooks<-subbooks[order(subbooks$user,subbooks$id),]
subbooks$user<-ave(subbooks$user,subbooks$help,FUN=function(x) as.numeric(factor(x)))
subbooks<-subbooks[order(subbooks$id,subbooks$user),]
subbooks$id<-ave(subbooks$id,subbooks$help,FUN=function(x) as.numeric(factor(x)))
subbooks<-subbooks[c("user","id","rating")]
subbooks<-subbooks[order(subbooks$user,subbooks$id),]


#comparing algorithms
#only user ratings for users who have rated at least 3 books
subsparse<-sparseMatrix(i=subbooks$user,j=subbooks$id,x=subbooks$rating,
                        dims=c(length(unique(subbooks$user)),length(unique(subbooks$id))),
                        dimnames=list(paste("u",1:length(unique(subbooks$user)),sep=""),
                                      paste("b",1:length(unique(subbooks$id)),sep="")))
subreal<-new("realRatingMatrix",data=subsparse)

#compare
set.seed(5785)
scheme<-evaluationScheme(subreal[1:900],method="split",train=0.9,k=1,given=3,goodRating=5)
#algorithms
algorithms<-list("random items"=list(name="RANDOM",param=NULL),
                 "popular items"=list(name="POPULAR",param=NULL),
                 "user-based CF"=list(name="UBCF",param=list(nn=50)),
                 "item-based CF"=list(name="IBCF",param=list(k=50)),
                 "SVD approximation" = list(name="SVD", param=list(k = 50)))
results<-evaluate(scheme,algorithms,type="topNList",n=c(1,3,5,10,15,20))
plot(results,annotate=c(1,3),legend="bottomright")
plot(results,"prec/rec",annotate=3,legend="topleft")
#look up TPR, FPR, precision and recall meanings in terms of recommender systems


#recommender
#all user ratings
sparse<-sparseMatrix(i=books$user,j=books$id,x=books$rating,
                     dims=c(length(unique(books$user)),length(unique(books$id))),
                     dimnames=list(paste("u",1:length(unique(books$user)),sep=""),
                                   paste("b",1:length(unique(books$id)),sep="")))
real<-new("realRatingMatrix",data=sparse)

#correlation matrix to see similarity of users
sim_users<-similarity(real[1:50,],method="cosine",which="users")
sim_users
image(as.matrix(sim_users),main="User similarity")

#use popular items and user based collalaborative filtering
#run algorithms over first 4000 users
#popular items
r_pop<-Recommender(real[1:2000],method="POPULAR")
#i am user 2407, predict top 5 recommendations for me
rec_pop<-predict(r_pop,real[2407],n=5)
recbooks_pop<-as.data.frame(as(rec_pop,"list"))
recbooks_pop$id<-as.numeric(gsub("[b]","",recbooks_pop$u2407))
recbooks_pop<-recbooks_pop[c("id")]
recbooks_pop<-merge(booklist,recbooks_pop,by="id")

#UBCF
r_ubcf<-Recommender(real[1:2000],method="UBCF")
rec_ubcf<-predict(r_ubcf,real[2407],n=5)
recbooks_ubcf<-as.data.frame(as(rec_ubcf,"list"))
recbooks_ubcf$id<-as.numeric(gsub("[b]","",recbooks_ubcf$u2407))
recbooks_ubcf<-recbooks_ubcf[c("id")]
recbooks_ubcf<-merge(booklist,recbooks_ubcf,by="id")

