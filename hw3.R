#Jonathan Walton
#March 2, 2017
#STAT 4004
#Homework 3

rm(list=ls())
getwd()
setwd('C:/Users/Jonathan/Documents/School/STAT/Stat Computing/HWs/HW3')

library(twitteR)
library(plyr)
library(ggplot2)
library(lubridate)

#1
setup_twitter_oauth(consumer_key="ewK8uuwlNCIZ3t7VVF269TwqS", 
                    consumer_secret="U4tcnwNyZY715yZDs4pQ3ucvotP3XIeAKakVbkXCnAZbjm5ujv")

searchTerms<-c("@verizon", "@ATT", "@Tmobile",'@sprint')
names(searchTerms)<-searchTerms

searchResults<-lapply(searchTerms, function(tt){
  print(tt)
  searchTwitter(searchString=tt, n=3000, lang='en')
})
class(searchResults)
searchResults[1]

for (i in 1:length(searchTerms)){
  text.result[[i]] =  laply(searchResults[[i]], function(t) t$getText())
}
names(text.result) = names(searchTerms)
View(text.result[[1]][])

#2
hu.liu.pos = scan(file='positive-words.txt',what='character', comment.char=';')
hu.liu.neg = scan(file='negative-words.txt',what='character', comment.char=';')

# Add a few twitter and industry favorites that are specific for the topic.
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')

for (i in 1:length(searchTerms)){
  score.result[[i]]=sentiment_scores(text.result[[i]], pos.words, neg.words, .progress='text')
}
names(score.result) = names(searchTerms)

win.graph()
par(mfrow=c(2,2))
hist(score.result[[1]]$score,col= 'blue',xlim=c(-5,5),ylim=c(0,700), main='Verizon',breaks=15)
hist(score.result[[2]]$score, col='blue', xlim=c(-5,5),ylim=c(0,700), main='ATT',breaks=15)
hist(score.result[[3]]$score, col='blue', xlim=c(-5,5),ylim=c(0,700), main='TMobile',breaks=15)
hist(score.result[[4]]$score, col='blue', xlim=c(-5,5),ylim=c(0,700), main='Sprint',breaks=15)

#3
very.pos = c(sum(score.result[[1]]$score>=2), sum(score.result[[2]]$score>=2), sum(score.result[[3]]$score>=2), sum(score.result[[4]]$score>=2))
very.neg = c(sum(score.result[[1]]$score<=-2), sum(score.result[[2]]$score<=-2), sum(score.result[[3]]$score<=-2), sum(score.result[[4]]$score<=2))
very.total = very.pos+very.neg
very.score = round(100*very.pos/very.total)
SCORE = cbind(very.pos,very.neg,very.total,very.score)
rownames(SCORE) = names(searchTerms)
View(SCORE)

library(XML)
satis<-htmlParse("http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Wireless+Telephone+Service")

satis.table<- readHTMLTable(satis,stringsAsFactors = FALSE)
class(satis.table)
length(satis.table)

table = satis.table[[1]]
View(table)

ACSI=c(table[which(table[,1]=='Verizon Wireless'),which(colnames(table)=='16')],
       table[which(table[,1]=='AT&T Mobility'),which(colnames(table)=='16')],
       table[which(table[,1]=='T-Mobile'),which(colnames(table)=='16')],
       table[which(table[,1]=='Sprint'),which(colnames(table)=='16')])
Output=cbind(SCORE,ACSI)
View(Output)