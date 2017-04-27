#Final Project 
#Amir Ghaderi 500794236  
#Mezbah Uddin 500793378
#Md Shariful Islam 500801351
#Full R-Codes

#Install Packages

install.packages("lubridate")
install.packages("TwitterR")
install.packages("plyr")
install.packages("wordcloud")
install.packages("tm",dependencies=TRUE)
install.packages("ggplot2")
install.packages("gridExtra")

#Load libriaries
library(twitteR)
library(lubridate)
library(plyr)
library(wordcloud)
library(tm)
library(ggplot2)
library(gridExtra)
library(RTextTools)
library(e1071)
library(plyr)
library(dplyr)
library(tm)
library(lubridate)
library(scales)
library(twitteR)
library(SnowballC)

#Load Access Information
consumer_key <-"6rXm5Qp7dSK6FzHfkF3PyohYt"
consumer_secret <-"zGHmPzgvOWe9ZTa66seWcTIoNU5TpoiJa6kLFLTvWhHcc0uFRi" 
access_token <-"267246937-3D6x0QvsbAzymz4bA3L37KFOulW3qzU2Wu4KsWm8"
access_secret <-"Wy8TdW5zBKnhrUgd6PKxtiBtNzLSbH2n2d592vwz3Ezuv"

#Establish a connection to twitter seach API
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Load data
data1 <- searchTwitter("@Delta", n=1000)
df1 <- do.call("rbind", lapply(data1, as.data.frame))

data2 <- searchTwitter("@WestJet", n=1000)
df2 <- do.call("rbind", lapply(data2, as.data.frame))

write.csv(df1,'delta.csv')
write.csv(df2,'westjet.csv')

#Preprocessing 

#Distribution of Characters
df1$charsintweet <- sapply(df1$text, function(x) nchar(x))
df2$charsintweet <- sapply(df2$text, function(x) nchar(x))

plot1 <- ggplot(data = df1, aes(x = charsintweet)) +
  geom_histogram(aes(fill = ..count..), binwidth = 8) +
  theme(legend.position = "none") +
  xlab("Characters per Tweet") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")+
  ggtitle("df1") 

plot2 <- ggplot(data = df2, aes(x = charsintweet)) +
  geom_histogram(aes(fill = ..count..), binwidth = 8) +
  theme(legend.position = "none") +
  xlab("Characters per Tweet") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")+
  ggtitle("df2") 

grid.arrange(plot1,plot2,ncol=2)

#Number of Unique users
df1_unique = length((unique(df1$screenName)))
df2_unique = length((unique(df2$screenName)))

df3 <- data.frame(Data_Sets = factor(c("d1f","df2")),levels=c("d1f","df2"), Users=c(df1_unique,df2_unique))

ggplot(data = df3, aes(x=Data_Sets, y=Users))+
  geom_bar(stat="identity",fill="midnightblue")+
  xlab("Data Sets")+
  ylab("Frequency")+
  ggtitle("Number of Unique Users")

#Percent retweeets comparision

df1_retweets <- length(df1$isRetweet[df1$isRetweet == TRUE])/1000
df2_retweets <- length(df2$isRetweet[df2$isRetweet == TRUE])/1000

df4 <- data.frame(Data_Sets = factor(c("d1f","df2")),levels=c("d1f","df2"), Users=c(df1_retweets,df2_retweets))

ggplot(data = df4, aes(x=Data_Sets, y=Users))+
  geom_bar(stat="identity",fill="midnightblue")+
  xlab("Data Sets")+
  ylab("Percentage")+
  scale_y_continuous(labels = percent)+
  ggtitle("Retweet Percentages")

#Only Keep tweets
d1 <- df1[,1]
d2 <- df2[,1]

#Export the test_data for labelling 
write.table(d1, "Delta",row.names=FALSE)
write.table(d2, "WestJet",row.names=FALSE)

#Import labeled test_data back
d1<- read.table("delta1.txt",header = FALSE, sep="")
d2 <- read.table("westjet1.txt",header = FALSE, sep="")

d1 <- d1[-1,]
d2 <- d2[-1,]

#Rename Columns
colnames(d1) <- c("text","pos","neg")
colnames(d2) <- c("text","pos","neg")

#Convert to numeric
d1$pos <- as.integer(d1$pos)
d1$neg <- as.integer(d1$neg)
d2$pos <- as.integer(d2$pos)
d2$neg <- as.integer(d2$neg)

d1$neg <- -1*d1$neg
d2$neg <- -1*d2$neg

#Keeping Only Positives and Negative Tweets
str(d1)
d1 <- subset(d1, (pos >=2 & neg==-1) | (neg<=-2 & pos ==1))
d2 <- subset(d2, (pos >=2 & neg==-1) | (neg<=-2 & pos ==1))

#Adding labels
d1$label[d1$neg<=-2] <- "negative"
d1$label[d1$pos>=2] <- "positive"

d2$label[d2$neg<=-2] <- "negative"
d2$label[d2$pos>=2] <- "positive"

table(d1$label)
table(d2$label)

#subsets
d1n<-subset(d1,label=="negative")
d2n<-subset(d2,label=="negative")
d1p<- subset(d1,label=="positive")
d2p<- subset(d2,label=="positive")
#grouped barplot in D3

#Text Analysis
textScrubber <- function(dataframe) {
  
  dataframe$text <-  gsub("-", " ", dataframe$text)
  dataframe$text <-  gsub("&", " ", dataframe$text)
  dataframe$text <-  gsub("[[:punct:]]", " ", dataframe$text)
  dataframe$text <-  gsub("[[:digit:]]", "", dataframe$text)
  dataframe$text <-  gsub("http\\w+", "", dataframe$text)
  dataframe$text <-  gsub("\n", " ", dataframe$text)
  dataframe$text <-  gsub("[ \t]{2,}", "", dataframe$text)
  dataframe$text <-  gsub("^\\s+|\\s+$", "", dataframe$text)
  dataframe$text <-  gsub("will", "", dataframe$text)
  dataframe$text <-  gsub("want", "", dataframe$text)
  dataframe$text <-  gsub("can", "", dataframe$text)
  dataframe$text <-  gsub("day", "", dataframe$text)
  dataframe$text <-  gsub("every", "", dataframe$text)
  dataframe$text <-  gsub("character","",dataframe$text)
  dataframe$text <-  tolower(dataframe$text)
  
  return(dataframe)
}

d1n <- textScrubber(d1n)
d2n <- textScrubber(d2n)
d1p <- textScrubber(d1p)
d2p <- textScrubber(d2p)

tdmCreator <- function(dataframe, stemDoc = F, rmStopwords = T){
  
  
  tdm <- Corpus(VectorSource(dataframe$text))
  if (isTRUE(rmStopwords)) {
    tdm <- tm_map(tdm, removeWords, stopwords())
  }
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  tdm <- TermDocumentMatrix(tdm,
                            control = list(wordLengths = c(3, 15)))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}

d1n <- tdmCreator(d1n)
d2n <- tdmCreator(d2n)
d1p <- tdmCreator(d1p)
d2p <- tdmCreator(d2p)
#Selects the 15 most used words.
d1n <- d1n[2:20,]
d2n <- d2n[2:20,]
d1p<- d1p[2:20,]
d2p<- d2p[2:20,]

#Create bar graph with appropriate colours
#and use coord_flip() to help the labels look nicer.
d1n_plot <- ggplot(d1n, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

d2n_plot <- ggplot(d2n, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))
grid.arrange(d1n_plot, d2n_plot, ncol=2)
d1p_plot <- ggplot(d1p, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

d2p_plot <- ggplot(d2p, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))
  
#There are other ways to get these plots
#side-by-side, but this is easy.
grid.arrange(d1p_plot, d2p_plot, ncol=2)

# Dataframe with relevant words
a<-c("Hate","delay","cancel","miss","problem","Fuck","never","bag","bomb","seat")
b<-c(10,8,7,6,6,5,5,4,4,4)
d1rn<-cbind(a,b)
d1rn<-as.data.frame(d1rn)
colnames(d1rn)<-c("text","frequency")
d1rn$frequency<-as.numeric(d1rn$frequency)
d1rn<-d1rn[with(d1rn, order(-frequency)), ]
c<-c("runway","spray","hassle","access","delay","board","seat","luggage","charging","issue")
d<-c(24,23,8,8,6,5,3,3,3,3)
d2rn<-cbind(c,d)
colnames(d2rn)<-c("text","frequency")
d2rn<-as.data.frame(d2rn)
d2rn$frequency<-as.numeric(d2rn$frequency)
d2rn<-d2rn[with(d1rn, order(-frequency)), ]
e<-c("thanks","view enjoy","always","best","service","helpful","seats","businessclass","check","wish")
f<-c(26,15,9,6,5,5,5,3,10,5)
d1rp<-cbind(e,f)
d1rp<-as.data.frame(d1rp)
colnames(d1rp)<-c("text","frequency")
d1rp$frequency<-as.numeric(d1rp$frequency)
d1rp<-d1rp[with(d1rp, order(-frequency)), ]
g<-c("service","kind","help","great","nice","happy","tickets","london","seat","crew")
h<-c(12,12,10,31,9,5,5,4,4,7)
d2rp<-cbind(g,h)
d2rp<-as.data.frame(d2rp)
colnames(d2rp)<-c("text","frequency")
d2rp<-as.data.frame(d2rp)
colnames(d2rp)<-c("text","frequency")
d2rp$frequency<-as.numeric(d2rp$frequency)
d2rp<-d2rp[with(d2rp,order(-frequency)), ] 

#Create bar graph with appropriate colours
#and use coord_flip() to help the labels look nicer.
d1n_plot <- ggplot(d1rn, aes(x = reorder(text, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

d2n_plot <- ggplot(d2rn, aes(x = reorder(text, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))
grid.arrange(d1n_plot, d2n_plot, ncol=2)

d1p_plot <- ggplot(d1rp, aes(x = reorder(text, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

d2p_plot <- ggplot(d2rp, aes(x = reorder(text, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))
grid.arrange(d1p_plot, d2p_plot, ncol=2)


d1c<-rbind(d1rp,d1rn)
d2c<-rbind(d2rp,d2rn)

#Make Word Clouds 
wordcloud(words=d1c$text,freq = d1c$frequency,random.order = T,random.color = T, colors = c("blue","red","green","orange","pink"),min.freq=1)
wordcloud(words=d2c$text,freq = d2c$frequency,random.order = T,random.color = T, colors = c("blue","red","green","orange","pink"),min.freq=1)



####### Running classifiers Df1

d1clss<- d1[,c(1,4)]

matrix= create_matrix(d1clss[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:400,], as.factor(d1clss[1:400,2]) )
# test the validity
predicted = predict(classifier, mat[401:527,]); predicted
table(d1clss[401:527, 2], predicted)
recall_accuracy(d1clss[401:527, 2], predicted)

container = create_container(matrix, as.numeric(as.factor(d1clss[,2])),
                             trainSize=1:400, testSize=401:527,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)

# recall accuracy
recall_accuracy(as.numeric(as.factor(d1clss[401:527, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(d1clss[401:527, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(d1clss[401:527, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(d1clss[401:527, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(d1clss[401:527, 2])), results[,"SVM_LABEL"])

analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summar
N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")
cross_validate(container,N,"BAGGING")



####### Running classifiers Df2

d2clss<- d2[,c(1,4)]

matrix= create_matrix(d2clss[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:362,], as.factor(d2clss[1:362,2]) )
# test the validity
predicted = predict(classifier, mat[363:463,]); predicted
table(d2clss[363:463, 2], predicted)
recall_accuracy(d1clss[363:463, 2], predicted)

container = create_container(matrix, as.numeric(as.factor(d1clss[,2])),
                             trainSize=1:362, testSize=363:463,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)

# recall accuracy
recall_accuracy(as.numeric(as.factor(d2clss[363:463, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(d2clss[363:463, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(d2clss[363:463, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(d2clss[363:463, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(d2clss[363:463, 2])), results[,"SVM_LABEL"])

analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summar
N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")
cross_validate(container,N,"BAGGING")


