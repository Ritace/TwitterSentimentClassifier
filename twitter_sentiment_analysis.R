library(twitteR)
library(SnowballC)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(jsonlite)
library(sentiment)
library(ggplot2)
display <- function(corpus,lower=1,upper=10)
{
  for(i in lower:upper){
    cat(paste("[[",i,"]]",sep=""))
    writeLines(strwrap(strwrap(corpus[[i]],width = 73)))
    cat("\n")
  }
}
api_key <- "IVgGAvf59B8ZyUGW8LFhNKOMk" 
api_secret <- "wWJJmXfg2hLpJflmSHl3FPZocAlyKfV3KYaMMV68pw0BFZcsZG"
token <- "2908185337-8oXij5jwNAVk7cRmVK61F5Jq3Ca7GXL5maGcs7F"
token_secret <- "p8usfYc9gdIrJHBwKMjnXBC2CbbPiCgPob65PT7QZcnnj"
######### Got 2000 tweets from twitter and saved
setup_twitter_oauth(api_key,api_secret,token,token_secret)

#lets get the tweets mate
#these tweets are saved 
print("getting tweets")
tweets <- searchTwitter("#minions", lang="en",n = 2000)
textOftweets<-twListToDF(tweets)$text

##########
# textOftweets<-dput(file = "~/R/funwithR/data/proudToLove2000Tweets")
#removing retweet entities RT|via
textOftweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", textOftweets)
#removing @peple
textOftweets <- gsub("@\\w+","",textOftweets)
#punctuation removing
textOftweets <- gsub("[[:punct:]]","",textOftweets)
#digits removing
textOftweets <- gsub("[[:digit:]]","",textOftweets)
#removing html links not required for analysis
textOftweets <- gsub("http\\w+","",textOftweets)

#removing unnecessary spaces
textOftweets <- gsub("[ \t]{2,}","",textOftweets)
textOftweets <- gsub("^\\s+|\\s+$","",textOftweets)
textOftweets<-gsub("[^[:graph:]]", " ",textOftweets) 
textOftweets <-gsub("[^0-9a-zA-Z ,./?><:;’~`!@#&*’]","", textOftweets)

class_emotion = classify_emotion(textOftweets, algorithm="bayes", prior=1.0,verbose=TRUE)
# get emotion best fit
# seems that in classify_emotion use only unigrams but bigrams are not considered
# bigrams ma pani consider only appropirate bigrams

emotion = class_emotion[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

comment_df <- data.frame(emotion=emotion, stringsAsFactors=FALSE)
barplot <- ggplot(comment_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion))+xlab("Emotions Categories") +  
  ylab("Tweet Count")+ggtitle("Sentiment Analysis of Tweets on Emotions")

#   theme(panel.grid.major = element_line(linetype = c("12")))
print(barplot)