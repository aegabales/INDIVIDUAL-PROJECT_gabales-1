#Packages
install.packages("twitteR")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plotly")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("tidytext")
install.packages("rtweet")
install.packages("tm")
install.packages("slam")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("corpus")

#libraries
library(twitteR)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(tidytext)
library(rtweet)
library(tm)
library(slam)
library(wordcloud)
library(wordcloud2)
library(corpus)

CONSUMER_SECRET <- "4a9VMi8yUx9w3kmHlcH4HZCo28xrq8KB4Xp5CiQxUYThcHIGxY"
CONSUMER_KEY <- "L5UGjr9G9AqtLSNUOqHexrcwB"
ACCESS_SECRET <- "FURsl7iwSXu5PMCTz6I0xHpaOHmUwwo32EGZ8UjrtpMGZ"
ACCESS_TOKEN <- "1599531219519410176-meJwDRKsJh4NE5Lc8mOxoM8CRvBQsj"

#connect to twitter app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)

trendTweets <- searchTwitter("#bts -filter:retweets",
                             n = 10000,
                             maxID = NULL,
                             lang = "en",
                             since = "2022-12-14",
                             until = "2022-12-21",
                             retryOnRateLimit = 120)
trendTweets

df <- twListToDF(trendTweets)
View(df)

head(df, n= 5)
names(df)
class(df)
data_text <- head(df$text)[1:5]
data_text

save(df,file= "df.Rdata")
load(file= "df.Rdata")

sapply(df, function(x) sum(is.na(x)))

trendingTwt <- df %>% 
  select(screenName, text, created, statusSource)

#Plot time series from the date created. with legends.

ggplot(data = df, aes(x = created)) + geom_histogram(aes(fill = ..count..)) +
  xlab("Time") + ylab("Number of Tweets") +
  scale_fill_gradient(low = "coral", high = "cyan") +
  theme(legend.position = "right")

#Plot a graph (any graph you want) based on the type of device - found in Source - that the user use. Include the legends.

TypeofDevices <- function(x) {
  if(grepl(">Twitter for iPhone</a>", x)){
    "iphone"
  }else if(grepl(">Twitter for iPad</a>", x)){
    "ipad"
  }else if(grepl(">Twitter for Android</a>", x)){
    "android"
  } else if(grepl(">Twitter Web Client</a>", x)){
    "Web"
  } else if(grepl(">Twitter for Windows Phone</a>", x)){
    "windows phone"
  }else if(grepl(">dlvr.it</a>", x)){
    "dlvr.it"
  }else if(grepl(">IFTTT</a>", x)){
    "ifttt"
  }else if(grepl(">Facebook</a>", x)){  
    "facebook"
  }else {
    "others"
  }
}
df$tweetSource = sapply(df$statusSource, TypeofDevices)

trendSource <- df %>% select(tweetSource) %>%
  group_by(tweetSource) %>% summarize(count=n()) %>%
  arrange(desc(count)) 

DeviceSource <- subset(trendSource, count >10)


dataSource <- data.frame(category = trendSource$tweetSource,
                          count = trendSource$count)

dataSource$fraction = dataSource$count / sum(dataSource$count)
dataSource$percentage = dataSource$count / sum(dataSource$count) * 100
dataSource$ymax = cumsum(dataSource$fraction)
dataSource$ymin = c(0, head(dataSource$ymax, n=-1))
dataSource$roundP = round(dataSource$percentage, digits = 2)

Device_Source <- paste(dataSource$category, dataSource$roundP, "%")

ggplot(df[df$tweetSource != 'others',], aes(tweetSource, fill = tweetSource)) +
  geom_bar() +
  theme(legend.position="right",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of tweets") +
  ggtitle("Type of Device that Users Use")

#Create a wordcloud from the screenName.

screen_name <- df %>%
  select(screenName) %>%
  group_by(screenName) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) 

corpusFile <- Corpus(VectorSource(df$screenName))  
class(df$screenName)

wordcloud2(data=screen_name, size=2, color='random-dark',
           shape = 'circle', backgroundColor="cyan")

