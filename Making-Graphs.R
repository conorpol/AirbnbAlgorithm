#import the dataset
air2 = read.csv("FinalCleanedWithAllVars.csv")
air2 = air2[,-c(1)] 

# Turning numeric columns to factors
air2[,c(1,3,4,5,7,9,10,11,12,17:20,26,40:41,43,46:82)] = lapply(air2[,c(1,3,4,5,7,9,10,11,12,17:20,26,40:41,43,46:82)], factor)
#air22$zipcode = as.character(air22$zipcode)
####### Graph 1: Property Type per district
library(ggplot2)
g <- ggplot(air2, aes(zipcode))

g + geom_bar(aes(fill=property_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Property Type", 
       subtitle="Property Types per District") 

####### Graph 2: Word Plot of Description
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


#turn description into a corpus
text = air2$description
docs <- Corpus(VectorSource(text))
inspect(docs)

# Text Transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("paris", "apartment","room","bed","kitchen","floor","located","bathroom","bedroom","flat")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


# Build term document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Make the word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))

##### Graph 3: Find words that occur at least 4 times
findFreqTerms(dtm, lowfreq = 4)
# what words are associated with living
findAssocs(dtm, terms = "living", corlimit = 0.3)

head(d, 10)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

####### Graph 4: Boxplot Price and Response time
theme_set(theme_classic())
attach(air2)
# Plot
install.packages("ggthemes")
library(ggthemes)
library(ggplot2)
g <- ggplot(air2, aes(host_response_time, price))
g + geom_boxplot(aes(fill= price))+theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Price vs. Host Response Time ", 
       x="Host Response Time",
       y="Price") + scale_y_continuous(name="Price", limits=c(0, 600))

### Graph 5: Diverging Bar Plot
#############  Random Forest Time#############

library(randomForest)
attach(air2)
#make a data frame with the predictors
predictors = air2[,c(12,17,18,19,46:48,53:65,83,85)]

myforest = randomForest(IncomePerMonth~., ntree=500, data=air2, importance=TRUE)
myforest


importance(myforest)


predict(myforest,data.frame(zipcode=75001,square_meter=2250, importance=TRUE))
    
        