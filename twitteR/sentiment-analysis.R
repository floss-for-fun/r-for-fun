# https://mkmanu.wordpress.com/2014/08/05/sentiment-analysis-on-twitter-data-text-analytics-tutorial/

# Load the necessary packages
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(sentiment)
# If you got some errors, use
#library(devtools)
#install_github('yht/timjurka-sentiment')
# To install this package
# Rstem source, the dependency, can be download at http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz

# Find OAuth settings for twitter:
library(httr)
oauth_endpoints("twitter")

## <oauth_endpoint>
##  request:   https://api.twitter.com/oauth/request_token
##  authorize: https://api.twitter.com/oauth/authenticate
##  access:    https://api.twitter.com/oauth/access_token

# Register an application (API) at https://apps.twitter.com/
# Once done registering, look at the values of api key, secret and token
# Insert these values below:
api_key <- "You Twitter API Key"
api_secret <- "You Twitter API Secret Key"
access_token <- "You Twitter API Access Token"
access_token_secret <- "You Twitter API Access Token Secret"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

## [1] “Using direct authentication”

# Now let us collect some tweets (2000 in our example) containing the term “BJP” from twitter (language = English,
# if you wish you can set other languages to fetch tweets in those languages in your analytics)
bjp_tweets = searchTwitter("BJP", n=2000, lang="en")

# fetch the text of these tweets
bjp_txt = sapply(bjp_tweets, function(x) x$getText())

# Now we will prepare the above text for sentiment analysis
# First we will remove retweet entities from the stored tweets (text)
bjp_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", bjp_txt)
# Then remove all “@people”
bjp_txt = gsub("@\\w+", "", bjp_txt)
# Then remove all the punctuation
bjp_txt = gsub("[[:punct:]]", "", bjp_txt)
# Then remove numbers, we need only text for analytics
bjp_txt = gsub("[[:digit:]]", "", bjp_txt)
# the remove html links, which are not required for sentiment analysis
bjp_txt = gsub("http\\w+", "", bjp_txt)
# finally, we remove unnecessary spaces (white spaces, tabs etc)
bjp_txt = gsub("[ \t]{2,}", "", bjp_txt)
bjp_txt = gsub("^\\s+|\\s+$", "", bjp_txt)
# if anything else, you feel, should be removed, you can. For example “slang words” etc using the above function and methods.

# Since there can be some words in lower case and some in upper, we will try to eredicate this non-uniform pattern by making all the words in lower case. This makes uniform pattern.

# Let us first define a function which can handle “tolower error handling”, if arises any, during converting all words in lower case.
catch.error = function(x) {
	# let us create a missing value for test purpose
	y = NA
	# try to catch that error (NA) we just created
	catch_error = tryCatch(tolower(x), error=function(e) e)
	# if not an error
	if (!inherits(catch_error, "error"))
	y = tolower(x)
	# check result if error exists, otherwise the function works fine.
	return(y)
}

# Now we will transform all the words in lower case using catch.error function we just created above and with sapply function
bjp_txt = sapply(bjp_txt, catch.error)

# Also we will remove NAs, if any exists, from bjp_txt (the collected and refined text in analysis)
bjp_txt = bjp_txt[!is.na(bjp_txt)]

# also remove names (column headings) from the text, as we do not want them in the sentiment analysis
names(bjp_txt) = NULL

# Now the text is fully prepared (or at least for this tutorial) and we are good to go to perform Sentiment Analysis using this text

# As a first step in this stage, let us first classify emotions
# In this tutorial we will be using Bayes’ algorithm to classify emotion categories
# for more please see help on classify_emotion (?classify_emotion) under sentiment package
bjp_class_emo = classify_emotion(bjp_txt, algorithm="bayes", prior=1.0)
# the above function returns an of bject of class data.frame with seven columns (anger, disgust, fear, joy, sadness, surprise, best_fit) and one row for each document:

# we will fetch emotion category best_fit for our analysis purposes, visitors to this tutorials are encouraged to play around with other classifications as well.
emotion = bjp_class_emo[,7]

# Replace NA’s (if any, generated during classification process) by word “unknown”
# There are chances that classification process generates NA’s. This is because, sentiment package uses an in-built dataset “emotions”, which containing approximately 1500 words classified into six emotion categories: anger, disgust, fear, joy, sadness, and surprise
# If any words outside this dataset are given, the process will term the words as NA’s
emotion[is.na(emotion)] = "unknown"

# Similar to above, we will classify polarity in the text
# This process will classify the text data into four categories (pos – The absolute log likelihood of the document expressing a positive sentiment, neg – The absolute log likelihood of the document expressing a negative sentimen, pos/neg  – The ratio of absolute log likelihoods between positive and negative sentiment scores where a score of 1 indicates a neutral sentiment, less than 1 indicates a negative sentiment, and greater than 1 indicates a positive sentiment; AND best_fit – The most likely sentiment category (e.g. positive, negative, neutral) for the given text)
bjp_class_pol = classify_polarity(bjp_txt, algorithm="bayes")

# we will fetch polarity category best_fit for our analysis purposes, and as usual, visitors to this tutorials are encouraged to play around with other classifications as well
polarity = bjp_class_pol[,4]

# Let us now create a data frame with the above results obtained and rearrange data for plotting purposes
# creating data frame using emotion category and polarity results earlier obtained
sentiment_dataframe = data.frame(text=bjp_txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)

# rearrange data inside the frame by sorting it
sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# In the next step we will plot the obtained results (in data frame)

# First let us plot the distribution of emotions according to emotion categories
# We will use ggplot function from ggplot2 Package (for more look at the help on ggplot) and RColorBrewer Package
ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
 scale_fill_brewer(palette="Dark2") +
 ggtitle('Sentiment Analysis of Tweets on Twitter about BJP') +
 theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')

# Similary we will plot distribution of polarity in the tweets
ggplot(sentiment_dataframe, aes(x=polarity)) +
 geom_bar(aes(y=..count.., fill=polarity)) +
 scale_fill_brewer(palette="RdGy") +
 ggtitle('Sentiment Analysis of Tweets on Twitter about BJP') +
 theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')

# Finally, we will now separate the text (the words) according to emotions (categories) and visualize these words with a comparison cloud (using “wordcloud” Package)

# First, separate the words according to emotions
bjp_emos = levels(factor(sentiment_dataframe$emotion))
n_bjp_emos = length(bjp_emos)
bjp.emo.docs = rep("", n_bjp_emos)
for (i in 1:n_bjp_emos) {
 tmp = bjp_txt[emotion == bjp_emos[i]]
 bjp.emo.docs[i] = paste(tmp, collapse=" ")
}

# Here is a hick. Please not that there can be words in the emotion categories which you do not want to be.
# Like earlier in this tutorial, where I asked you to remove words such as slangs etc, here also you can remove
# these words specified as stopwords.
# For exaple we take “english” as the word which we want to remove and not be present in the word cloud,
# here how we do that:
bjp.emo.docs = removeWords(bjp.emo.docs, stopwords("english"))

# Now let us create a corpus which computes and represent words on corpora (corpora are collections of documents
# containing (natural language) text). For more please look at help on Corpora under “tm” package.
bjp.corpus = Corpus(VectorSource(bjp.emo.docs))
bjp.tdm = TermDocumentMatrix(bjp.corpus)
bjp.tdm = as.matrix(bjp.tdm)
colnames(bjp.tdm) = bjp_emos

# creating, comparing and plotting the words on the cloud
comparison.cloud(bjp.tdm, colors = brewer.pal(n_bjp_emos, "Dark2"),
 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

