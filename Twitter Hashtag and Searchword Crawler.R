library(rtweet)
library(dplyr)
library(graphics)

dftweet <- search_tweets("#hashtag or search word", n=18000, include_rts = TRUE)

df <- dftweet[ which(dftweet$lang== 'en'),]

df <- df %>%
  select(user_id, status_id, created_at, screen_name, text, favorite_count, source, is_quote, is_retweet, retweet_text, retweet_created_at)


# Cleaning the data with twitter corpus
library(purrr)
library(syuzhet)
library(corpus)

library(tm)
mycorpus <- Corpus(VectorSource(df$text))
mycorpus <- tm_map(mycorpus, removeWords, stopwords())

# Removing RedHat word like COVID which I am expecting to appear the most as it is the basis for the search hashtag

myStopWords <- "input stop words here, eg words from the hashtag, searchword or phrase"
mycorpus <- tm_map(mycorpus, removeWords, myStopWords)

# Removing urls
remove_urls <- function(x) gsub("http[^[:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(remove_urls))

# Remove anything that is not english letters, words and space
rnp <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(rnp))
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, stemDocument)

dtm <- DocumentTermMatrix(mycorpus)

                                      # WordCloud
library(wordcloud)
wc <- wordcloud(mycorpus, min.freq = 500)
wc <- wordcloud(mycorpus, min.freq = 500, random.order = FALSE)

                            # Removing alphanumeric characters
df$text <- gsub("[^0-9A-Za-z///' ]","", df$text)

                                  # Removing web links
df$text <- gsub("http:\\w+", "", df$text)

                                # Removing the "rt" text
df$text <- gsub("rt", "", df$text)

df$text <- tolower(df$text)
