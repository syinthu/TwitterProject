
install.packages('rtweet')
library(rtweet)

install.packages('dplyr')
library(dplyr)
# Installing text mining package tm

install.packages('tm')
library(tm)

install.packages('tidytext')
library(tidytext)

install.packages('SnowballC')
library(SnowballC)

# search_tweets agruements:

# First element is a string that we will be searing for. Spaces in the string behave like AND.

# Eg:   'Data Science'    will search for string containing both Data AND Science
#       'Data OR Science' will search for string containing either Data OR Science

# Maximum number of tweets returned is 18,000. To increase this we need to set agurment
# "retryonratelimit = TRUE"

#	AdmiralUK	is user ID 2284842134

# List of Competitors and IDs

# avivaplc	30626664
#	DirectLine	742552178
#	RACcarinsurance	2647187960
#	Churchill	18447841
#	esure	20693066
#	lv	683233
#	TescoCar	413024494
#	axainsurance	23746445
#	PrivilegeUK	3194105644
# AA Uk 384323508

# Ones that need to be checked are AvivaUK, Hastings, More than, Zurich

# Financial Ombudsman is 115711865
# Auxialls is 931827672
# FCA is 169431716




Users <- rt[rt$status_id == 1238873429815812096,]
InsuranceCompanies <- (Users$mentions_screen_name)
InsuranceID <- (Users$mentions_user_id)

Competitors <- cbind(as.data.frame(InsuranceCompanies),as.data.frame(InsuranceID))

rt <- search_tweets(
  "@AdmiralUK", n = 10, include_rts = FALSE
)
rt <- as.data.frame(load("/cloud/project/tweets.RData"))
rt <- as.data.frame(tweets)

StartDate <- as.Date('2020-03-17')
class(rt$created_at)

rt[as.Date(rt$created_at) < StartDate,'created_at']

tweet <- rt$text

tweet_source <- VectorSource(tweet)
tweet_corpus <- VCorpus(tweet_source)

tm_map(tweet_corpus, removeNumbers)
tm_map(tweet_corpus, removePunctuation)

head(tweet_corpus)



###################################

# Looking at Tweets per day

Dates <- rt %>%
  select(created_at, reply_to_screen_name, screen_name) %>%
  mutate(Date = as.Date(created_at))

# Any tweets to us per day

TweetsPerDay <- Dates %>%
  select(Date) %>%
  group_by(Date) %>%
  mutate(DayOfWeek = weekdays(Date))%>%
  count( Date,DayOfWeek, sort = FALSE)

# Tweets directly to @AdmiralUK

TweetsToAdmiral <- Dates %>%
  filter(reply_to_screen_name == 'AdmiralUK') %>%
  group_by(Date) %>%
  mutate(DayOfWeek = weekdays(Date))%>%
  count( Date,DayOfWeek,reply_to_screen_name, sort = FALSE)

# Tweets by user

TweetsByUser <- rt %>%
  select (screen_name) %>%
  count(screen_name, sort = TRUE) %>%
  filter(n > 1)

# Tweets by Verified accounts (News articles about us)

VerifiedTweets <- rt %>%
  select(name, screen_name, text, followers_count, favorite_count, retweet_count, verified) %>%
  filter((verified == TRUE | as.numeric(followers_count) > 1000) & screen_name != 'AdmiralUK') %>%
  arrange(desc(followers_count))

# Tweets with a lot of interactions

InteractedTweets <- rt %>%
  select(name, screen_name, text, followers_count, favorite_count, retweet_count, verified) %>%
  filter((as.numeric(retweet_count) > 1 | as.numeric(favorite_count) > 1) & screen_name != 'AdmiralUK') %>%
  arrange(desc(favorite_count), desc(retweet_count))

# Emojis summary

Emojis <- rt %>%
  select(text) %>%
  mutate(text = gsub("[\x01-\x7F]", "", text)) %>%   # Remove regular text
  mutate(text = gsub("[[:punct:]]", "", text)) %>%   # Remove Punctuation 
  unnest_characters(emoji,text, to_lower = TRUE) %>% # Split to single characters
  count(emoji, sort=TRUE)

# Tidy text

Tidytext <- rt%>%
  select(text) %>%
  mutate(text = gsub("@\\w+ *", "", text)) %>%                 # Removes Mentions and Usernames
  mutate(text = gsub("[[:digit:]]\\w+ *", "", text)) %>%       # Removes Numbers with words attached
  mutate(text = gsub("\\w[[:digit:]]+ *", "", text)) %>%       # Removes Numbers with words before
  mutate(text = gsub("[[:digit:]]+ *", "", text)) %>%          # Removes Numbers
  mutate(text = gsub("[^\x01-\x7F]", "", text)) %>%            # Removes Emojis
  mutate(text = gsub("[[:punct:]]", "", text)) %>%             # Remove Punctuation including Hashtags
  unnest_tweets(word,text, strip_url = TRUE, to_lower = TRUE)

Popwords <- Tidytext %>%
  anti_join(get_stopwords()) %>%
  count(word, sort=TRUE)

Popstems <- Tidytext %>%
  anti_join(get_stopwords()) %>%
  mutate(stem = wordStem(word))%>%
  mutate(word = stemCompletion(stem, Popwords$word)) %>%
  count(word, sort = TRUE)
