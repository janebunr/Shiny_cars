library(streamR)
library(RCurl)
library(bitops)
library(rjson)
library(twitteR)
library(tm)
library(dplyr)
library(ggplot2)
library(ROAuth)
library(wordcloud)
library(qdapRegex)
library(twitteR)
library(tidyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(quanteda)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(topicmodels)
library(ggraph)
library(topicmodels)
library(igraph)
library(dplyr)
library(stringr)
library(janeaustenr)

# Connecting Twiitter API
api_key = "api_key"
api_secret = "api_secret"
access_token = "access_token"
access_token_secret = "access_token_secret"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

register_sqlite_backend("~/Data/File/path/CARDB")

### Collecting data
TESLA <- searchTwitter("a tesla", n=10000, lang="en", retryOnRateLimit = 500) 
store_tweets_db(TESLA,table_name="Tesla")

PORSCHE <- searchTwitter("porsche", n=10000, lang="en", retryOnRateLimit = 120)
store_tweets_db(PORSCHE,table_name="porsche")

BMW <- searchTwitter("BMW", n=10000, lang="en", retryOnRateLimit = 120)
store_tweets_db(BMW,table_name="BMW")

AUDI <- searchTwitter("audi", n=10000, lang="en",retryOnRateLimit = 120)
store_tweets_db(AUDI,table_name="Audi")

MERCEDES <- searchTwitter("mercedes", n=10000, lang="en",retryOnRateLimit = 120)
store_tweets_db(MERCEDES,table_name="Mercedes")

# Loading data from local database
Tesla = load_tweets_db(table_name = "Tesla")
Tesla <- do.call("rbind",lapply(Tesla, as.data.frame))
Tesla$text <- iconv(Tesla$text, "ASCII", "UTF-8", sub="")

Porsche = load_tweets_db(table_name ="porsche")
Porsche <- do.call("rbind",lapply(Porsche, as.data.frame))
Porsche$text <- iconv(Porsche$text, "ASCII", "UTF-8", sub="")

bmw = load_tweets_db(table_name = "BMW")
bmw <- do.call("rbind",lapply(bmw, as.data.frame))
bmw$text <- iconv(bmw$text, "ASCII", "UTF-8", sub="")

Audi = load_tweets_db(table_name = "Audi")
Audi <- do.call("rbind",lapply(Audi, as.data.frame))
Audi$text <- iconv(Audi$text, "ASCII", "UTF-8", sub="")

Mercedes = load_tweets_db(table_name = "Mercedes")
Mercedes <- do.call("rbind",lapply(Mercedes, as.data.frame))
Mercedes$text <- iconv(Mercedes$text, "ASCII", "UTF-8", sub="")

#combining all tweets
combined_tweets = c(Tesla$text, Porsche$text, bmw$text, Audi$text, Mercedes$text)

# Data cleaning 
rm_url(combined_tweets, pattern=pastex("@rm_twitter_url", "@rm_url")) # removing urls

text <- lapply(combined_tweets, function(x) {
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  x = gsub('\\b+RT', '', x) ## Remove RT
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  x = gsub('@\\S+', '', x) ## Remove Mentions
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  x = gsub(' +',' ',x) ## Remove extra whitespaces
})

########### EXPLORING DATA ##########
#####################################
CAR_TWEETS <- data_frame(line=1:nrow(data.frame(combined_tweets)), text = as.character(text))
brand_list <- data_frame(CarBrand = rep(c("Tesla", "Porsche" ,"BMW" ,"Audi", "Mercedes-Benz"), each=30000),line=1:nrow(data.frame(combined_tweets)))
CAR_TWEETS <- CAR_TWEETS %>% left_join(brand_list)


#### wordcloud #####
tidy_text <- CAR_TWEETS %>% unnest_tokens(word,text)
tidy_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))


### list of most common positive and negative words
bing_word_counts <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


## wordcloud of most common positive and negative words
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red2", "blue3"),
                   max.words = 100)