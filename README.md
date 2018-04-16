# Shiny_cars

Supercars are fast, exciting, exotic, and irresistible. One of the topics that I’m enthusiastic about is cars, and this is the reason today I decided to explore different topics and trends that people are currently talking about among major supercar automakers.

The purpose of this work is to visualize and gain insightful information from raw qualitative data or descriptive information from an open source data Twitter, about customers’ brand attitudes among five major automobile companies:

Tesla,
Porsche AG
Audi AG
BMW
Mercedes-Benz

The major focus are factors that play a major role influencing customers’ attention and brand attractiveness including current brand perception, and product attributes from car features to individual driving experiences.

Approach

Social media and its popularity indeed change our daily lives and buying decisions in recent years. Social sites not only enhance our experience, but they also allow us to be heard and satisfy our curiosity because we crave for information. People complain, compliment and capture their current moments from tweeting about how upset they're when their favorite sport teams lose to sharing their ordinary moments. Because this data is raw and first handed, it allows me to reach an individual and expose to innumerable numbers of tweets that people are talking about these five major car brands. My goal is to look into customers' attitude towards each brand including the attractiveness, innovativeness and expensiveness of the products and hopfully their puchase intentions.

To approach, I'll crawl data from Twitter using Twitter API in the total of 150,000 tweets (30,000 tweets each car brand), analyze using text analysis and topic model to visualize insightful information using R Programming. The book that I use to refer to is Text Mining with R: A Tidy Approach by Julia Silge and David Robinson.

Getting data from Twitter

First, to crawl tweets about interested car brands from Twitter using Twitter API using key words "a tesla", "Porsche", "BMW","Audi" and "Mercedes" 10,000 tweets per day each brand on 3 different days (total of 30,000 tweets each brand or 150,000 tweets overall) And then store as a dataset each to a local database.



