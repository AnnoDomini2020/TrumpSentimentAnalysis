#### Heading ####
## "Text mining and sentiment analysis of Trump tweets, 2020-2021,"
## by Andrew Infantino
## Based on code provided by Dr. Rafael A. Irizarry in the spring of 2020 for the HarvardX Data Science Professional Certificate program.
## Compiled 23 June 2021

#### Libraries and data ####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(textdata)) install.packages("textdata", repos = "http://cran.us.r-project.org")

trump_tweets <- read_csv("tweets_01-08-2021.csv") # Courtesy of thetrumparchive.com

#### Exploratory analysis ####
class(trump_tweets)
head(trump_tweets)
names(trump_tweets)
trump_tweets %>% select(text) %>% head()
class(trump_tweets$date)

# Narrow the data to 2020-2021:
last_tweets <- trump_tweets %>%
  filter(date >= ymd("2020-01-01") &
           date < ymd("2021-01-08"))

# What devices did Trump use to tweet then?
last_tweets %>% summarize(unique(device))

# Only three clients. No Android:
# 1) Twitter for iPhone
# 2) Twitter Media Studio
# 3) Twitter Web App

# What was his favorite device?
last_tweets %>% count(device) %>% arrange(desc(n))

## Twitter for iPhone: 1200 tweets
## Twitter Media Studio: 85 tweets
## Twitter Web App: 4 tweets

# What were the web app tweets?
last_tweets %>% filter(device == "Twitter Web App") %>% select(text)

# How about the media studio tweets?
last_tweets %>% filter(device == "Twitter Media Studio") %>% select(text) %>% head()

# Let's visualize the percent of tweets posted for each platform by time of day:
ds_theme_set()
last_tweets %>%
  mutate(hour = hour(with_tz(date, "EST"))) %>% # Extract time of day (nearest hour, EST) for each tweet.
  filter(device != "Twitter Web App") %>%       # Only four tweets there. Insufficient for conclusions.
  count(device, hour) %>%                       # Count number of tweets posted by each device for each hour.
  group_by(device) %>%
  mutate(percent = n/sum(n)) %>%                # Compute percentage of tweets for each device by hour posted.
  ungroup %>%
  ggplot(aes(hour, percent, color = device)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# iPhone tweets are posted most frequently between 6:00-7:30AM EST. Media Studio tweets are most common at night
# and minimal in the morning. One may speculate that Trump's staff tweeted for him through that platform, but the
# data is too limited to draw conclusions.

#### Content Analysis ####
# Converting free text to tidy tables, we can statistically examine and visualize tweet content. unnest_tokens() extracts "tokens" (data points) -- 
# such as words, characters, ngrams, sentences, lines, or regex-defined patterns -- from string vectors and gives each one a row. Here's an example:
example <- data_frame(line = c(1,2,3,4),
                      text = c("Roses are red,", "Violets are blue,",
                               "I'm tired of this poem,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# Let's try it on tweet #12389:
last_tweets$text[12389]
last_tweets[12389,] %>%
  unnest_tokens(word, text) %>%
  select(word)

# The dataframe is not arranged by date, so it's not his first or last one from the examined time period. Look at tweet #44:
last_tweets$text[44]
last_tweets[44,] %>%
  unnest_tokens(word, text) %>%
  select(word) %>% as.data.frame() # Keeping as tibble doesn't show full list.

# The function converts all tokens to words and deletes charachters such as @, #, and &. Tokens are not the same in twitter as in standard English.
# Rather than use the default token (words), define a regex pattern that captures twitter characters:
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# unnest_tokens() lets us use regex to extract hashtags and mentions appropriately:
last_tweets[44,] %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word) %>% as.data.frame()

# Let's remove links to pictures:
last_tweets[44,] %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word) %>% as.data.frame()

# Doesn't work because link is cut off in text. Let's expand the scope:
last_tweets[44,] %>%
  mutate(text = str_replace_all(text, "https://t[A-Za-z\\d./]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word) %>% as.data.frame()

# Now we can extract the words for all tweets:
tweet_words <- last_tweets %>%
  mutate(text = str_replace_all(text, "https://t[A-Za-z\\d./]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern)

# What are the most commonly used words?
tweet_words %>%
  count(word) %>%
  arrange(desc(n))

# Except for RT, these are called "stop words", which tidytext collects in a database. Here's a preview:
stop_words

# They're not useful, so we can filter them out:
tweet_words <- last_tweets %>%
  mutate(text = str_replace_all(text, "https://t[A-Za-z\\d./]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% c(stop_words$word, "rt", "@realdonaldtrump"))
  # The latter two elements otherwise appear in the list below. Not useful. "@realdonaldtrump" is only written in retweets.

# What are the most commonly used non-stop words other than RT and @realdonaldtrump?
tweet_words %>%
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

# Some tokens (not seen) are just numbers, identifi able via the regex ^\d+$. Some are quotes that start with '. 
# Eliminate them via via str_detect() and str_replace():
tweet_words <- last_tweets %>%
  mutate(text = str_replace_all(text, "https://t[A-Za-z\\d./]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% c(stop_words$word, "rt", "@realdonaldtrump") & 
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# Let's examine how words change around major events of 2020-2021. Using the odds ratio, let y be a given word for one of
# two time periods around an event (before and after). Calculate the ratio of words that are y to not y for a time period,
# the equivalent ratio for the other time period, and the ratio of those two ratios. Use the 0.5 correction for the prev-
# alence of zero proportions. Let's use the WHO's COVID-19 pandemic declaration as our first event:
pandemic_periods <- tweet_words %>%
  mutate(period = ifelse(date < ymd("2020-03-11"), "Before", "After")) %>%
  count(word, period) %>%
  spread(period, n, fill = 0) %>%
  mutate(or = (Before + 0.5)/(sum(Before) - Before + 0.5)/ # Number of times a given word is used before to all other words used before.
           ( (After + 0.5)/(sum(After) - After + 0.5) ) )  # Number of times a given word is used after to all other words used after.
pandemic_periods %>% arrange(desc(or))
pandemic_periods %>% arrange(or)

# Filter out infrequent words:
pandemic_periods %>% 
  filter(Before + After > 100) %>%
  arrange(desc(or))                # Most likely to appear before.
pandemic_periods %>%
  filter(Before + After > 100) %>%
  arrange(or)                      # Most likely to appear after.

# Let's examine the death of George Floyd:
floyd_periods <- tweet_words %>%
  mutate(period = ifelse(date < ymd("2020-05-25"), "Before", "After")) %>%
  count(word, period) %>%
  spread(period, n, fill = 0) %>%
  mutate(or = (Before + 0.5)/(sum(Before) - Before + 0.5)/ # Popularity of word y before.
           ( (After + 0.5)/(sum(After) - After + 0.5) ) )  # Popularity of word y after.
floyd_periods %>% arrange(desc(or))
floyd_periods %>% arrange(or)
floyd_periods %>% filter(Before + After > 100) %>% arrange(desc(or)) # Most likely to appear before.
floyd_periods %>% filter(Before + After > 100) %>% arrange(or)       # Most likely to appear after.

# Let's examine Election Day:
election_periods <- tweet_words %>%
  mutate(period = ifelse(date < ymd("2020-11-03"), "Before", "After")) %>%
  count(word, period) %>%
  spread(period, n, fill = 0) %>%
  mutate(or = (Before + 0.5)/(sum(Before) - Before + 0.5)/ # Popularity of word y before.
           ( (After + 0.5)/(sum(After) - After + 0.5) ) )  # Popularity of word y after.
election_periods %>% arrange(desc(or))
election_periods %>% arrange(or)
election_periods %>% filter(Before + After > 100) %>% arrange(desc(or)) # Most likely to appear before.
election_periods %>% filter(Before + After > 100) %>% arrange(or)       # Most likely to appear after.

# We can attribute different "sentiments" to different words. While missing context-dependent sentiments such as sarcasm,
# summaries can be insightful for large number of words. Let's apply it to different time periods.

#### Sentiment Analysis ####
# tidytext has a package for various lexica and maps in which we can search the values of different words:
sentiments
get_sentiments("bing")   # Positive or negative
get_sentiments("afinn")  # Rated -5 to 5.

# Different lexica provide different sentiments:
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# Let's use the nrc lexicon to explore the different sentiments of each tweet:
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
tweet_words %>%                       # We'll primarily examine George Floyd's death as a focal point.
  mutate(period = ifelse(date < ymd("2020-05-25"), "Before", "After")) %>%
  inner_join(nrc, by = "word") %>%    # Join each word with a sentiment from nrc.
  select(period, word, sentiment) %>% # inner_join() keeps only those with a match.
  sample_n(10)                        # Preview ten random examples.

# More words were used after than before George Floyd's death, but the former period was longer:
tweet_words %>%
  mutate(period = ifelse(date < ymd("2020-05-25"), "Before", "After")) %>%
  group_by(period) %>% 
  summarize(n = n())

# A tweet can have multiple sentiments. Let's analyze only words themselves:
sentiment_counts <- tweet_words %>%
  mutate(period = ifelse(date < ymd("2020-05-25"), "Before", "After")) %>%
  left_join(nrc, by = "word") %>%
  count(period, sentiment) %>%
  spread(period, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))

# We can compute the odds of each sentiment occurring during a period, the proportion of sentimental to non-sentimental words,
# and compare the odds ratio by period:
sentiment_counts %>%
  mutate(Before = Before / (sum(Before) - Before),
         After = After / (sum(After) - After),
         or = Before/After) %>%
  arrange(desc(or))                    # Rank by odds of use before rather than after.

# Using confidence intervals, let's determine the statistical significance of the log odds ratios. Compile them in a 2x2 table:
log_or <- sentiment_counts %>%
  mutate( log_or = log((Before / (sum(Before) - Before)) / (After / (sum(After) - After))),
          se = sqrt( 1/Before + 1/(sum(Before) - Before) + 1/After + 1/(sum(After) - After)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))
log_or

# Visualize the above data:
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log-odds ratio") +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ggtitle("After vs. Before George Floyd's Death")

# Disgust and non-sentiment are over-represented before Floyd's death beyond chance. Fear, anger, joy, surprise, anticipation, and sadness are significantly
# over-represented afterward. What words drive the difference for disgust?
floyd_periods %>% 
  inner_join(nrc) %>%
  filter(sentiment == "disgust" & Before + After > 10) %>%
  arrange(desc(or))

# Let's visualize the equivalent list above for more sentiments:
floyd_periods %>% 
  inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Before + After > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Let's use the pandemic declaration as a focal point:
tweet_words %>%
  mutate(period = ifelse(date < ymd("2020-03-11"), "Before", "After")) %>%
  left_join(nrc, by = "word") %>%
  count(period, sentiment) %>%
  spread(period, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none")) %>%
  mutate( log_or = log((Before / (sum(Before) - Before)) / (After / (sum(After) - After))),
          se = sqrt( 1/Before + 1/(sum(Before) - Before) + 1/After + 1/(sum(After) - After)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or)) %>%   mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log-odds ratio") +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ggtitle("After vs. Before WHO COVID-19 Pandemic Declaration")

# Disgust and (to a smaller extent) negativity are significantly more likely to be used before than after the pandemic declaration. Joy and anticipation
# yield a slightly higher chance of use afterward.
pandemic_periods %>% 
  inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Before + After > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# And finally election day:
tweet_words %>%
  mutate(period = ifelse(date < ymd("2020-11-03"), "Before", "After")) %>%
  left_join(nrc, by = "word") %>%
  count(period, sentiment) %>%
  spread(period, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none")) %>%
  mutate( log_or = log((Before / (sum(Before) - Before)) / (After / (sum(After) - After))),
          se = sqrt( 1/Before + 1/(sum(Before) - Before) + 1/After + 1/(sum(After) - After)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or)) %>%   mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log-odds ratio") +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ggtitle("After vs. Before Election Day 2020")

# Anger and (to a lesser extent) negativity are significantly more likely to be observed after than before Election Day. Joy was more likely to be observed earlier.
election_periods %>% 
  inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Before + After > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
