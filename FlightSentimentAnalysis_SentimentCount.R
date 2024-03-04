library("tidyverse")
library("ggplot2")

tweets <- read_csv("Data/Tweets.csv")

tweets_clean_aggregate <- tweets %>%
  select(-name, -text, -tweet_coord, -tweet_location) %>% 
  group_by(airline_sentiment, airline) %>% 
  count()

colors <- c("red", "blue", "green")
ggplot(tweets_clean_aggregate, aes(x = airline_sentiment, y = n, fill = airline_sentiment)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Sentiment", y = "Number of Tweets") +
  ggtitle("Twitter Activity by Sentiment") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25, size = 3.5) +
  theme_classic() +
  scale_fill_manual(values = colors) +
  theme(axis.ticks.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ airline)
