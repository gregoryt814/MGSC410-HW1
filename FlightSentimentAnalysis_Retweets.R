library("tidyverse")
library("ggplot2")

tweets <- read_csv("Data/Tweets.csv")

tweets_clean_aggregate <- tweets %>%
  select(airline_sentiment, airline, retweet_count) %>% 
  group_by(airline_sentiment) %>% 
  summarize(retweet_count = sum(retweet_count))

colors <- c("#E57373", "#90CAF9", "#81C784")
ggplot(tweets_clean_aggregate, aes(x = airline_sentiment, y = retweet_count, fill = airline_sentiment)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Sentiment", y = "Number of Retweets") +
  ggtitle("Twitter Retweets by Sentiment") +
  geom_text(aes(label = retweet_count), position = position_dodge(width = 0.9), vjust = -0.25, size = 3.5) +
  theme_classic() +
  scale_fill_manual(values = colors) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")


tweets_clean_aggregate <- tweets %>%
  select(airline_sentiment, airline, retweet_count, user_timezone) %>% 
  mutate(user_timezone = as.factor(user_timezone),
         user_location =
           fct_recode(user_timezone,
                      "Eastern Time (US & Canada)" = "America/Atikokan",
                      "Australia" = "Adelaide",
                      "Mountain Time (US & Canada)" = "America/Boise",
                      "Central Time (US & Canada)" = "America/Chicago",
                      "Eastern Time (US & Canada)" = "America/Detroit",
                      "Pacific Time (US & Canada)" = "America/Los_Angeles",
                      "Eastern Time (US & Canada)" = "America/New_York",
                      "Mountain Time (US & Canada)" = "Arizona",
                      "Eastern Time (US & Canada)" = "EST",
                      "Western Europe" = "Amsterdam",
                      "Southern Europe" = "Athens",
                      "East Asia" = "Beijing",
                      "Central Europe" = "Berlin",
                      "South America" = "Bogota",
                      "South America" = "Brasilia",
                      "Australia" = "Brisbane",
                      "Western Europe" = "Brussels",
                      "South America" = "Buenos Aires",
                      "South America" = "Caracas",
                      "North Africa" = "Casablanca",
                      "Australia" = "Melbourne",
                      "Northern Europe" = "Helsinki",
                      "Mexico" = "Tijuana",
                      "South America" = "Quito",
                      "East Asia" = "Taipei",
                      "Australia" = "Sydney",
                      "East Asia" = "Seoul",
                      "Southern Europe" = "Madrid",
                      "Eastern Time (US & Canada)" = "Indiana (East)",
                      "South America" = "Santiago",
                      "Southern Europe" = "Rome",
                      "Western Europe" = "Paris",
                      "Middle East" = "Abu Dhabi",
                      "Middle East" = "Tehran",
                      "Eastern Time (US & Canada)" = "Mid-Atlantic",
                      "Central Europe" = "Vienna",
                      "Northern Europe" = "Stockholm",
                      "Southern Europe" = "New Caledonia",
                      "Middle East" = "Jerusalem",
                      "South America" = "La Paz",
                      "Mexico" = "Guadalajara",
                      "East Asia" = "Bangkok",
                      "Mexico" = "Mexico City",
                      "Mexico" = "Mazatlan",
                      "Western Europe" = "London",
                      "Western Europe" = "Edinburgh",
                      "Western Europe" = "Dublin",
                      "Northern Europe" = "Greenland",
                      "South Asia" = "New Delhi",
                      "South Asia" = "Islamabad",
                      "East Asia" = "Singapore",
                      "Western Europe" = "Copenhagen",
                      "South America" = "Lima",
                      "East Asia" = "Hong Kong",
                      "Australia" = "Perth") %>% 
           fct_lump_min(3)) %>% 
  drop_na(user_timezone)

tweets_clean_aggregate <- tweets_clean_aggregate %>% 
  group_by(user_location, airline_sentiment, airline) %>% 
  summarize(retweet_count = sum(retweet_count)) %>% 
  filter(retweet_count > 0)

ggplot(tweets_clean_aggregate, aes(x = user_location, y = retweet_count, fill = airline_sentiment)) + 
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  xlab("User Region") +
  ggtitle("Retweets by Region (Minimum 2 Retweets)") +
  geom_text(aes(label = retweet_count),
            position = position_stack(vjust = 0.5),
            size = 3.5,
            fontface = "bold") +
  scale_fill_manual(values = colors) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~ airline, nrow = 3)
