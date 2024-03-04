library("tidyverse")
library("ggplot2")
library(scales)

tweets <- read_csv("Data/Tweets.csv")

tweets_clean <- tweets %>%
  select(-name, -text, -tweet_coord, -tweet_location) %>% 
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

table(tweets_clean$user_location)
  
tweets_clean_aggregate <- tweets_clean %>% 
  group_by(user_location) %>% 
  summarize(num_neg = sum(airline_sentiment == "negative"),
            num_pos = sum(airline_sentiment == "positive"),
            num_neu = sum(airline_sentiment == "neutral"),
            num = n()) %>% 
  mutate(per_neg = num_neg / num,
         per_pos = num_pos / num,
         per_neu = num_neu / num) %>% 
  pivot_longer(cols = starts_with("per"), values_to = "per") %>% 
  mutate(per_label = ifelse(per != 0, sprintf("%0.0f%%", per * 100), "")) %>% 
  rename(sentiment = name)

ggplot(tweets_clean_aggregate, aes(x = user_location, y = num, fill = user_location)) +
  geom_bar(position = 'dodge', stat = "identity") +
  coord_flip() +
  labs(x = "User Region", y = "Number of Tweets") +
  ggtitle("Twitter Activity by Region") +
  geom_text(aes(label = num), position = position_dodge(width = 0.9), hjust = -0.25, size = 3.5) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

colors <- c("red", "blue", "green")
ggplot(tweets_clean_aggregate, aes(x = user_location, y = per, fill = sentiment)) + 
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  xlab("User Region") +
  ggtitle("Sentiments by Region") +
  geom_text(aes(label = per_label),
            position = position_stack(vjust = 0.5),
            size = 3.5,
            fontface = "bold") +
  scale_color_manual(values = colors) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

