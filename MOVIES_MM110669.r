library(ggplot2)
library(ggplot2movies)
library(dplyr)

data(movies)
?movies
movies_clean <- movies %>%
  filter(
    !is.na(rating),                      
    !is.na(votes),                       
    votes >= 10,                         
  ) %>%
  mutate(
    genre_count = Action + Animation + Comedy + Drama + Documentary + Romance + Short
  ) %>%
  filter(genre_count > 0)  


str(movies_clean)
summary(movies_clean)

ggplot(movies_clean, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Movie Ratings", x = "Rating", y = "Count", caption = "Source: 'movies' dataset")

ggplot(movies_clean, aes(x = length)) +
  geom_histogram(binwidth = 10, fill = "darkorange", color = "black") +
  labs(title = "Distribution of Movie Lengths", x = "Length (minutes)", y = "Count", caption = "Source: 'movies' dataset")

ggplot(movies_clean, aes(x = votes)) +
  geom_histogram(binwidth = 0.1, fill = "darkgreen", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of Votes (Log Scale)", x = "Votes (log scale)", y = "Count", caption = "Source: 'movies' dataset")

ggplot(movies_clean, aes(x = votes, y = rating)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_x_log10() +
  labs(title = "Votes vs Rating", x = "Votes (log scale)", y = "Rating", caption = "Source: 'movies' dataset")

ggplot(movies_clean, aes(x = length, y = rating)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Movie Length vs Rating", x = "Length (minutes)", y = "Rating", caption = "Source: 'movies' dataset")

ggplot(movies_clean, aes(x = as.factor(Action), y = rating)) +
  geom_boxplot(fill = "cyan", color = "black") +
  scale_x_discrete(labels = c("Not Action", "Action")) +
  labs(title = "Ratings by Action Genre", x = "Genre", y = "Rating", caption = "Source: 'movies' dataset")

ggplot(movies_clean, aes(x = votes, y = rating, color = as.factor(Action))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Action, labeller = labeller(Action = c("0" = "Not Action", "1" = "Action"))) +
  scale_x_log10() +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "Votes vs Rating by Action Genre", x = "Votes (log scale)", y = "Rating", color = "Genre", caption = "Source: 'movies' dataset")

movies_clean <- movies_clean %>%
  mutate(decade = (year %/% 10) * 10)

ggplot(movies_clean, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  facet_wrap(~ decade) +
  labs(title = "Histogram of Ratings by Decade", x = "Rating", y = "Count", caption = "Source: 'movies' dataset")

genre_combinations <- movies_clean %>%
  group_by(Action, Comedy) %>%
  summarise(avg_rating = mean(rating))

ggplot(genre_combinations, aes(x = as.factor(Action), y = as.factor(Comedy), fill = avg_rating)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heatmap of Average Ratings by Genre Combinations", x = "Action", y = "Comedy", fill = "Avg Rating", caption = "Source: 'movies' dataset")

ratings_over_time <- movies_clean %>%
  group_by(year) %>%
  summarise(avg_rating = mean(rating))

ggplot(ratings_over_time, aes(x = year, y = avg_rating)) +
  geom_line(color = "darkgreen") +
  labs(title = "Average Ratings Over Time", x = "Year", y = "Average Rating", caption = "Source: 'movies' dataset")

