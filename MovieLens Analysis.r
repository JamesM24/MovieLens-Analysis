
# Final Presentation - Consulting Models & Methods Team 7

library(tidyverse)
library(ggplot2)
library(readxl)
library(scales)
library(ggthemes)

getwd()

# Read main data sets provided 

Movies <- read.csv('movies.csv')
Tags <- read.csv('tags.csv')
Ratings <- read.csv('ratings.csv')

# Read Links data set

Links <- read.csv('links.csv')

# Genome Data sets

Genome_tags <- read.csv('genome-tags.csv')
Genome_scores <- read.csv('genome-scores.csv')
Combined_Genomes <- Genome_scores %>% 
  left_join(Genome_tags, by = c('tagId' = 'tagId')) %>% 
  arrange(desc(relevance))

# Clean out duplicates and filter for relevance above 75%

most_relevant <- Combined_Genomes %>%
  filter(relevance >= 0.75)
most_relevant_filtered <- most_relevant[!duplicated(most_relevant$tag),]

# Merged data frames Movie + Tags

Movies_and_Tags <- Movies %>% left_join(Tags, by = c('movieId' = 'movieId'))
Movies_Tags_dated <- Movies_and_Tags %>% 
  mutate(timestamp1 = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), year = format(timestamp1, "%Y"))

# Remove UserId & MovieId columns

Ratings_cleaned <- as.data.frame(Ratings[,3:4], drop=FALSE)

# Ratings merged with movies and tags using timestamp

Movies_tags_ratings1 <- Movies_Tags_dated %>% inner_join(Ratings_cleaned, by =  'timestamp')

# Separated the title column 

Movies_tags_ratings <- Movies_tags_ratings1 %>% extract(title, c("title", "Year released"), regex = "(.*)\\s\\((\\d+)\\)", convert = TRUE) 

# Movies split by genre

Movies_genre_analysis <- Movies_tags_ratings %>%
  separate_rows(genres, sep = "\\|") 
View(Movies_genre_analysis)

most_rated_genre <- Movies_genre_analysis %>% 
  group_by(genres) %>%
  mutate(avg_genre_score = mean(rating))

genres_cleaned <-most_rated_genre[!duplicated(most_rated_genre$genres),]

# Actors sheet

Actors_df <- read_excel("actors_internet.xlsx", sheet = 1)
Actors <- Movies_tags_ratings %>% 
  inner_join(Actors_df, by =c('tag'='tag'))

most_rated_actor <- Actors  %>% 
  group_by(tag) %>%
  mutate(avg_actor_score = mean(rating))

Actors_cleaned <- most_rated_actor[!duplicated(most_rated_actor$tag),]

drop <- c("tag","avg_actor_score")
Avg_score_actor = Actors_cleaned[,(names(Actors_cleaned) %in% drop)]
View(Avg_score_actor)

# Filter by time stamp

No_timestamp_dups <- Movies_tags_ratings[!duplicated(Movies_tags_ratings$timestamp),]

# Most rated movies

most_rated <- Movies_tags_ratings %>% 
  group_by(title) %>%
  mutate(avg_score = mean(rating))
View(most_rated)

most_rated_cleaned <- most_rated[!duplicated(most_rated$title),]
View(most_rated_cleaned)

drop <- c("title","avg_score")
Avg_score = most_rated_cleaned[,(names(most_rated_cleaned) %in% drop)]
View(Avg_score)

# Most Active users

most_user <- Movies_tags_ratings %>% group_by(userId) %>% summarise(rating_count=n()) %>% arrange(desc(rating_count))
View(most_user)

# Years with most ratings

popular_year <- Movies_tags_ratings %>% group_by(`Year released`) %>% summarise(rating_count=n()) %>% arrange(desc(rating_count)

popular_genre <- Movies_tags_ratings %>% 
  group_by(`genres`) %>% summarise(rating_count=mean()) %>% 
  arrange(desc(rating_count))

# Identify what the tags are

Movies_tags_ratings_titles <- Movies_tags_ratings[!duplicated(Movies_tags_ratings$title),]
Isolated_tags <- as.data.frame(Movies_tags_ratings_titles[,5], drop=false)
View(Isolated_tags)

# Find which movie tags have highest ratings

Highest_rating <- Movies_tags_ratings %>%
  group_by(tag) %>%
  arrange(rating) %>%
  mutate(ranking = rank(desc(rating)))

# Find which Genre has the highest ratings

Highest_genre_rating <- Movies_tags_ratings %>%
  group_by(genres) %>%
  arrange(rating) %>%
  mutate(ranking = rank(desc(rating)))

# Find which movie genres have the highest ratings

Ratings_genre <- Movies_tags_ratings %>% 
  filter(rating == 5) %>%
  group_by(genres)
Ratings_genre_cleaned <- Ratings_genre[!duplicated(Ratings_genre$title),]
nrow(Ratings_genre_cleaned)

Ratings_duplicates <- Ratings_genre_cleaned[!duplicated(Ratings_genre_cleaned$timestamp),]

Ratings_pivot <- Ratings_duplicates %>%
  pivot_wider(names_from = genres, values_from = title)

# Other Analysis

Disney <- Movies_tags_ratings %>% 
  filter(tag == 'Disney')
avg_Disney <- mean(Disney$rating)

Pixar <- Movies_tags_ratings %>% 
  filter(tag == 'Pixar')
avg_Pixar <- mean(Pixar$rating)

print(avg_Disney)
print(avg_Pixar)

# Year Rated

Movies_genre_analysis %>%
  group_by(movieId) %>%
  slice(1) %>%
  ggplot(aes(year)) +
  geom_bar() +
  labs(x="Year Rated",
       y="Absolute Frequency")

# Rating Distribution

Movies_genre_analysis %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_line() +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Rating Distribution", subtitle = "Higher ratings are prevalent.") + 
  xlab("Rating") +
  ylab("Count") +
  theme_economist()

# Average Rating from the data 

Median_rating <- median(Movies_genre_analysis$rating)
print(Median_rating)
Average_rating <- mean(Movies_genre_analysis$rating)
print(Average_rating)

# Median Rating is 4
# Average Rating is 3.58