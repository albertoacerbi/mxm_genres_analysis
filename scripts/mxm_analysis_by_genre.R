library(tidyverse) 
library(tidytext)
library(gridExtra)
library(viridis)


# load data:
mxm_data <- read_csv("../../data/mxm.csv")
# in the mxm datataset we keep only years for which we have more than 500 songs
count_by_year <- mxm_data %>%
  count(year) %>%
  filter(n > 500) %>% # from 1965 to 2010
  summarise(sum(n)) # N=159015

mxm_data <- mxm_data %>%
  filter(year >= 1965 & year <= 2010)


# #############################################################################
# which genres are present?:

genres <- mxm_data %>%
  filter(genre != "NA") %>%
  group_by(genre) %>%
  tally() %>%
  arrange(n) %>%
  slice(150 : 169) # we consider only the most common 20 genre 
# there are in total 168 genres (plus "NA")

genres$genre <- as_factor(genres$genre)

ggplot(data = genres, aes(x = genre, y = n)) +
  geom_bar(stat = "identity", fill = "#EFC000FF") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most common 20 genres in the mxm dataset", y = "number of songs") +
  ggsave("../plots/genres.pdf", width = 6, height = 6) 

# TO DO:
# relative prevalence of each genre:
# https://www.r-graph-gallery.com/136-stacked-area-chart.html

# absolute values:
genres_relative <- mxm_data %>%
  filter(genre %in% genres$genre[11:20]) %>%
  group_by(genre, year) %>% 
  tally() 

# percentages:
genres_relative <- genres_relative  %>%
  group_by(year, genre) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = n / sum(n))

# plot:
ggplot(data = genres_relative, aes(x = year, y = percentage, fill=genre)) + 
  geom_area(size=.3, colour="white") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(y = "proportion in the dataset", title = "Relative prevalence of 10 most common genres")
# need to adjust the plot and order by final proportion  
  
# #############################################################################
# prepare for sentiment analaysis: 

mxm_data <- mxm_data %>%
  unnest_tokens(word,lyrics)

# load, merge, and tidy LIWC lists of negative/positive emotion words:  
LIWC_negemo <- read_csv("../LIWC/negemo.csv") # see also negemo_no_swear.csv
LIWC_posemo <- read_csv("../LIWC/posemo.csv")
LIWC <- tibble(word = c(LIWC_negemo$Negative, LIWC_posemo$Positive ),
                   sentiment = c(rep("negative", dim(LIWC_negemo)[1]), 
                                 rep("positive", dim(LIWC_posemo)[1])))

# #############################################################################
# cumulative trends of emotion-related words:

emo_trends_mxm <- mxm_data %>%
  inner_join(LIWC) %>%
  count(year, sentiment) %>%
  spread(sentiment, n, fill = 0, convert = T) %>%
  left_join(count(mxm_data, year)) %>% 
  ungroup() %>%
  add_column(genre = "all") 

p1 <- ggplot(data = emo_trends_mxm, aes(year, (positive)/n) )+
  geom_point(shape = 19, color = "#0073C2FF" ) +
  geom_smooth(method = lm, color = "#0073C2FF") +
  labs(y="proportion", title = "Positive emotions (all genres)")  +
  theme_minimal() 

p2 <- ggplot(data = emo_trends_mxm, aes(year, (negative)/n) )+
  geom_point(shape = 19, color = "#CD534CFF" ) +
  geom_smooth(method = lm, color = "#CD534CFF") +
  labs(y="proportion", title = "Negative emotions (all genres)")  +
  theme_minimal()

pdf("../plots/mxm_all.pdf", width = 10, height = 5)
grid.arrange(p1, p2, nrow = 1) 
dev.off() 

# #############################################################################
# plot by genres:

outfiles <- str_replace(genres$genre, "/", "-")
counter <- 1
# average emotionalty of each genre:
av_emo <- tibble(positive = rep(NA, 20), negative = NA, genre  = NA)

for(i in genres$genre){
  mxm_data_genre <- mxm_data %>%
    filter(genre == i)
  
  emo_trends_mxm <- mxm_data_genre %>%
    inner_join(LIWC) %>%
    count(year, sentiment) %>%
    spread(sentiment, n, fill = 0, convert = T) %>%
    left_join(count(mxm_data_genre, year)) %>% 
    ungroup() %>%
    add_column(genre = i) 
  
  av_emo$negative[counter] <- sum(emo_trends_mxm$negative)/sum(emo_trends_mxm$n)
  av_emo$positive[counter] <- sum(emo_trends_mxm$positive)/sum(emo_trends_mxm$n)
  av_emo$genre[counter] <- i
  
  p1 <- ggplot(data = emo_trends_mxm, aes(year, (positive)/n) )+
    geom_point(shape = 19, color = "#0073C2FF" ) +
    geom_smooth(method = lm, color = "#0073C2FF") +
    labs(y="proportion", title = paste("Positive emotions (",i,")", sep = ""))  +
    theme_minimal() 
  
  p2 <- ggplot(data = emo_trends_mxm, aes(year, (negative)/n) )+
    geom_point(shape = 19, color = "#CD534CFF" ) +
    geom_smooth(method = lm, color = "#CD534CFF") +
    labs(y="proportion", title = paste("Negative emotions (",i,")", sep = ""))  +
    theme_minimal()
  
  pdf(paste("../plots/mxm_", outfiles[counter], ".pdf", sep =""), width = 10, height = 5)
  grid.arrange(p1, p2, nrow = 1) 
  dev.off() 
  
  counter <- counter + 1
}

# plot average emo by genre:
av_emo$genre <- as_factor(av_emo$genre)

p1 <- ggplot(data = av_emo, aes(x = reorder(genre, positive), y = positive)) +
  geom_bar(stat = "identity", fill = "#0073C2FF") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Genres ordered by overall positivity", y = "proportion", x = "genre")

p2 <- ggplot(data = av_emo, aes(x = reorder(genre, negative), y = negative)) +
  geom_bar(stat = "identity", fill = "#CD534CFF") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Genres ordered by overall negativity", y = "proportion", x = "genre")

pdf("../plots/overall_emo_genres.pdf", width = 10, height = 5)
grid.arrange(p1, p2, nrow = 1) 
dev.off() 



# is it possible to calculate the relative contribution of each genre to the general trend?

