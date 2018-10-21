library(tidyverse)
library(magrittr)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(GGally)
library(jsonlite)
library(rvest)
library(tuber)
library(tidytext)
library(text2vec)
library(caret)
library(glmnet)
library(xgboost)

# This is the full code for the Youtuber strategy 
# It is EDA with Youtube data and to predict the view counts based on video content using word embedding
# The raw dataset here : https://www.kaggle.com/datasnaek/youtube-new


# 1. Importing Data ####
us = read_csv("USvideos.csv")

us$trending_date = as.Date(us$trending_date, '%y.%d.%m')

table(us$category_id)
us_category = fromJSON("US_category_id.json", flatten = T)

str(us_category)
items = us_category$items[, c(3, 5)]

for(i in 1:nrow(us)){
  us$topic[i] = items$snippet.title[us$category_id[i] == items$id]
}

str(us)

# scraping subscriber numbers from Youtube API 
us$channel_id = 0
for(i in 1:nrow(us)){
  a = get_video_details(video_id = us$video_id[i])$items[[1]]$snippet$channelId
  us$channel_id[i] = ifelse(length(a)==0, NA, a)
}

us$subscribe = 0
for(i in 1:nrow(us)){
  if(is.na(us$channel_id[i])){
    us$subscribe[i] == 0
  } else {
    b = get_channel_stats(channel_id = us$channel_id[i])$statistics$subscriberCount
    us$subscribe[i] = ifelse(length(b)==0, 0, b)
  }
}

sum(is.na(us$channel_title))
sum(us$subscribe == 0)

us$subscribe = as.integer(us$subscribe)


range(us$publish_time)


# 2. Exploratory Data Anaylsis ####  
# 2-1. channel_title => channel_id & subscribers
str(us)
us$video_id = factor(us$video_id)

n_distinct(us$channel_title)    # -> 2207 channels

us = us %>%
  group_by(channel_title) %>%
  count() %>%
  right_join(us) %>%
  ungroup()
 
# top 20 channels for the highest number of videos
us %>%
  distinct(channel_title, n) %>%
  arrange(-n) %>%
  head(20) %>%
  ggplot(aes(x = reorder(channel_title, n), y = n)) + 
  geom_col(fill = 'lightseagreen') + 
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 20 channels for the hightest number of videos') + 
  theme_bw()

# top 20 channels for the highest number of subscribers
us %>%
  group_by(channel_title) %>%
  summarise(avg_sub = median(subscribe)) %>%
  arrange(-avg_sub) %>%
  head(20) %>%
  ggplot(aes(x = reorder(channel_title, avg_sub), y = avg_sub)) + 
  geom_col(fill = 'darkorange1') +
  coord_flip() + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x = NULL, y = NULL, title = 'Top 20 Channels for the highest number of subscribers') + 
  theme_bw()
 

# 2-2. category_id => topic analysis  
# the number of videos & channels per topic
n_topic = us %>%
  group_by(topic) %>%
  summarise(vn_topic = sum(n))

n_topic = n_topic %>%
  inner_join(us, by = 'topic') %>%
  distinct(topic, channel_title) %>%
  group_by(topic) %>%
  count() %>%
  inner_join(n_topic, by = 'topic')

a1 = n_topic %>%
  distinct(topic, vn_topic) %>%
  ggplot(aes(x = reorder(topic, vn_topic), y = vn_topic)) +
  geom_col(fill = 'steelblue') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'The Number of Videos per Topic') +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

a2 = n_topic %>%
  distinct(topic, vn_topic, n)%>%
  ggplot(aes(x = reorder(topic, vn_topic), y = n)) +
  geom_col(fill = 'coral1') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'The Number of Channels per Topic') +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

grid.arrange(a1, a2, nrow = 2)


# total number of views, likes, dislikes and comments per topic
topic_avg = us %>%
  group_by(topic) %>%
  summarise(avg_like = median(likes),
            avg_dislike = median(dislikes),
            avg_view = median(views, na.rm = T), 
            avg_comment = median(comment_count, na.rm = T)) 

p1 = ggplot(topic_avg, aes(x = reorder(topic, total_view), y = total_view)) + 
  geom_col(fill = 'lightseagreen') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Median Number of Views') +
  theme_bw()

p2 = ggplot(topic_avg, aes(x = reorder(topic, total_view), y = total_comment)) + 
  geom_col(fill = 'darkorange1') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Median Number of Comments') +
  theme_bw()

grid.arrange(p1, p2, nrow = 1)


p3 = ggplot(topic_avg, aes(x = reorder(topic, total_view), y = total_like)) + 
  geom_col(fill = 'steelblue') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Median Number of Likes') +
  theme_bw()

p4 = ggplot(topic_avg, aes(x = reorder(topic, total_view), y = total_dislike)) + 
  geom_col(fill = 'indianred1') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Median Number of Dislikes') +
  theme_bw()

grid.arrange(p3, p4, nrow = 1)


# 2-3. publish_time & trending_date 
# time period need to be trendy 
range(us$trending_date)
range(us$publish_time)

us_time = us %>%
  select(channel_id, video_id, topic, publish_time, trending_date, subscribe) %>%
  mutate(upload_date = as.Date(publish_time, format = '%Y-%m-%d'), 
         period = as.integer(trending_date - upload_date))

myTheme = theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                panel.background = element_rect(fill = 'white', color = 'grey50'),
                panel.grid.major = element_line(color = 'grey90'))

# trending period distribution
us_time %>%
  filter(period < 30) %>%
  ggplot(aes(x = topic, y = period, fill = topic)) + 
  geom_boxplot(show.legend = F, varwidth = T) +
  labs(x = NULL, y = 'Days', title = 'Days To Be Trending') +
  myTheme

summary(us_time$period)  # -> period <= 3  for Top .25% 

# the distribution of the subscriber numbers for trending videos
hist(us_time$subscribe[us_time$period <= 3], xlab = NULL, main = 'The Subscriber Numbers of Trendy Videos')

# the subscriber numbers of top trendy channels (be trendy within a day)
us_time %>% 
  filter(period < 2) %>% 
  group_by(channel_id) %>%
  mutate(avg_sub = mean(subscribe))  %>%
  ggplot(aes(x = topic, y = avg_sub, fill = topic)) + 
  geom_boxplot(show.legend = F, varwidth = T) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL, title = 'The Subscribers Numbers of Top Trendy channels') + 
  myTheme 

# the top trendy video numbers per topic 
us_time %>%
  filter(period < 2) %>%
  group_by(topic) %>%
  count() %>%
  ggplot(aes(x = topic, y = n, fill = topic)) + 
  geom_col(show.legend = F) +
  labs(x = NULL, y = 'Video Counts', title = 'The Number of Trendy Videos per Topic') +
  myTheme


# 2-4. the length of title vs views
us$len_title = str_length(us$title)
us$nword_title = str_count(us$title, pattern = '[\\w]+')

# the distribution of the title length
hist(us$len_title, main = 'The Length of Title', xlab = NULL)

# the distribution of the word counts of the title
hist(us$nword_title, main = 'The Word Count of Title', xlab = NULL)

summary(us$views)

# view count vs. title length 
us %>%
  ggplot(aes(x = len_title, y = views)) + 
  geom_point(col = 'lightseagreen') +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, title = 'View Count vs. Title Length') + 
  theme_bw()

# + facet by topic
us %>%
  ggplot(aes(x = len_title, y = views, col = topic)) + 
  geom_point(show.legend = F) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(topic~.) + 
  labs(x = NULL, title = 'View Count vs. Title Length') + 
  theme_bw()


# view count vs word count of the title  
us %>%
  ggplot(aes(x = nword_title, y = views)) + 
  geom_point(color = 'darkorange1') +
  scale_x_continuous(limits = c(0, 50)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, title = 'View Count vs. Title Word Count') + 
  theme_bw()

# + facet by topic
us %>%
  ggplot(aes(x = nword_title, y = views, col = topic)) + 
  geom_point(show.legend = F) +
  scale_x_continuous(limits = c(0, 50)) + 
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(topic~.) +
  labs(x = NULL, title = 'View Count vs. Title Word Count') + 
  theme_bw()


# 2-5. the number of tags vs views  
str(us$tags) 

us$n_tags = str_count(us$tags, pattern = '\"\\|\"') + 1
hist(us$n_tags, main = 'The Number of Tags', xlab = NULL)

us$n_tags[us$views > 200000000]
summary(us$n_tags)

# the number of tags per topic
ggplot(us, aes(x = topic, y = n_tags, fill = topic)) +
  geom_boxplot(show.legend = F, varwidth = T) +   
  labs(x = NULL, y = NULL, title = 'The Number of Tags per Topic') + 
  myTheme

# view counts vs tag numbers 
ggplot(us, aes(x = n_tags, y = views)) + 
  geom_point(col = 'steelblue') +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = NULL, title = 'View Count vs. Tag Numbers') + 
  theme_bw()

# + facet by topic
ggplot(us, aes(x = n_tags, y = views, col = topic)) +
  geom_col(show.legend = F) + 
  facet_wrap(topic~., scales = 'free_y') + 
  labs(x = NULL, y = NULL, title = 'View Count vs Tag Numbers per Topic') 


table(us$topic)

keyword = function(x){
  # unnest token the tag column
  tag_tidy = us %>%
    filter(topic == x) %>%
    filter(views > quantile(us$views, probs = .75)) %>%
    select(tags) %>%
    unnest_tokens(output = 'word', input = tags) 
  
  # wordcloud plot for hot keyword
  tag_tidy %>%
    count(word) %>%
    arrange(desc(n)) %$%
    wordcloud::wordcloud(words = word, freq = n, min.freq = 50, scale = c(.5, 1.5), max.words = 50, colors = brewer.pal(8, 'Dark2'))
}

par(mfrow=c(2, 3))

keyword('Comedy')
keyword('Entertainment')
keyword('Gaming')
keyword('Howto & Style')
keyword('Music')
keyword('Pets & Animals')
keyword('Travel & Events')


# 2-6. Pairs Plot & correlation 
us %>%
  select(subscribe, views, likes, dislikes, comment_count, len_title, nword_title, n_tags) %>%
  ggpairs()

# 3. Text Mining ####
# 3-1. feature engineering
us_time = us_time %>%
  select(publish_time, period) %>%
  mutate(year = year(publish_time),
         month = month(publish_time), 
         day = day(publish_time), 
         hour = hour(publish_time), 
         wday = wday(publish_time)) %>%
  select(-publish_time) %>%
  mutate_all(factor) %>%
  mutate(period = as.integer(period))

us_factor = us %>%
  select(comments_disabled, ratings_disabled, video_error_or_removed) %>%
  mutate_all(factor) %>%
  mutate_all(as.integer)

us_count = us %>%
  select(channel_id, n, views, subscribe, len_title, nword_title, n_tags) %>%
  group_by(channel_id) %>%
  mutate(subscribe = log1p(mean(subscribe))) %>%
  ungroup() %>%
  select(-channel_id) %>%
  mutate(views = log1p(views))


# 3-2. word embedding 
us_text = us %>%
  select(title, tags, description, views) %>%
  mutate(text = paste(title, tags, description),
         id = us$video_id) %>%
  select(id, text)

us_text$text = gsub(us_text$text, pattern = '[^[:alnum:]]', replacement = ' ')
us_text$text = tolower(us_text$text)

full = cbind(us_text, us_time, us_factor, us_count)

str(full)
names(full)
sapply(full, function(x){sum(is.na(x))})

# creating an iterator and a vocabulary 
it = itoken(iterable = full$text,
            tokenizer = word_tokenizer)  

v = create_vocabulary(it, stopwords = stopwords::stopwords('en')) %>%
  prune_vocabulary(doc_proportion_max = .5, term_count_min = 5)
print(v)

vectorizer = vocab_vectorizer(v)

# casting into dtm with Tf-Idf
dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)

dim(dtm_tfidf)

# splitting data into train and test set
part = sample(x = nrow(full), size = nrow(full)*0.8)

tr_y = full$views[part]
actual = full$views[-part]
te_y = actual
te_y = NA

tr_te = full %>%
  select(-text, -id, -views) %>%
  model.matrix(~.-1, .) %>%
  cbind(dtm_tfidf)

tr = tr_te[part, ] 
te = tr_te[-part, ] 

# 4. Fitting the model ####
rm(tr_te, us, us_count, us_factor, us_text, us_time, v)

# 4-1. glmnet
model_glmnet = cv.glmnet(x = tr, y = tr_y, family = 'gaussian', 
                         nfolds = 5, alpha = 0, type.measure = 'mse')
model_glmnet$lambda.min

pred_glmnet = predict(model_glmnet, newx = te, s = 'lambda.min')

hist(pred_glmnet)
hist(actual)

rmse = function(x){
  return(sqrt(mean(x^2)))
}
rmse(pred_glmnet - actual)


# 4-2. xgboost
dtr = xgb.DMatrix(tr, label = tr_y)
dte = xgb.DMatrix(te)

myParam <- list(objective = "reg:linear",
                booster = "gbtree",
                eval_metric = "rmse",
                nthread = 4,
                eta = 0.05,
                max_depth = 8,
                min_child_weight = 5,
                subsample = 0.7,
                colsample_bytree = 0.7)

model_xgb = xgboost(dtr, tr_y, param = myParam, 
                    nrounds = 3000, print_every_n = 100, early_stopping_rounds = 100)

pred_xgb = predict(model_xgb, dte)

hist(pred_xgb)
hist(actual)

rmse(pred_xgb - actual)

xgb.importance(feature_names = names(tr), model = model_xgb) %>% xgb.plot.importance(top_n = 30)


