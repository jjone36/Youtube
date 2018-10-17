library(tidyverse)
library(magrittr)
library(gridExtra)
library(lubridate)
library(jsonlite)
library(rvest)
library(tuber)
library(rebus)

# Youtube API ####
#https://help.aolonnetwork.com/hc/en-us/articles/218079623-How-to-Create-Your-YouTube-API-Credentials#1
api_key = '542589435889-5o92q1aojo6lan2qmgd8in1v7hvdmuv6.apps.googleusercontent.com'
api_secret = 'ngqxw_Z2ZvVI_ZOI8mQvqlK6'

yt_oauth(app_id = api_key, app_secret = api_secret)
file.remove('.httr-oauth')

# Clustering Analysis : Youtube channel and Video Content 

# 1. Importing dataset ####
us = read_csv("USvideos.csv")
str(us)


# 2. Inspecting dataset column by column
# 2-1. trending_date 
us$trending_date = as.Date(us$trending_date, '%y.%d.%m')
range(us$trending_date)
range(us$publish_time)

# 2-2. channel_title 
n_distinct(us$channel_title)

us = us %>%
  group_by(channel_title) %>%
  count() %>%
  right_join(us)
 
# top 20 channels for the highest number of videos
us[1:20, ] %>%
ggplot(aes(x = reorder(channel_title, n), y = n)) + 
  geom_col(fill = 'lightseagreen') + 
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 20 channels for the hightest number of videos') + 
  theme_bw()

# subscribers per channel
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
str(us)

us$subscribe = us$subscribe / 10000



# 2-3. category_id 
table(us$category_id)
us_category = fromJSON("US_category_id.json", flatten = T)

str(us_category)
items = us_category$items[, c(3, 5)]

for(i in 1:nrow(us)){
  us$topic[i] = items$snippet.title[us$category_id[i] == items$id]
}

str(us)

# write.csv(x = us, file = 'us2.csv', row.names = F)
# the number of videos and channels per topic
n_topic = us %>%
  group_by(topic) %>%
  summarise(vn_topic = sum(n)) %>%
  inner_join(us, by = 'topic') %>%
  select(topic, vn_topic, channel_title) %>%
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


# total number of views, likes, and dislikes per topic
topic_sum = us %>%
  group_by(topic) %>%
  summarise(total_like = sum(likes),
            total_dislike = sum(dislikes),
            total_view = sum(views, na.rm = T)) 

p1 = ggplot(topic_sum, aes(x = reorder(topic, total_view), y = total_view)) + 
  geom_col(fill = 'darkgreen') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Total Number of Views') +
  theme_bw()

p2 = ggplot(topic_sum, aes(x = reorder(topic, total_view), y = total_like)) + 
  geom_col(fill = 'steelblue') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Total Number of Likes') +
  theme_bw()

p3 = ggplot(topic_sum, aes(x = reorder(topic, total_view), y = total_dislike)) + 
  geom_col(fill = 'indianred1') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Total Number of Dislikes') +
  theme_bw()

grid.arrange(p1, p2, p3, nrow = 3)


# 2-4. title, tags




# 2-5. descriptions







