library(janitor)
library(tidyverse)


#reading in data, and removing unhelpful columns. 
sales <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names() %>% 
  # img_url and url aren't useful for analysis. last_update is unrelated to the game.
  # vg_chartz_score is only NA values
  # status only has values of 1
  # total_shipped only has NA values. 
  # user_score only has 174 / ~19000 values that are not NA
  select(-img_url, -url, -last_update, -vg_chartz_score, -status, -total_shipped, -user_score, -vgchartzscore) %>% 
  # I'm interested in global sales, so I'm going to drop NA values from that column
  filter(!is.na(global_sales))

view(sales)
unique(sales$total_shipped)

sum(!is.na(sales$vgchartzscore))

sales_ratings <- read_csv("raw_data/sales-2016-with-ratings.csv")




sales %>% 
  filter(!is.na(global_sales), !is.na(genre)) %>%
  group_by(genre) %>% 
  summarise(mean_global_sales = mean(global_sales)) %>% 
  ggplot(aes(x = genre, y = mean_global_sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45))

# As expected, the data is REALLY not normally distributed. Of course there are lots of games that don't sell much, and loads that do sell
# really well. 
sales %>% 
  filter(!is.na(global_sales), !is.na(genre)) %>%
  group_by(genre) %>% 
  ggplot(aes(x = genre, y = global_sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45))

# This isn't going to work. It leaves no data so I'm taking it out. 
#no_na_sales <- sales %>% 
#  drop_na()


# I'd like to see if global sales relates to rating at all (I'm expecting a positive correlation). 
# I'll do one for customer rating and one for critic rating.

sales %>% 
  filter(!is.na(global_sales), !is.na(critic_score), !is.na(user_score)) %>% 
  ggplot(aes(y = global_sales)) +
  geom_point(aes(x = critic_score), colour = "red")+
  geom_point(aes(x = user_score), colour = "blue")

# There's definitely a strong positive correlation between scores and global sales. and it looks as though critics give more low scores
#  to games.

# I'd like to see whether there's a difference in global sales and sales in other parts of the world. 

sales %>% 
  select(global_sales, na_sales, jp_sales, pal_sales) %>%
  drop_na() %>% 
  ggplot(aes(y = global_sales)) +
  geom_point(aes(x = na_sales), colour = "blue") +
  geom_point(aes(x = pal_sales), colour = "green") +
  geom_smooth(aes(x = na_sales), method = "lm", colour = "dark blue", alpha = 0.5, se = FALSE) +
  geom_point(aes(x = jp_sales), colour = "red") +
  geom_smooth(aes(x = jp_sales), method = "lm", colour = "dark red", alpha = 0.5, se = FALSE) +
  geom_smooth(aes(x = pal_sales), method = "lm", colour = "dark green", alpha = 0.5, se = FALSE)

# Okay, so clearly European and American sales are postitively correlated with global sales. Japanese sales are a little different. 
# They're distributed in a cone with a much wider diameter. Makes the line of best fit a bit weird because there's no jp_sales above 3.0 m



#I'd like to see if rating has any correlation with global sales

sales %>% 
  select(global_sales, esrb_rating) %>% 
  group_by(esrb_rating) %>% 
  summarise(mean_global_sales = mean(global_sales)) %>% 
  ggplot(aes(x = esrb_rating, y = mean_global_sales)) +
  geom_col()
# It does look like the mean global sales does correlate with the rating. The mean sales for games rated Mature is almost double the 
# next highest. 






