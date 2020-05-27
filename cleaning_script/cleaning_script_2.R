library(janitor)
library(tidyverse)
library(ggpmisc)


ratings <- read_csv("raw_data/sales-2016-with-ratings.csv") %>% 
  clean_names() %>% 
  select(-rating, -developer) %>% 
  filter(!is.na(name), !is.na(genre)) %>% 
  mutate(user_score = as.numeric(na_if(user_score, "tbd"))) %>% 
  filter(!is.na(critic_score) | !is.na(user_score))

# I'm making a linear model of critic score varying with user_score, so that I can impute the missing values in user_score and
# critic_score where there's only one missing of the two. 
rating_score_model <- lm(critic_score ~ user_score, data = ratings)

rating_score_intercept <- rating_score_model$coefficients[1]
rating_score_gradient <- rating_score_model$coefficients[2]


ratings_clean <- ratings %>%
  mutate(user_score = ifelse(is.na(ratings$user_score), (critic_score  - rating_score_intercept) / rating_score_gradient, user_score),
         critic_score = ifelse(is.na(ratings$critic_score), rating_score_intercept + (rating_score_gradient * user_score), critic_score))


write_csv(ratings_clean, path = "clean_data/ratings_clean.csv")

#confirming that the NA imputing has worked. 
steve <- tibble(ratings$critic_score - ratings_clean$critic_score)

ratings %>% 
  filter(is.na(user_score) | is.na(critic_score))

# counting NA values in each column. 

ratings %>% 
  mutate(name = sum(is.na(name)),
         genre = sum(is.na(genre)),
         platform = sum(is.na(platform)),
         publisher = sum(is.na(publisher)),
         global_sales = sum(is.na(global_sales)),
         na_sales = sum(is.na(na_sales)),
         eu_sales = sum(is.na(eu_sales)),
         jp_sales = sum(is.na(jp_sales)),
         other_sales = sum(is.na(other_sales)),
         year_of_release = sum(is.na(year_of_release)),
         critic_score = sum(is.na(critic_score)),
         critic_count = sum(is.na(critic_count)),
         user_score = sum(is.na(user_score)),
         user_count = sum(is.na(user_count))) %>%
  head(1)

# There's a lot of missing values in the critic score and count columns. There's also a lot of missing data for the user score and particularly the user count columns. I'm going to drop
# the rating column because that data is already present in the other data set. I'll also drop the developer column for the same reason. 

# Removing the rows in name and genre that have missing values. 
# Of the 8582 values that are missing in the critic score and critic count columns, all of them are missing in both columns. 

# There are 6704 rows where user score and user count are both NA. In all columns where user score is missing, it's also missing in user count. 
# In the rows where user_count is NA, it looks as though the first few rows are all "tbd". checking whether that's the case everywhere. 
# Which it is. Changing "tbd" values to NA in user_score, and checking for tbd values elsewhere, which I can't find. 
# However, on those data adventures, I found a "N/A" value in year of release. Checking for more. 
# There are 269 rows where year_of release is "N/A". 

# What am I interested in in this data set? I'm interested in critic score and user score. If I take out all of the NA values, I'll only have 7000 observations, which really isn't all that
# many. I'll drop all of the rows where both of them are NA. I'll check how different the user and critic scores, and I'll consider imputing them with the other. 
# Taking those out has left us with 573 NAs in critic score and count, and 1120 NAs in user score and count.




ratings %>% 
  drop_na() %>% 
  ggplot(aes(x = user_score, y = critic_score)) +
  geom_point()+
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(formula = formula,
               aes(label = paste(after_stat(eq.label))), 
               parse = TRUE)

# From this we can see that the formula for this line of best fit is y = 5.59 x + 30.1. 
# I'm going to extract these from a linear model so that they're not hard coded, then I will transform the data.

rating_score_model <- lm(critic_score ~ user_score, data = ratings)

rating_score_intercept <- rating_score_model$coefficients[1]
rating_score_gradient <- rating_score_model$coefficients[2]

ratings %>% 
  filter(is.na(critic_score) & is.na(user_score))
