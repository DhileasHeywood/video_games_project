library(tidyverse)
library(modelr)
library(ggiraphExtra)
library(GGally)
library(fastDummies)


sales_regional <- read_csv("clean_data/sales_all_regions.csv")
sales_global <- read_csv("clean_data/sales_global.csv")


# looking to build a predictive model to identify important predictors of global, or regional sales. 
# Starting with global


# selecting the important columns, and removing education, sandbox and boardgame, as they only have 1 or two observations - unreliable.
model_global_data <- sales_global %>% 
  filter(genre != "Education", genre != "Board Game", genre != "Sandbox") %>% 
  select(-name, -basename, -publisher, -developer, -rank, -year)


model_global_data %>% 
  ggpairs(cardinality_threshold = 17)

# This graph shows that there is a correlation with publisher_ranked, there's a small, negative correlation with year, It looks as though 
# There are correlations with genre, esrb_rating and platform. There's definitely something with publisher_ranked. 

# I'm going to start with genre, esrb_rating, and publisher_ranked

global_mod1a <- lm(global_sales ~ genre, data = model_global_data)
summary(global_mod1a)
# r squared is 0.02969. That is BAD. Let's see if the next predictor is better. 

global_mod1b <- lm(global_sales ~ esrb_rating, data = model_global_data)
summary(global_mod1b)
# r squared is 0.04844. That's double the last one! But still Absolutely dreadful. 

global_mod1c <- lm(global_sales ~ platform, data = model_global_data)
summary(global_mod1c)
# r squared is 0.02309. Back to dreadful again. 

global_mod1d <- lm(global_sales ~ publisher_ranked, data = model_global_data)
summary(global_mod1d)
# r squared is 0.0009536. Oh wells. 






# global_mod1b is clearly the 'best'. It explains about 4.8% of the variance. Let's have a look and see whether it's allowable.
par(mfrow = c(2,2))
plot(global_mod1b)
# residuals vs fitted looks good. 
# normal Q-Q is bad. it's really very not normal
# The scale-location plot is another story. The red line has a slight upward trend, and it's clear that the higher the fitted values, 
# the more spread out the variables. a little concerning. 
# The residuals vs leverage graph isn't concerning. 


# after looking at those, I'm going to have a look at ln(global_sales)


model_global_data <- model_global_data %>% 
  mutate(log_global_sales = log(global_sales)) %>% 
  filter(global_sales > 0)

global_mod1f <- lm(log_global_sales ~ genre, data = model_global_data)
summary(global_mod1f)
# r squared is 0.07158. That is muuuuch better, but still not good overall.  

global_mod1g <- lm(log_global_sales ~ esrb_rating, data = model_global_data)
summary(global_mod1g)
# r squared is 0.1305. That's nearly double the last one! And we're into double figures on the percentage of the variance explained! 

global_mod1h <- lm(log_global_sales ~ platform, data = model_global_data)
summary(global_mod1h)
# r squared is 0.0431. Back to not great. 

global_mod1i <- lm(log_global_sales ~ publisher_ranked, data = model_global_data)
summary(global_mod1i)
# r squared is 0.01055. Oh wells. 



par(mfrow = c(2,2))
plot(global_mod1g)
# This looks much better. The normal QQ is acceptable now, and the scale-location plot I was worried about is fine. 

# I'll be taking mod1g going forward. 




model_global_remaining_resid <- model_global_data %>% 
  add_residuals(global_mod1g) %>% 
  select(-c("log_global_sales", "esrb_rating", "global_sales"))

model_global_remaining_resid %>% 
  ggpairs(cardinality_threshold = 17)

# I'm tempted to say that publisher_ranked is the best of the categoricals. But I'm also intrigued by all of them, because none are 
# especially good. Let's give them a go. 

global_mod2a <- lm(log_global_sales ~ esrb_rating + publisher_ranked, data = model_global_data)
summary(global_mod2a)
# r squared is 0.135. This is not much better than the previous model. I'll have a look at the others, and might perform an ANOVA test
# to see if it's worth adding the predictor. 

global_mod2b <- lm(log_global_sales ~ esrb_rating + platform, data = model_global_data)
summary(global_mod2b)
# r squared is 0.171. This is a pretty big improvement, however, it's important to note, that up until now, the p values of each predictor
# have been (roughly half the time) significant above 0.05. In this case, the re is only one p value < 0.05, and only two others that
# are < 0.1. This isn't great. If this is the best r squared, again, I'll have to perform an ANOVA

global_mod2c <- lm(log_global_sales ~ esrb_rating + genre, data = model_global_data)
summary(global_mod2c)
# r squared is 0.167. Again, and improvement, and the p values are much better. 

anova(global_mod1g, global_mod2b)
# This shows a p value less than 0.01, so we can reject the null hypothesis and say that mod2b is significantly better than mod1g

# I'll now just have a look at the diagnostic plots

par(mfrow = c(2,2))
plot(global_mod2b)

# residuals vs fitted looks good. The line is a bit wobbly, but there's no upward or downward trend. The normalQQ is a little off at 
# either end, but not toooooo much to worry about. Happy with that too. 
# scale_location looks good as well. There's a slight upward trend, but not too much to be concerned about. It wiggles and goes down as
# the fitted values increase. And the residuals vs leverage has nothing to be concerned about. 

# looks like we're going with mod2b going forward. 

model_global_remaining_resid <- model_global_data %>% 
  add_residuals(global_mod2b) %>% 
  select(-c("log_global_sales", "esrb_rating", "global_sales", "platform"))

model_global_remaining_resid %>% 
  ggpairs(cardinality_threshold = 17)
# It's a tough call between genre and publisher_ranked. I'll try both. 

global_mod3a <- lm(log_global_sales ~ esrb_rating + platform + genre, data = model_global_data)
summary(global_mod3a)
# r squared is 0.2021. And again, the p values look good. That's not a bad increase tbh. 

global_mod3b <- lm(log_global_sales ~ esrb_rating + platform + publisher_ranked, data = model_global_data)
summary(global_mod3b)
# r squared is 0.1774, which is a tiny tiny increase from the last one. 


global_mod3c <- lm(log_global_sales ~ esrb_rating + platform + year, data = model_global_data)
summary(global_mod3c)


par(mfrow = c(2,2))
plot(global_mod3a)

# looks like all of the diagnostic plots are good. They're better than the last lot. 


model_global_remaining_resid <- model_global_data %>% 
  add_residuals(global_mod3a) %>% 
  select(-c("log_global_sales", "esrb_rating", "global_sales", "platform", "genre"))

model_global_remaining_resid %>% 
  ggpairs(cardinality_threshold = 17)

# There wasn't much point in doing that, it's the only predictor we have left. 


global_mod4 <- lm(log_global_sales ~ esrb_rating + platform + genre + publisher_ranked, data = model_global_data)
summary(global_mod4)
# r squared is 0.2079, which is barely an increase. I'm going to perform an ANOVA test to confirm whether it's a useless addition or not


anova(global_mod3a, global_mod4)
# The p value for the anova test is 2.2e-16, which is less than 0.01, which means that we can reject the null hypothesis and say that
# mod4 is significantly better than mod3a. 


# At this point the model can predict 20.8% of the variance in the data, which isn't a whole lot. I'll do this again for the regional 
# datasets, and see if we can come up with something that might be useful. 
# I also might revisit this when I've performed the ratings analysis, however, I'm not sure whether I'd like to include ratings in a 
# model, as a game developer isn't going to have ratings for their game before they publish it, so I'm not sure whether that'd be any
# use to them at all. Maybe for once the game has been developed. 


