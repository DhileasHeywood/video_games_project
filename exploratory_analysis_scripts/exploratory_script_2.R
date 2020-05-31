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
  select(-name, -basename, -publisher, -developer, -rank)


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

global_mod1e <- lm(global_sales ~ year, data = model_global_data)
summary(global_mod1e)
# r squared is 0.001734. I guess we're not going to get a good model out of these.... 





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

global_mod1j <- lm(log_global_sales ~ year, data = model_global_data)
summary(global_mod1j)
# r squared is 0.03846. I guess we're not going to get a good model out of these.... 

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


