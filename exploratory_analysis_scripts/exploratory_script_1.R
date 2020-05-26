library(tidyverse)


sales_global <- read_csv("clean_data/sales_global.csv")
sales_north_america <- read_csv("clean_data/sales_north_america.csv")
sales_europe <- read_csv("clean_data/sales_europe.csv")
sales_japan <- read_csv("clean_data/sales_japan.csv")
sales_other <- read_csv("clean_data/sales_other.csv")
sales_all_regions <- read_csv("clean_data/sales_all_regions.csv")


# Firstly, I'd like to see which genres have the most sales for each region
sales_global %>% 
  filter(!str_detect(name, "[Mm]inecraft")) %>% 
  group_by(genre) %>% 
  summarise(mean_global_sales = mean(global_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = genre, y = mean_global_sales, fill = number_games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_b() +
  ylim(0, 0.7)
  
sales_north_america %>% 
  filter(!str_detect(name, "[Mm]inecraft")) %>% 
  group_by(genre) %>% 
  summarise(mean_sales = mean(na_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = genre, y = mean_sales, fill = number_games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_b() +
  ylim(0, 0.7)

sales_europe %>% 
  filter(!str_detect(name, "[Mm]inecraft")) %>% 
  group_by(genre) %>% 
  summarise(mean_sales = mean(pal_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = genre, y = mean_sales, fill = number_games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_b() +
  ylim(0, 0.7)

sales_japan %>% 
  filter(!str_detect(name, "[Mm]inecraft")) %>% 
  group_by(genre) %>% 
  summarise(mean_sales = mean(jp_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = genre, y = mean_sales, fill = number_games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_b() +
  ylim(0, 0.7)

sales_other %>% 
  filter(!str_detect(name, "[Mm]inecraft")) %>% 
  group_by(genre) %>% 
  summarise(mean_sales = mean(other_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = genre, y = mean_sales, fill = number_games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_b() +
  ylim(0, 0.7)

# From these, it looks like Minecraft is unbelievably popular, which heavily affects the results. Looking at them all without minecraft, we can 
# get a better picture of how the sales of other genres of games are distributed. 
# It's also interesting to note that the only game in the 'sandbox' genre is Minecraft. For that reason, if I do make a predictive model, I'll
# take Minecraft out of the data, because it will *really* screw things up by saying that if your game is sandbox, it'll be super successful.
# There is insufficient data on sandbox games.
# It's also important to note that different regions have different tastes in video games. For example, in Japan, the most popular genre is Party,
# whereas in North America, the most popular is Shooter, and in Europe it is Action-Adventure. 
# I've put all of the graphs on the same scale so it's possible to see how many games are bought in each region in comparison to globally. For 
# example, Party games sell, on average, 0.19 million copies in Japan, and they sell, on average, about 0.21 million copies worldwide, suggesting
# that most party games that are sold are sold in Japan. 



# I'd like to see how data is distributed. I'm expecting most games will be on the low end, and there will be a lot of outliers with very high 
# global sales. 
sales_global %>% 
  group_by(genre) %>% 
  ggplot(aes(x = genre, y = global_sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45))
# As expected, the data is REALLY not normally distributed. Of course there are lots of games that don't sell much, and loads that do sell
# really well.


# I'd like to see whether there's a difference in global sales and sales in other parts of the world. 

sales_all_regions %>% 
  ggplot(aes(x = global_sales, y = regional_sales)) +
  geom_point(aes(colour = region)) +
  geom_smooth(aes(colour = region), method = "lm", alpha = 0.5, se = FALSE) +
  xlab("Global Sales") +
  ylab("Regional Sales")

# Okay, so clearly European and American sales are postitively correlated with global sales. Japanese sales have a much lower correlation with 
# global sales (the gradient of the line is much shallower), which means that sales of a game in Japan are less good at predicting if a game is
# going to sell well globally.





#I'd like to see if esrb rating has any correlation with global sales

sales_global %>% 
  group_by(esrb_rating) %>% 
  summarise(mean_global_sales = mean(global_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = esrb_rating, y = mean_global_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_b() 
# It does look like the mean global sales does correlate with the rating. The mean sales for games rated Mature is almost double the 
# next highest. Checking whether that is the case for other regions.
# In addition, there are at least twice as many games rated E than M. Despite this, the games rated M sell on average twice as many copies. 

sales_north_america%>% 
  group_by(esrb_rating) %>% 
  summarise(mean_sales = mean(na_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = esrb_rating, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_b()

sales_europe %>% 
  group_by(esrb_rating) %>% 
  summarise(mean_sales = mean(pal_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = esrb_rating, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_b()

sales_japan %>% 
  group_by(esrb_rating) %>% 
  summarise(mean_sales = mean(jp_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = esrb_rating, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_b()

sales_other%>% 
  group_by(esrb_rating) %>% 
  summarise(mean_sales = mean(other_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = esrb_rating, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_b()


# What this tells us is that there is a lot of missing data for ratings!
# Having said that, it is telling with regards to what rating of game is sold where. 
# Globally, in North America, Europe and other regions, games rated Mature sell more copies on average. 
# In Japan, however, the games that sell the most copies on average are rated E for 'Suitable for Everyone".



# I'd like to look at platform now. 

sales_global %>% 
  group_by(platform) %>% 
  summarise(mean_sales = mean(global_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = platform, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(0, 0.6))

sales_north_america %>% 
  group_by(platform) %>% 
  summarise(mean_sales = mean(na_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = platform, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(0, 0.6))

sales_europe %>% 
  group_by(platform) %>% 
  summarise(mean_sales = mean(pal_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = platform, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(0, 0.6))

sales_japan %>% 
  group_by(platform) %>% 
  summarise(mean_sales = mean(jp_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = platform, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(0, 0.6))

sales_other %>% 
  group_by(platform) %>% 
  summarise(mean_sales = mean(other_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = platform, y = mean_sales, fill = number_games)) +
  geom_col() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(0, 0.6))


# Globally, games released on Xbox sell more copies on average than on playstation, despite there being more games released on playstation. 
# nintendo has the third highest average global sales with 0.3 million. PC is surprisingly low in my opinion, but I'm biased as a PC gamer. 

# looking at the data from North America, games released on x box have the highest mean number of sales, again, followed by playstation, and like
# the global data, playstation has more games released. 

# In Europe, games released on playstation have the highest mean number of sales, as well as the highest number of games released. 

# In Japan, the platform with the highest mean number of sales is Nintendo, followed by Playstation and platforms not in the usual big 4, which
# makes sense, as most of the other platforms are Japanese. 

# In other regions, Playstation has the highest mean number of sales, follwed by Xbox. 






# I'm interested to see how year released affects this data.


sales_global %>% 
  ggplot(aes(x = year, y = global_sales)) +
  geom_col() 

# Here we can see that The data is left skewed, with a peak in 2008 when around 575 million games were sold in total. There aren't many sales in 
# 2019 or 2020. This isn't surprising for 2020 as it's still early in the year, but it's surprising for 2019. I suspect this is due to a lack of 
# data, rather than people not buying games. 
# I'd like to see how many games were released each year. 

sales_global %>% 
  group_by(year) %>% 
  summarise(number_released = n()) %>% 
  ggplot(aes(x = year, y = number_released)) +
  geom_col() 

# There is also a peak in number of games released in 2008, which corresponds to the peak in number of sales in 2008. 

# I'd like to split this graph up into genres. 
sales_global %>% 
  group_by(year, genre) %>% 
  summarise(number_released = n()) %>% 
  ggplot(aes(x = year, y = number_released, fill = genre)) +
  geom_col() + 
  scale_fill_viridis_d()

# Ow my eyes. Having said that it looks like there are a lot of sports games released, especially in 2005-2010.
# let's look at that in more detail. 

sales_global %>% 
  filter(genre == "Sports" | genre == "Shooter" | genre == "Action" | genre == "Action-Adventure") %>% 
  group_by(year, genre) %>% 
  summarise(number_released = n()) %>% 
  ggplot(aes(x = year, y = number_released, fill = genre)) +
  geom_col() + 
  scale_fill_viridis_d()

# It's easier to say that the number of sports games released each year has been decreasing since 2010. It looks like the average number of Action 
# games released each year has stayed about the same, maybe decreasing a bit. 
# In addition, Action-Adventure games didn't exist before 2012. 


# I'd like to look at how many games are sold compared to how many are released. 
sales_global %>% 
  group_by(year) %>% 
  summarise(number_released = n(), global_sales = sum(global_sales)) %>% 
  mutate(sales_per_games_released = (global_sales/number_released)) %>% 
  ggplot(aes(x = year, y = sales_per_games_released))+
  geom_col()

# So again, 2019 and 2020 are a bit low, but I think it's due to lack of data about these years. 
# from about 1995 to 2018, it looks as though the trend is cyclical with a time of 6 years or so. fluctuating between about 0.25 million games 
# sold per game released and 0.5 million games sold per game released. Minecraft was released in 2014, which is going to skew things, because 
# minecraft is unbelievably popular. 


# I'd really like to look at some of the above graphs for more recent years, since it's probably not nearly as relevant to be looking at what the 
# industry was like in the 90s. I'm going to look at all games released since 2000

sales_global %>% 
  filter(!str_detect(name, "[Mm]inecraft"), year >= 2000) %>% 
  group_by(genre) %>% 
  summarise(mean_global_sales = mean(global_sales), number_games = length(unique(name))) %>% 
  ggplot(aes(x = genre, y = mean_global_sales, fill = number_games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_b() 

# This graph is relatively similar to the initial graph. 


