library(tidyverse)



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


# Globally, games released on Xbox sell more copies on average than on playstation













sales %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(x = year, y = global_sales, fill = genre)) +
  geom_col() + 
  scale_fill_viridis_d()

sales %>% 
  filter(!is.na(year)) %>% 
  group_by(year) %>% 
  summarise(number_released = n(), global_sales = sum(global_sales)) %>% 
  mutate(sales_per_games_released = (global_sales/number_released)) %>% 
  ggplot(aes(x = year, y = sales_per_games_released))+
  geom_col()+
  scale_fill_viridis_b()


sales %>% 
  filter(!is.na(year)) %>% 
  group_by(year) %>% 
  summarise(number_released = n(), global_sales = sum(global_sales)) %>%
  ggplot(aes(x = number_released, y = global_sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

