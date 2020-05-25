library(janitor)
library(tidyverse)


sales_all_regions <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names() %>% 
  # img_url and url aren't useful for analysis. last_update is unrelated to the game.
  # vg_chartz_score is only NA values
  # status only has values of 1
  # total_shipped only has NA values. 
  # user_score only has 174 / ~19000 values that are not NA
  select(-img_url, 
         -url, 
         -last_update, 
         -vg_chartz_score, 
         -status, 
         -total_shipped, 
         -user_score, 
         -vgchartzscore, 
         -critic_score) %>% 
  # I'm interested in global sales, so I'm going to drop NA values from that column
  filter(!is.na(global_sales),
         !is.na(na_sales),
         !is.na(pal_sales),
         !is.na(jp_sales),
         !is.na(other_sales),
         !is.na(year)) %>% 
  # Changing NA values in the esrb rating column to be 'unrated', to avoid confusion, and to allow for modelling
  mutate(esrb_rating = replace_na(esrb_rating, "unrated")) %>% 
  # pivoting the table longer for ease of analysis
  pivot_longer(cols = na_sales:other_sales, names_to = "region", values_to = "regional_sales") %>% 
  # getting rid of the _sales suffix from each region, and making the regions more intelligible
  separate(col = region, sep = "_", into = c("region", "junk")) %>% 
  select(-junk) %>% 
  mutate(region = ifelse(region == "na", "north america", region),
         region = ifelse(region == "pal", "europe", region),
         region = ifelse(region == "jp", "japan", region)) %>% 
  mutate(platform = ifelse(str_detect(platform, "^PS[A-Za-z0-9]*"), "playstation", platform),
         platform = ifelse(str_detect(platform, "^X[A-Z0-9a-z]+"), "xbox", platform),
         platform = ifelse(str_detect(platform, "Mob"), "mobile", platform),
         platform = ifelse(str_detect(platform, "N[A-FH-Z0-9]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^G[ABC]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "W[A-Za-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform), 
         platform = ifelse(platform == "sony" | platform == "xbox" | platform == "mobile" | platform == "nintendo" | platform == "pc", 
                           "other", platform))

write_csv(sales_all_regions, path = "clean_data/sales_all_regions.csv")



#reading in data, and removing unhelpful columns. 
sales_global <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names() %>% 
  # img_url and url aren't useful for analysis. last_update is unrelated to the game.
  # vg_chartz_score is only NA values
  # status only has values of 1
  # total_shipped only has NA values. 
  # user_score only has 174 / ~19000 values that are not NA
  select(-img_url, 
         -url, 
         -last_update, 
         -vg_chartz_score, 
         -status, 
         -total_shipped, 
         -user_score, 
         -vgchartzscore, 
         -critic_score,
         -pal_sales, 
         -na_sales, 
         -jp_sales,
         -other_sales) %>% 
  # I'm interested in global sales, so I'm going to drop NA values from that column
  filter(!is.na(global_sales),
         !is.na(year),
         !str_detect(name, "\\(JP sales\\)")) %>% 
  # Changing NA values in the esrb rating column to be 'unrated', to avoid confusion, and to allow for modelling
  mutate(esrb_rating = replace_na(esrb_rating, "unrated")) %>% 
  mutate(platform = ifelse(str_detect(platform, "^PS[A-Za-z0-9]*"), "playstation", platform),
         platform = ifelse(str_detect(platform, "^X[A-Z0-9a-z]+"), "xbox", platform),
         platform = ifelse(str_detect(platform, "Mob"), "mobile", platform),
         platform = ifelse(str_detect(platform, "N[A-FH-Z0-9]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^G[ABC]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "W[A-Za-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "sony" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
                           "other", platform)
         )


write_csv(sales_global, path = "clean_data/sales_global.csv")

# On second thoughts, I think the other regional sales might be interesting/informative to look at too, so I'm going to make a separate
# file for each of them. 


sales_north_america <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names() %>% 
  # img_url and url aren't useful for analysis. last_update is unrelated to the game.
  # vg_chartz_score is only NA values
  # status only has values of 1
  # total_shipped only has NA values. 
  # user_score only has 174 / ~19000 values that are not NA
  # we don't need the pal_sales, na_sales, or global_sales columns any more, so dropping those.
  select(-img_url, 
         -url, 
         -last_update, 
         -vg_chartz_score, 
         -status, 
         -total_shipped, 
         -user_score, 
         -vgchartzscore, 
         -critic_score,
         -global_sales, 
         -pal_sales, 
         -jp_sales,
         -other_sales) %>% 
  # I'm interested in North American sales, so I'm going to drop NA values from that column
  filter(!is.na(na_sales),
         !is.na(year),
         !str_detect(name, "\\(JP sales\\)")) %>% 
  # Changing NA values in the esrb rating column to be 'unrated', to avoid confusion, and to allow for modelling
  mutate(esrb_rating = replace_na(esrb_rating, "unrated")) %>% 
  mutate(platform = ifelse(str_detect(platform, "^PS[A-Za-z0-9]*"), "playstation", platform),
         platform = ifelse(str_detect(platform, "^X[A-Z0-9a-z]+"), "xbox", platform),
         platform = ifelse(str_detect(platform, "Mob"), "mobile", platform),
         platform = ifelse(str_detect(platform, "N[A-FH-Z0-9]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^G[ABC]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "W[A-Za-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "sony" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
                           "other", platform)
  )


write_csv(sales_north_america, path = "clean_data/sales_north_america.csv")




sales_europe <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names() %>% 
  # img_url and url aren't useful for analysis. last_update is unrelated to the game.
  # vg_chartz_score is only NA values
  # status only has values of 1
  # total_shipped only has NA values. 
  # user_score only has 174 / ~19000 values that are not NA
  # we don't need the pal_sales, na_sales, or global_sales columns any more, so dropping those.
  select(-img_url, 
         -url, 
         -last_update, 
         -vg_chartz_score, 
         -status, 
         -total_shipped, 
         -user_score, 
         -vgchartzscore, 
         -critic_score,
         -global_sales, 
         -na_sales, 
         -jp_sales,
         -other_sales) %>% 
  # I'm interested in European sales, so I'm going to drop NA values from that column
  filter(!is.na(pal_sales),
         !is.na(year),
         !str_detect(name, "\\(JP sales\\)")) %>% 
  # Changing NA values in the esrb rating column to be 'unrated', to avoid confusion, and to allow for modelling
  mutate(esrb_rating = replace_na(esrb_rating, "unrated")) %>% 
  mutate(platform = ifelse(str_detect(platform, "^PS[A-Za-z0-9]*"), "playstation", platform),
         platform = ifelse(str_detect(platform, "^X[A-Z0-9a-z]+"), "xbox", platform),
         platform = ifelse(str_detect(platform, "Mob"), "mobile", platform),
         platform = ifelse(str_detect(platform, "N[A-FH-Z0-9]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^G[ABC]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "W[A-Za-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "sony" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
                           "other", platform)
  )


write_csv(sales_europe, path = "clean_data/sales_europe.csv")



sales_japan <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names() %>% 
  # img_url and url aren't useful for analysis. last_update is unrelated to the game.
  # vg_chartz_score is only NA values
  # status only has values of 1
  # total_shipped only has NA values. 
  # user_score only has 174 / ~19000 values that are not NA
  # we don't need the pal_sales, na_sales, or global_sales columns any more, so dropping those.
  select(-img_url, 
         -url, 
         -last_update, 
         -vg_chartz_score, 
         -status, 
         -total_shipped, 
         -user_score, 
         -vgchartzscore, 
         -critic_score, 
         -global_sales, 
         -na_sales, 
         -pal_sales,
         -other_sales) %>% 
  # I'm interested in global sales, so I'm going to drop NA values from that column
  filter(!is.na(year),
         !is.na(jp_sales)) %>% 
  # Changing NA values in the esrb rating column to be 'unrated', to avoid confusion, and to allow for modelling
  mutate(esrb_rating = replace_na(esrb_rating, "unrated")) %>% 
  mutate(platform = ifelse(str_detect(platform, "^PS[A-Za-z0-9]*"), "playstation", platform),
         platform = ifelse(str_detect(platform, "^X[A-Z0-9a-z]+"), "xbox", platform),
         platform = ifelse(str_detect(platform, "Mob"), "mobile", platform),
         platform = ifelse(str_detect(platform, "N[A-FH-Z0-9]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^G[ABC]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "W[A-Za-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "sony" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
                           "other", platform)
  )



write_csv(sales_japan, path = "clean_data/sales_japan.csv")



sales_other <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names() %>% 
  # img_url and url aren't useful for analysis. last_update is unrelated to the game.
  # vg_chartz_score is only NA values
  # status only has values of 1
  # total_shipped only has NA values. 
  # user_score only has 174 / ~19000 values that are not NA
  select(-img_url, 
         -url, 
         -last_update, 
         -vg_chartz_score, 
         -status, 
         -total_shipped, 
         -user_score, 
         -vgchartzscore, 
         -critic_score,
         -pal_sales, 
         -na_sales, 
         -jp_sales,
         -global_sales) %>% 
  # I'm interested in other sales, so I'm going to drop NA values from that column
  filter(!is.na(other_sales),
         !is.na(year),
         !str_detect(name, "\\(JP sales\\)")) %>% 
  # Changing NA values in the esrb rating column to be 'unrated', to avoid confusion, and to allow for modelling
  mutate(esrb_rating = replace_na(esrb_rating, "unrated")) %>% 
  mutate(platform = ifelse(str_detect(platform, "^PS[A-Za-z0-9]*"), "playstation", platform),
         platform = ifelse(str_detect(platform, "^X[A-Z0-9a-z]+"), "xbox", platform),
         platform = ifelse(str_detect(platform, "Mob"), "mobile", platform),
         platform = ifelse(str_detect(platform, "N[A-FH-Z0-9]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^G[ABC]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "W[A-Za-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "sony" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
                           "other", platform)
  )


write_csv(sales_other, path = "clean_data/sales_other.csv")



# Having a look to see whether dropping NA values leaves any data. It leaves some. 



# Checking to see how many NA values each column has
sales_other %>% 
  mutate(rank = sum(is.na(rank)),
         name = sum(is.na(name)),
         basename = sum(is.na(basename)),
         genre = sum(is.na(genre)),
         esrb_rating = sum(is.na(esrb_rating)), 
         platform = sum(is.na(platform)),
         publisher = sum(is.na(publisher)), 
         developer = sum(is.na(developer)),
         #critic_score = sum(is.na(critic_score)),
         #global_sales = sum(is.na(global_sales)),
         #na_sales = sum(is.na(na_sales)),
         #pal_sales = sum(is.na(pal_sales)),
         #jp_sales = sum(is.na(jp_sales)),
         other_sales = sum(is.na(other_sales)),
         year = sum(is.na(year))) %>% 
  head(1)
# So critic_score has by far the most NAs, which is 77% of the sales data. With that many, I think it's worth dropping the column, 
# because I don't want more than 3/4 of the data to be imputed. 

# The next highest proportion of NAs is jp_sales. It has twice as many NA values as na_sales and pal_sales. It also has more than 
# half of its data missing, so I think I'll drop that column too. It wasn't particularly correlated with global_sales either, so I'm 
# not too bothered by that. In addition, I wouldn't use it if I were making a model to predict whether a game is going to be successful
# since you couldn't put any sales in for a theoretical game that's yet to be produced. 

# There's only 5605 missing esrb_ratings. That's not wholly terrible. Plus, since the data is categorical, it's possible to allocate NA
# to it's own category. 

# There are 3 missing values in the developer column. Since there aren't many, and the data is categorical, I'm going to leave them in. 


# With regards to platform, I'm going to make categories. There are four major platforms that games are usually released on. PlayStation, X box 
# Nintendo and PC. I'm going to try to put the data into those categories.


unique(sales_global$platform)
# There are 39 different unique categories. Let's try and figure out what they all are. 

# "PS3"  "PS4"  "PS2"  "X360" "Wii"  "XOne" "PC"   "PSP"  "PS"   "DS"   "NS"   "2600" "GBA"  "NES"  "XB"   "3DS"  "PSN"  "GEN"  "PSV"  "DC"  
# "N64"  "GB"   "SNES" "SAT"  "GBC"  "GC"   "SCD"  "WiiU" "WS"   "VC"   "NG"   "WW"   "PCE"  "XBL"  "3DO"  "GG"   "OSX"  "PCFX" "Mob" 

# I'm going to make 5 categories. Microsoft (for all xbox games), Sony (for anything playstation related), Nintendo (for Nintendos), PC (for any
# pc platforms), mobile (for mobile games), and other, for anything that doesn't fit into those categories (your Atari and your WonderSwan)

