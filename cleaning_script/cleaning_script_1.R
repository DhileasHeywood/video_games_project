library(janitor)
library(tidyverse)



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
         platform = ifelse(str_detect(platform, "W[A-RT-Za-rt-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "playstation" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
                           "other", platform),
         publisher = str_to_lower(publisher),
         publisher_ranked = publisher %in% ranked_publishers
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
         platform = ifelse(str_detect(platform, "W[A-RT-Za-rt-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "playstation" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
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
         platform = ifelse(str_detect(platform, "W[A-RT-Za-rt-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "playstation" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
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
         platform = ifelse(str_detect(platform, "W[A-RT-Za-rt-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^VC"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "playstation" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
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
         platform = ifelse(str_detect(platform, "W[A-RT-Za-rt-z]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "3D[OS]+"), "nintendo", platform),
         platform = ifelse(str_detect(platform, "^PC[A-Z]*"), "pc", platform), 
         platform = ifelse(str_detect(platform, "OSX"), "pc", platform),
         platform = ifelse(platform != "playstation" & platform != "xbox" & platform != "mobile" & platform != "nintendo" & platform != "pc", 
                           "other", platform),
         publisher = str_to_lower(publisher)
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


# Joining newly formed graphs together

sales_regional <- left_join(sales_north_america, 
                            sales_europe, 
                            by = c("name", "basename", "genre", "esrb_rating", "platform", "publisher", "developer", "year", "rank")) %>%
  left_join(sales_japan, by = c("name", "basename", "genre", "esrb_rating", "platform", "publisher", "developer", "year", "rank")) %>% 
  left_join(sales_other, by = c("name", "basename", "genre", "esrb_rating", "platform", "publisher", "developer", "year", "rank")) %>% 
  pivot_longer(cols = c("na_sales", "pal_sales", "jp_sales", "other_sales"), names_to = "region", values_to = "sales") %>% 
  separate(col = region, into = c("region", "rubbish"), sep = "_") %>% 
  select(-rubbish) %>% 
  mutate(region = ifelse(region == "pal", "europe", region),
         region = ifelse(region == "na", "north america", region),
         region = ifelse(region == "jp", "japan", region)) %>% 
  drop_na() %>% 
  mutate(publisher = str_to_lower(publisher),
         developer = str_to_lower(developer),
         publisher_ranked = publisher %in% ranked_publishers)

write_csv(sales_regional, path = "clean_data/sales_all_regions.csv")


# In order to do analysis of games publishers, I need to find a source of ranking them. Part of the data supplied contains ratings from 
# Metacritic, who also publish a list of top games publishers every year. I'm going to take data from their 2019 list for 2018 releases, 
# abd their 2017 list for 2016 releases. (There's no list for 2017 releases.)

# source: https://www.metacritic.com/feature/game-publisher-rankings-for-2018-releases

ranked_publishers <- c("capcom", 
                       "sega", 
                       "electronic arts", 
                       "ea",
                       "nintendo", 
                       "ubisoft", 
                       "sony", 
                       "square enix", 
                       "bandai namco entertainment",
                       "digerati distribution", 
                       "nis america", 
                       "plug in digital", 
                       "focus home interactive", 
                       "thq nordic",
                       "activision blizzard", 
                       "paradox interactive",
                       "505 games", 
                       "take-two interactive", 
                       "aksys games", 
                       "bethseda softworks",
                       "microsoft", 
                       "devolver digital",
                       "team17",
                       "arc system works", 
                       "warner bros. interact",
                       "zen studios", 
                       "curve digital",
                       "koel tecmo games",
                       "adult swim",
                       "konami",
                       "microids",
                       "milestone s.r.l", 
                       "1c entertainment", 
                       "headup games", 
                       "tinybuild", 
                       "good shepherd ent.", 
                       "flyhigh works", 
                       "wales interactive", 
                       "qubicgames", 
                       "xseed/marvelous", 
                       "ratalalka games", 
                       "2k", 
                       "rockstar", 
                       "xseed games",
                       "marvelous usa",
                       "telltale games",
                       "warner bros. interactive",
                       "degica",
                       "daedalic entertainment",
                       "gambitious digital ent.",
                       "artifex mundi",
                       "10tons entertainment",
                       "idea factory international",
                       "deep silver")
