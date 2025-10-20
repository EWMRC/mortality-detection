library(tidyverse)
library(sf)
library(move)
library(momentuHMM)
library(furrr)

ground_test <- read_csv("ground_test_2_rformat.csv", 
                        col_types = cols(`GMT Time` = col_datetime(format = "%m/%d/%Y %H:%M"))) %>% 
  rename(ID = id,
         time = `GMT Time`,
         x = Longitude,
         y = Latitude)

ground_test_sf <- ground_test %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

centroids <- ground_test %>% 
  group_by(ID) %>% 
  summarise(x = mean(x), y = mean(y)) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

ground_test$centroid_dist <- ground_test %>% 
  pull(ID) %>% 
  unique() %>% 
  map(.f = function(id){
    iter_centroid <- centroids %>% 
      filter(ID == id)
    
    iter_sf <- ground_test_sf %>% 
      filter(ID == id)
    
    st_distance(iter_centroid, iter_sf) %>% #meters
      as.numeric()
  }) %>% 
  unlist()

# Thresholds by ID
ground_test %>% 
  group_by(ID) %>% 
  summarise(habitat = unique(habitat),
            threshold_9 = quantile(centroid_dist, probs = 0.9), 
            threshold_75 = quantile(centroid_dist, probs = 0.75),
            threshold_5 = quantile(centroid_dist, probs = 0.5)) %>% # 
  View()

# Thresholds by habitat
ground_test %>%
  group_by(habitat) %>% 
  summarise(threshold_9 = quantile(centroid_dist, probs = 0.9), 
            threshold_75 = quantile(centroid_dist, probs = 0.75),
            threshold_5 = quantile(centroid_dist, probs = 0.5)) %>% # 
  View()

# 50% thresholds:
# Young forest/Aspen 4.931429
# Forest 10.854253
# Short grass 5.332848
# Tall grass 3.933362

# calibration info
ground_test %>% 
  filter(ID == 215688) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  mapview::mapview()

amwo_hmm %>% 
  group_by(test) %>% 
  summarise(mn_step = mean(step, na.rm = TRUE),
            sd_step = sd(step, na.rm = TRUE),
            mn_angle = mean(angle, na.rm = TRUE),
            sd_angle = sd(angle, na.rm = TRUE),
            mn_dist = mean(mean_dist_5),
            sd_dist = sd(mean_dist_5))
