ground_test <- read_csv("ground_test_2_rformat.csv", 
                        col_types = cols(`GMT Time` = col_datetime(format = "%m/%d/%Y %H:%M"))) %>% 
  rename(ID = id,
         time = `GMT Time`,
         x = Longitude,
         y = Latitude)

ground_test %>% 
  filter(ID == 215688) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  mapview::mapview()
