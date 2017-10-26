# With this script, you can draw points in a map semi-automatically given a vector with names, cities and countries

# Load libraries ----------------------------------------------------------
  
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(ggplot2, ggmap, tidyverse, ggrepel, stringr, gsheet, magrittr)

  

# Cities vector -----------------------------------------------------------

  # Read Google doc https://docs.google.com/spreadsheets/d/1z6nIPLtDs26GnrdKz_WwJnuN5aw8Vi3CGLra0L-HUGk/edit?pli=1#gid=0
  cities = 
    gsheet2tbl("1z6nIPLtDs26GnrdKz_WwJnuN5aw8Vi3CGLra0L-HUGk/gid=0") %>% 
    select(Institution, City, Country) %>% 
    rename(University = Institution) %>% 
    mutate(City_Country = paste0(City, ", ", Country)) %>%
    distinct(City_Country, .keep_all = T)
  
  
# Coordinate vector -------------------------------------------------------
  
  # Read file stored in the last run of the script
  if (file.exists("data/coordinates_cities.csv") == TRUE) {
    
      coordinates_cities_db = read_csv("data/coordinates_cities.csv") %>% 
        distinct(City_Country, .keep_all = T)
  
  } else {
  
      # If this is the first run, create a dummy DF
      coordinates_cities_db = c("") %>% as_tibble() %>% rename(City = value) %>% mutate(lon = NA)
    
  }

  
  # Join cities with known coordinates
  cities_final = cities %>% left_join(coordinates_cities_db)
  
  # Cities without coordinates
  cities_final_to_check = cities_final %>% filter(is.na(lon)) %>% select(University, City, Country, City_Country)
  
  
  
# Prepare vector and extract coordinates ----------------------------

  # Google API key (If needed) - only with dev version of ggmap
  # source("functions/google_api_key_private.R")
  # register_google(key = google_api_key)
  
  if (nrow(cities_final_to_check) > 0) {
      # Extract coordinates using cities_countries
      Coordinates = geocode(cities_final_to_check$City_Country)
      
      # Combine cities with coordinates
      Coordinates_cities =  cities_final_to_check %>% cbind(Coordinates)
      # geocodeQueryCheck()
  }
  
  # 
  Coordinates_cities = cities_final %>% mutate(lon = ifelse(is.na(lon), Coordinates_cities$lon, lon),
                                               lat = ifelse(is.na(lat), Coordinates_cities$lat, lat))

  
# Highlighted -------------------------------------------------------------
    
    # Cities to highlight
    Highlighted_List = c("Ashland")
    
    # Creates the column "Highlighted" inserting the color we want to use to plot their names 
    Coordinates_cities_high = Coordinates_cities %>% 
      mutate(Highlighted = ifelse(City %in% Highlighted_List, "royalblue4", "orange4"))
        # colors()



# Draw map ----------------------------------------------------------------
    
    which_map <- map_data("world")
    
    r_map_plot = ggplot() + geom_polygon(data = which_map, aes(x = long, y = lat, group = group, alpha = 0.8)) + #, fill = "none" 
      coord_fixed(1.3) +
      geom_point(data = Coordinates_cities_high, aes(x = lon, y = lat, color = "none", fill = "none", alpha = 0.8), size = 3, shape = 21) +
      guides(fill = FALSE, alpha = FALSE, size = FALSE, color = F) +
      scale_fill_manual(values = c("orange3")) + 
      scale_colour_manual(values = c("white"))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
      theme(axis.line = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(), legend.position = "none",
            # panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor = element_blank(), plot.background = element_blank())
    
    
      # Map with universities names
      r_map_plot_names = r_map_plot + geom_text_repel(data = Coordinates_cities_high, aes(x = lon, y = lat, label = University), 
                        segment.alpha = .5, segment.color = '#cccccc', colour = Coordinates_cities_high$Highlighted, size = 2 ) #hjust = 0.5, vjust = -0.5,  
      
    

    # Draw map
    # r_map_plot
    
    # Save maps
    ggsave("output_figures/The Psychological Science Accelerator Map.svg", r_map_plot, width = 17, height = 10)
    ggsave("output_figures/The Psychological Science Accelerator Map - names.svg", r_map_plot_names, width = 17, height = 10)
    
    