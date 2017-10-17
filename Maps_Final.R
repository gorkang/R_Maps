# With this script, you can draw points in a map semi-automatically given a vector with names, cities and countries


# Load libraries ----------------------------------------------------------
  
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(ggplot2, ggmap, tidyverse, ggrepel, stringr)



# Parameters --------------------------------------------------------------

  draw_universities_names = 0


# Cities vector -----------------------------------------------------------

  # c("NAME_UNIVERSITY, CITY, COUNTRY")
  
    cities_vector = c(
    "Cambridge University, Cambridge, UK",
    "University of Essex, Essex, UK", 
    "Kent University, Kent, UK",
    "Oxford University, Oxford, UK",
    "University of Edinburgh, Edinburgh, UK",
    "Heidelberg University , Heidelberg, Alemania",
    "Amsterdam University, Amsterdam, Holland",
    "Universidad de Barcelona, Barcelona, Spain", 
    "Universidad de La Laguna, Tenerife, Spain",
    "Universidad de Granada, Granada, España",
    "CNRS, Paris, France",
    "Universidad de Bolonia, Bolonia, Italy",
    "Australian Research Council, Sydney, Australia",
    "The University of New South Wales, Sydney, Australia",
    "Sydney University, Sydney, Australia", 
    "York University, Toronto, Canada", 
    "University of Toronto, Toronto, Canada", 
    "Chicago University, Chicago, Illinois",
    "UCSF, San Francisco, California",
    "Caltech, Pasadena, California",
    "Michigan State University, Michigan, USA",
    "Wisconsin University, Wisconsin, USA",
    "Boston College, Massachusetts, USA",
    "INCYT, Buenos Aires, Argentina",
    "INECO, Buenos Aires, Argentina", 
    "Universidad Favaloro, Buenos Aires, Argentina", 
    "PUC, Santiago, Chile",
    "UChile, Santiago, Chile",
    "Universidad de Costa Rica, San Jose, Costa Rica",
    "Universidad Autonoma del Caribe, Cali, Colombia",
    "Universidad de Antioquia, Medellin, Colombia",
    "Universidad ICESI, Cali, Colombia" ,
    "Universidad del Valle, Cali, Colombia",
    "University of Sao Paulo, Sao Paulo, Brasil", 
    "Max Planck, Berlín, Alemania", 
    "DZNE, Berlin, Alemania", 
    "Universidad de Macquarie, Sydney, Australia", 
    "Heriot-Watt University, Edinburgh, UK", 
    "Centro Uruguayo de Imagenología Molecular (CUDIM), Montevideo, Uruguay"
    )

  

# Prepare vector and extract coordinates ----------------------------

  # Separate cities and countries in the cities_vector
  cities = cities_vector %>% as_tibble() %>% cbind(str_split_fixed(cities_vector, ", ", 3)) %>% 
    rename(University_City_Country = value, University = `1`, City = `2`, Country = `3`) %>% 
    mutate(City_Country = paste0(City, ", ", Country))

  # Extract coordinates using cities_countries
  Coordinates = geocode(cities$City_Country)
  
  # Combine cities with coordinates
  Coordinates_cities =  cities %>% cbind(Coordinates)


# Highlighted -------------------------------------------------------------
    
    # Cities to highlight
        # TODO: Use universities instead ####
    Highlighted_List = c("Kent", "Cambridge", "Sydney", "Bologna", "Buenos Aires", "Cali", "Medellin", "Amsterdam")
    
    # Creates the column "Highlighted" inserting the color we want to use to plot their names 
    Coordinates_cities_high = Coordinates_cities %>% 
      mutate(Highlighted = ifelse(City %in% Highlighted_List, "royalblue4", "orange4"))
        # colors()



# Draw map ----------------------------------------------------------------
    
    which_map <- map_data("world")
    
    r_map_plot = ggplot() + geom_polygon(data = which_map, aes(x = long, y = lat, group = group, alpha = 0.8)) + #, fill = "none" 
      coord_fixed(1.3) +
      geom_point(data = Coordinates_cities_high, aes(x = lon, y = lat, color = "none", fill = "none", alpha = 0.8), size = 4, shape = 21) +
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
    
    if (draw_universities_names == 1) {
      
        # We plot Universities, or not
      r_map_plot = r_map_plot + geom_text_repel(data = Coordinates_cities_high, aes(x = lon, y = lat, label = University), 
                        segment.alpha = .5, segment.color = '#cccccc', colour = Coordinates_cities_high$Highlighted, size = 4 ) #hjust = 0.5, vjust = -0.5,  
      
    }

    # Draw map
    r_map_plot
    