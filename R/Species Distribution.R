library(tidyverse)
library(sp)
library(leaflet)
library(readxl)
library(ggplot2)
library(dplyr)
library(readxl)
library(mapview)
library(sf)
getwd()

# Species Occurrence Distribution Map
excel_file <- "./Data/combined_data.xlsx" 
data <- read_excel(excel_file, sheet = "1. Species Occurrence")
selected_species <- c("E. coli", "Salmonella", "Shigella", "V. cholerae", "V. fluvialis", "V. parahaemolyticus", "V. vulnificus", "Aeromonas")

# Filter data
filtered_data1 <- data %>%
  filter(Species %in% selected_species, 
         !Species %in% c("Unknown", "Unknown (Vibrio)")) %>%
  select(-"Concentration")

# Convert to a regular data frame
filtered_data_df <- fortify(filtered_data1, region = "Species")

# Create an sp object
coordinates(filtered_data1) <- c("Lon", "Lat")

# River Shape Files.
rivers <- sf::read_sf("./Data/Shape_Files/QGIS files/Sampling Sites River Path/Sampling Sites River Path.shp") 
ph_shp <- sf::read_sf("./Data/Shape_Files/QGIS files/Philippine Map/PHL_admPHL_adm3.shp")

# Plot the map
ggplot(data = filtered_data_df) +
  geom_sf(data = ph_shp) +
  geom_sf(data = rivers) +
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE) +
  geom_point(aes(x = Lon, y = Lat, color = Species), size = 2) +
  theme_bw()


ggsave(filename = paste0("./Outputs/", "Species Occurrence.png"), device = "png")

