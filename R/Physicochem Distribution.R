# for physiochem.
library(leaflet)
library(mapview)
library(htmlwidgets)
library(readxl)
library(ggplot2)
library(dplyr)
getwd()

# Read data
excel_file <- "./Data/combined_data.xlsx" 
data2 <- read_excel(excel_file, sheet = "2. Environmental Factors")
data2$Parameter <- as.character(data2$Parameter)

# Temperature
temp <- dplyr::filter(data2, Parameter == "Temperature")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .005, width = .005)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

ggsave(filename = paste0("./Outputs/", "Temperature.png"), device = "png")

# Oxidation-Reaction Potential
OR <- dplyr::filter(data2, Parameter == "Oxidation-Reaction Potential")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# pH
pH <- dplyr::filter(data2, Parameter == "pH")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# DO(mg/L)
DO <- dplyr::filter(data2, Parameter == "DO")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# EC
EC <- dplyr::filter(data2, Parameter == "EC")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# TDS
TDS <- dplyr::filter(data2, Parameter == "TDS")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# Sal
sal <- dplyr::filter(data2, Parameter == "SAL")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# BGA-PC
BGA-PC <- dplyr::filter(data2, Parameter == "BGA-PC")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# Altitude
alt <- dplyr::filter(data2, Parameter == "Altitude")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# Barometer
baro <- dplyr::filter(data2, Parameter == "Barometer")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()

# Depth
depth <- dplyr::filter(data2, Parameter == "Depth")
ggplot(data = filtered_data1)+
  geom_sf(data = ph_shp)+
  geom_sf(data = rivers)+
  geom_tile(data = temp, aes(x = Lon, y = Lat, fill = Value), height = .01, width = .01)+
  coord_sf(xlim = c(123.85, 124), ylim = c(10.33, 10.45), expand = FALSE)+
  #geom_point(aes(x=Lon, y=Lat, color= Species), size= 2)+
  theme_bw()