# Load required libs
#install.packages(c("leaflet", "leaflet.extras", "readxl", "dplyr", "tidyr", "gtools", "htmlwidgets"), dependencies = TRUE)
library(readxl)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(tidyr)
library(gtools)
library(htmlwidgets)
options(dplyr.width=Inf)

# read data
excel_file <- "~/Desktop/ENM/Species Occurrence Model/scratch2/scratch2/combined_data.xlsx"
data <- read_excel(excel_file, sheet = "1. Species Occurrence")
head(data)

# filtering data with selected species
sel_species <- c("E. coli", "Salmonella", "Shigella", "V. cholerae", "V. fluvialis", "V.parahamolyticus", "V.vulnificus", "Aeromonas")

fd <- data |>
  filter(Species %in% sel_species, !Species %in% c("Unknown", "Unknown (Vibrio)")) |> select(-"Concentration")

unique_species <- unique(fd$Species)


# color paette for each specie
species_palette = c(
    "E. coli" = "blue",
    "Salmonella" = "green",
    "Shigella" = "orange",
    "V. cholerae" = "purple",
    "V. fluvialis" = "brown",
    "V.parahamolyticus" = "red",
    "V.vulnificus" = "pink",
    "Aeromonas" = "yellow"
  )

cf_palette <- colorFactor(
  species_palette
  ,domain = unique_species
)

filtered_palette <- species_palette[unique_species]

# Create a leaflet map
map <- leaflet() |>
  addTiles() |>
  addCircleMarkers(
    data = fd,
    lng = ~Lon,
    lat = ~Lat,
    color = ~cf_palette(Species),
    radius = 5,
    fillOpacity = 0.7,
    popup = ~paste("Species: ", Species)
  ) |>
  addLegend(
    position = "bottomright",
    colors = filtered_palette,
    labels = unique_species,
    title = "Species"
  )

# Print the map
print(map)

# Extract dataframe from sheet 2
data2 <- read_excel(excel_file, sheet = "2. Environmental Factors")
parameters <- unique(data2$Parameter)

# reshape data, turning each parameter into a column.
data2_rs <- data2 |>
  group_by(Site, Lon, Lat, Parameter) |>
  slice(1) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  arrange(mixedsort(Site))

# we expect to have 14 rows in our reshaped data, one for each site.
head(data2_rs, n=10)

# locate sites w/ missing values
rows_with_missing <- data2_rs |> filter(rowSums(across(everything(), is.na)) > 0)
print(rows_with_missing)

# replace missing data with 0 for mapping purposes
data2_rs[is.na(data2_rs)] <- 0

# confirm that there are no missing values
rows_with_missing <- data2_rs |> filter(rowSums(across(everything(), is.na)) > 0)
print(rows_with_missing)

# function to generate map given the dataframe and specific parameter 
gen_parameter_map <- function(dataframe, param_name) {
  
  # Select relevant columns
  selected_cols <- c("Site", "Lon", "Lat", "Date", param_name)
  f_data <- dataframe |> select(all_of(selected_cols))
  
  # Create color scale
  color_scale <- colorNumeric(palette = "viridis", domain = f_data |> pull(param_name))
  
  # Generate leaflet map
  map <- leaflet(data = f_data) |>
    addTiles() |>
    addCircleMarkers(
      lng = ~Lon,
      lat = ~Lat,
      radius = 5,
      fillOpacity = 0.8,
      fillColor = ~color_scale(f_data[[param_name]]),
      color = "white",
      stroke = TRUE,
      weight = 1,
      popup = ~paste("Site: ", Site, "<br>Date: ", Date, "<br>", param_name, ": ", f_data[[param_name]]),
      label = ~as.character(param_name)
    ) |>
    addLegend(pal = color_scale, values = ~f_data[[param_name]], title = param_name)
  
  return(map)
}


# Example usage
temp_map <- gen_parameter_map(data2_rs, "Temperature")
print(temp_map)

# loop through each parameter and generate the map


# loop through each parameter and generate the map
for (param in parameters) {
  param_map <- gen_parameter_map(data2_rs, param)
  print(param_map)
  
  # save map as an HTML file
  map_file_name <- paste0(param, "_map.html")
  saveWidget(param_map, file = map_file_name, selfcontained = TRUE)
  
  # optional: save map as a PNG file (requires webshot2 package)
  # install.packages("webshot2")
  # webshot2::install_chromium()
  # library(webshot2)
  # webshot(url = map_file_name, file = paste0(param, "_map.png"))
}


