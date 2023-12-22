# for disease occurrence
library(leaflet)
library(dplyr)
library(readxl)
# Read data
excel_file <- "~/Desktop/ENM/Species Occurrence Model/Data/combined_data.xlsx" 
data3 <- read_excel(excel_file, sheet = "3. Disease Occurrence")

# Convert date to Date type
data3$'Date of Admission' <- as.Date(data3$`Date of Admission`, format = "%m/%d/%Y")

# Specify age groups and their colors
age_groups <- c("0-20", "21-40", "41-60", "61-80", "81-100")
age_colors <- c("blue", "green", "yellow", "orange", "red")

# Create a factor variable for Age with specified levels
data3$Age_factor <- cut(data3$Age, breaks = c(0, 20, 40, 60, 80, 100), labels = age_groups, include.lowest = TRUE)

# Specify the directory to save the HTML files
output_directory <- "~/Desktop/ENM/Species Occurrence Model/Maps/Disease Occurrence"

# ... (previous code)

# Print individual maps according to age groups and save as HTML files
for (age_group in age_groups) {
  filtered_data <- subset(data3, Age_factor == age_group)
  
  # Create a leaflet map
  map <- leaflet(filtered_data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~Lon,
      lat = ~Lat,
      popup = ~paste("Site: ", Site, "<br>Date of Admission: ", `Date of Admission`, "<br>Sex: ", Sex, "<br>Age: ", Age),
      label = ~Site,
      color = ~ifelse(any(Sex == "M"), "blue", "pink"),
      radius = 5
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("blue", "pink"),
      labels = c("Male", "Female"),
      title = "Sex"
    )
  
  # Add a custom legend for age groups
  map <- map %>% addControl(
    position = "topright",
    html = paste(
      "<strong>Age Group</strong><br>",
      sapply(unique(data3$Age_factor), function(group) {
        paste0(
          '<i style="background:', age_colors[which(age_groups == group)], '"></i>',
          group, '<br>'
        )
      })
    )
  )
  
  # Save the map as an HTML file
  saveWidget(map, file.path(output_directory, paste("map_", age_group, ".html", sep = "")))
  
  # Display the map
  print(map)
}


rm(data3, age_colors, age_group, age_groups, output_directory, map)





