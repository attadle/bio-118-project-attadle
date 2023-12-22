######### Step-wise Regression Models (Forward, Backward, Both) *may only choose one but all models does not show significance*

merged_data <- merge(filtered_data_df, data2, by = "Site")

# Pivot the data2 data frame to have parameters as columns
pivoted_data2 <- data2 %>%
  pivot_wider(names_from = Parameter, values_from = Value)

# Merge the pivoted data2 with the presence-absence data
merged_data <- merge(filtered_data_df, pivoted_data2, by = "Site")
head(merged_data)
# Fit logistic regression model
logistic_model <- glm(Presence ~ Temperature + `Oxidation-Reaction Potential` + pH + DOP + DO + EC + TDS + SAL + `BGA-PC` + Altitude + Barometer + Depth, 
                      data = merged_data, 
                      family = binomial)
summary(logistic_model)

# Forward Stepwise Regression
model_forward <- step(logistic_model, direction = "forward")
summary(model_forward)

# Backward Stepwise Regression
model_backward <- step(logistic_model, direction = "backward")

# Bidirectional Stepwise Regression
model_both <- step(logistic_model, direction = "both")

# Display the results
summary(model_forward)
summary(model_backward)
summary(model_both)

####### correlation heatmap
# Extract relevant columns for correlation analysis
cor_data <- merged_data[, c("Species", "Temperature", "Oxidation-Reaction Potential", "pH", "DOP", "DO", "EC", "TDS", "SAL", "BGA-PC", "Altitude", "Barometer", "Depth")]

# 1. Convert Species column to a factor
cor_data$Species <- as.factor(cor_data$Species)

# Calculate correlation matrix
cormat <- cor(cor_data[, -1], method = "pearson")

# Print the correlation matrix
print(cormat)

# 2. create correlation heatmap
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormatrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# 3. upper and lower triangles
# Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# 4. Finished correlation matrix heatmaps
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# 5. Reorder correlation matrix
reorder_cormat <- function(cormat){
# Use correlation between variables as distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

# Add correlation coefficients
ggheatmap <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = sprintf("%.2f", value)), color = "black", size = 4) +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background color to white
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# 6. Extract lon.x and lat.x for correlation analysis
lon_lat_data <- merged_data[, c("Lon.x", "Lat.x")]

# 7. Calculate correlation between lon.x, lat.x, and other variables
lon_lat_cor <- cor(lon_lat_data, cor_data[, -1], method = "pearson")

# 8. Melt the correlation matrix
melted_lon_lat_cor <- melt(lon_lat_cor)
head(melted_lon_lat_cor)

# 9. Add lon.x and lat.x correlation to the existing heatmap
ggheatmap <- ggheatmap + 
  geom_tile(data = melted_lon_lat_cor, aes(Var2, Var1, fill = value), color = "black") +
  geom_text(data = melted_lon_lat_cor, aes(Var2, Var1, label = sprintf("%.2f", value)),
            color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# 10. Print the updated heatmap
print(ggheatmap)

ggsave("~/Desktop/bio 118-class/exercises/bio 118 project/bio-118-project-attadle/Outputs/heatmap.png", width = 10, height = 8)


