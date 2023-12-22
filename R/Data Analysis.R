######### Step-wise Regression Models (Forward, Backward, Both) *may only choose one but all models does not show significance*

# Assuming 'Site' is the common identifier in both data frames
merged_data <- merge(filtered_data_df, data2, by = "Site")

# Pivot the data2 data frame to have parameters as columns
pivoted_data2 <- data2 %>%
  pivot_wider(names_from = Parameter, values_from = Value)

# Merge the pivoted data2 with the presence-absence data
merged_data <- merge(filtered_data_df, pivoted_data2, by = "Site")

# Fit logistic regression model
logistic_model <- glm(Presence ~ Temperature + `Oxidation-Reaction Potential` + pH + DOP + DO + EC + TDS + SAL + `BGA-PC` + Altitude + Barometer + Depth, 
                      data = merged_data, 
                      family = binomial)
summary(logistic_model)

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

###### PRESENCE MODEL DISREGARD FIRST

target_species <- c("Aeromonas", "E. coli", "Salmonella", "Shigella", "V. cholerae", "V. fluvialis", "V. parahaemolyticus", "V. vulnificus")

# Create a new column 'Presence' where 1 indicates presence and 0 indicates absence for any of the target species
filtered_data_df$Presence <- ifelse(filtered_data_df$Species %in% target_species, 1, 0)

# View the modified data frame.
print(filtered_data_df)
