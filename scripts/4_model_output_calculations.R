library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(raster)
library(TMB)
library(VAST)
install.packages("ggrepel")
library(ggrepel)
install.packages("patchwork")
library(patchwork)

#Download density dataframe and 
setwd("~/Dropbox/Pollock/rdata")
density_dataframe <- read.csv("density_dataframe_050922.csv")
table_for_ss3 <- read.csv("table_for_ss3_050922.csv")

#Extract category numbers
density_dataframe$Category2 <- str_extract(density_dataframe$Category, "\\d+")
density_dataframe$Category2 <- as.numeric(density_dataframe$Category2)

#Dataframe of just abundance
abundance <- as.data.frame(subset(density_dataframe, Category2<16))

#Rename Density column with abundance
colnames(abundance)[colnames(abundance) == "Density"] <- "abundance"

#Calculate area-expanded abundance
#Convert abundance to per km instead of per hectare
abundance$abundance_km <- abundance$abundance*0.01
abundance$abundance_area_expanded <- abundance$abundance_km*abundance$Area_km2

#Make age column
abundance$age<- abundance$Category2

#Dataframe of just weights
weights <- as.data.frame(subset(density_dataframe, Category2>15))

#Make age column
weights$Category2 <- (weights$Category2-15)

#Make age column
weights$age<- weights$Category2

#Rename Density column with weight
colnames(weights)[colnames(weights) == "Density"] <- "weight"

#Merge abundance and weight-at-age and clean up dataframe
complete <- merge(abundance, weights, by=c("Grid","Year", "age")) 
complete <- complete[, c("Grid", "Year", "age", "Area_km2.x", "Lon.x", "Lat.x", "abundance_km", "abundance_area_expanded", "weight")]
colnames(complete)[colnames(complete) == "Area_km2.x"] <- "Area_km2"
colnames(complete)[colnames(complete) == "Lat.x"] <- "Lat"
colnames(complete)[colnames(complete) == "Lon.x"] <- "Lon"
colnames(complete)[colnames(complete) == "abundance_area_expanded.x"] <- "abundance_area_expanded"

#Area-expanded abundance per year per age class
abundance_total <- aggregate(abundance_area_expanded~Year+age, data=abundance, FUN=sum)

#Make column of total abundance per age class per year
complete <- complete %>% 
  left_join(abundance_total, by = c("Year", "age"))

colnames(complete)[colnames(complete) == "abundance_area_expanded.y"] <- "abundance_annual_total"

##Calculate grid-level, abundance-weighted weight-at-age
#Calculate per grid weighted weight
complete$awwaa <-  complete$weight * (complete$abundance_area_expanded/complete$abundance_annual_total)

#Calculate model index weight-at-age
awwaa_matrix <- aggregate(awwaa~Year+age, data=complete, FUN=sum)
awwaa_matrix$awwaa_kg <- awwaa_matrix$awwaa*0.001

#Flip to wide and convert to kg for Ianelli for stock assessment
#spatial_model <- read.csv("awwaa_matrix_031822.csv")
spatial_model <- awwaa_matrix
spatial_model$X <- NULL
spatial_model$awwaa <- NULL
spatial_model <- spatial_model %>% spread(key="age", value="awwaa_kg")

#Model index weight-at-age from stock assessment (adjust formatting for comparison)
model <- table_for_ss3
model$Category <- as.numeric(model$Category)
model <- subset(model, Category>15)
model$type <- paste0("model")
model$Units <- NULL
model$age <- model$Category
model$age <- model$age-15
model$Stratum <- NULL
model$year <- model$Time
model$Time <- NULL
model$data <- model$Estimate
model$Estimate <- NULL
model$SE <- model$Std..Error.for.Estimate
model$X <- NULL
model$Std..Error.for.Estimate<- NULL
model$Std..Error.for.ln.Estimate. <- NULL
model$Category <- NULL

annual_abundance <- table_for_ss3
annual_abundance$Category <- as.numeric(annual_abundance$Category)
annual_abundance <- subset(annual_abundance, Category<16)
annual_abundance$Units <- NULL
annual_abundance$age <- annual_abundance$Category
annual_abundance$Stratum <- NULL
annual_abundance$year <- annual_abundance$Time
annual_abundance$Time <- NULL
annual_abundance$X <- NULL
annual_abundance$Category <- NULL
annual_abundance$SE <- annual_abundance$Std..Error.for.Estimate
annual_abundance$abundance <- annual_abundance$Estimate
annual_abundance$Estimate <- NULL
annual_abundance$Std..Error.for.Estimate <- NULL
annual_abundance$Std..Error.for.ln.Estimate. <- NULL

##Summary statistics of index weight-at-age (annual percent changes)
#Calculate percent change each year
matrix <- matrix %>% group_by(age) %>% 
  mutate(pct_change = ((awwaa/lag(awwaa) - 1) * 100))

#Calculate percent change from 1982 to 2019
matrix_subset <- subset(matrix, Year==1982|Year==2019)
matrix_subset <- matrix_subset %>% group_by(age) %>% 
  mutate(pct_change = ((awwaa/lag(awwaa) - 1) * 100))

#Save as csv's
saveRDS(complete, "calculated_model_outputs_050922.rds")
saveRDS(awwaa_matrix, "awwaa_matrix_050922.rds")
saveRDS(spatial_model, "spatial_model_050922.rds")
saveRDS(model, "model_index_weight.rds")
saveRDS(annual_abundance,"model_index_abundance.rds")
