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

#Download data (calculated model outputs, file name="")
setwd("~/Dropbox/Pollock/rdata")
data <- read.csv("calculated_model_outputs_050922.csv")

#Clean up formatting of data
data$awwaa <- NULL
data$local_abundance <- data$abundance_area_expanded.x
data$local_weight <- data$weight
data$abundance_area_expanded.x <- NULL
data$weight <- NULL
data$local_abundance <- drop_units(data$local_abundance)
data$age <- as.factor(data$age)
data$Grid <- as.factor(data$Grid)

###Calculate means and totals
##Calculate mean abundance per grid cell across years
data <- data %>%
  group_by(Grid, age) %>%
  mutate(mean_abundance=mean(local_abundance))%>% ungroup()

##Calculate mean weight per grid cell across years
data <- data %>%
  group_by(Grid, age) %>%
  mutate(mean_weight=mean(local_weight, na.rm=T))%>% ungroup()

##Calculate total abundances per year
data <- data %>%
  group_by(age, Year) %>%
  mutate(total_abundance=sum(local_abundance))%>% ungroup()

data <- data %>%
  group_by(age, Year) %>%
  mutate(total_mean_abundance=sum(mean_abundance))%>% ungroup()

###Calculate weighted weights
##Local abundance, local weight
data <- data %>%
  mutate(LL=(local_weight * (local_abundance/total_abundance)))%>% ungroup()

##Local abundance, mean weight
data <- data %>%
  mutate(LM=(mean_weight * (local_abundance/total_abundance)))%>% ungroup()

##Mean abundance, local weight
data <- data %>%
  mutate(ML=(local_weight * (mean_abundance/total_mean_abundance)))%>% ungroup()

write.csv(data, "local_abundance_weight_attribution_grid.csv")

#Calculate annual matrix
matrix1 <- aggregate(LL~age+Year, data=data, FUN=sum, na.action=na.omit)
matrix2 <- aggregate(ML~age+Year, data=data, FUN=sum, na.action=na.omit)
matrix3 <- aggregate(LM~age+Year, data=data, FUN=sum, na.action=na.omit)
#matrix4 <- aggregate(MM~age+Year, data=data, FUN=sum, na.action=na.omit)
matrix  <-  matrix1 %>% 
  full_join(matrix2, by=c("age", "Year")) %>% 
  full_join(matrix3, by=c("age", "Year")) #%>% 
#full_join(matrix4, by=c("age", "Year"))

write.csv(matrix, "local_abundance_weight_attribution_index.csv")
