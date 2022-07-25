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

# load specimen and cpue raw data
setwd("~/Dropbox/Pollock/rdata")

CPUE <- read.csv("cpue_final_050422.csv")
weight_obs <- as.data.frame(read.csv("specimen_data_edited_streamlined.csv"))

#Average observed weights from raw data
#Remove weird 0 ages
weight <- subset(weight_obs, weight_obs$age_bin>0)
#Combine weight_calculated and weight observed
weight$weight_combined <-ifelse( !is.na(weight$weight), weight$weight, weight$weight_calculated)

#Limit to before 2019 if wanted (ie exclude 2021 abundance data)
weight<- subset(weight, year<2020)

#Calculate mean weight per haul for each age class
avg_weight_haul <- aggregate(weight_combined~year+age_bin+hauljoin, data=weight, FUN=mean)

#Merge average weight per haul with total of age per year and only keep hauls with a weight
CPUE2 <- CPUE %>% 
  right_join(avg_weight_haul, by = c("hauljoin", "year", "age_bin"))

#CPUE per haul, weighted by total CPUE for age and year (for hauls with a weight)
#Total CPUE per age and year
total_count_age_year <- aggregate(age_cpue_sum~year+age_bin, data=CPUE2, FUN=sum)
#Merge total CPUE per age and year to average weight haul data
CPUE2<- CPUE2 %>% 
  left_join(total_count_age_year, by = c("year", "age_bin"))
#Change column name to total annual CPUE
colnames(CPUE2)[colnames(CPUE2) == "age_cpue_sum.y"] <- "cpue_annual_total"
#Calculate weighted CPUE
CPUE2$cpue_weighted <- CPUE2$age_cpue_sum.x/CPUE2$cpue_annual_total
#Calculate abundance-weighted weights from raw data
CPUE2$weighted_weight <- CPUE2$cpue_weighted * CPUE2$weight_combined
#Sum abundance-weighted weights
weighted_average <- aggregate(weighted_weight~year+age_bin, data=CPUE2, FUN=sum)

#Change column names and clean up data
weighted_average$age <- weighted_average$age_bin
weighted_average$age_bin <- NULL
weighted_average$metric <- paste("weight")
weighted_average$type <- paste("weighted average")
weighted_average$data <- weighted_average$weighted_weight
weighted_average$weighted_weight <- NULL
weighted_average$weight_weighted <- NULL
weighted_average$age <- as.factor(weighted_average$age)

#Make csv for giving to Jim for stock assessment
nonspatial_estimate <- weighted_average
nonspatial_estimate$metric <- NULL
nonspatial_estimate$type <- NULL
nonspatial_estimate$weight <- nonspatial_estimate$data
nonspatial_estimate$weight <- nonspatial_estimate$weight/1000
nonspatial_estimate$data <- NULL
nonspatial_estimate <- nonspatial_estimate %>% spread(key="age", value="weight")

saveRDS(weighted_average, "weighted_average.rds")
saveRDS(nonspatial_estimate, "nonspatial_estimate.rds")
