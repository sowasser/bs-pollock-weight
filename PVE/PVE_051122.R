library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(raster)
library(VAST)

data <- readRDS("data_for_PVE.rds")

###Attribution of local weight vs local abundance
##Calculate means and totals
#Calculate mean abundance per grid cell across years for each age class
data <- data %>%
  group_by(Grid, age) %>%
  mutate(mean_abundance=mean(local_abundance))%>% ungroup()

#Calculate mean weight per grid cell across years for each age class
data <- data %>%
  group_by(Grid, age) %>%
  mutate(mean_weight=mean(local_weight, na.rm=T))%>% ungroup()

#Calculate total abundances per year for each age class
data <- data %>%
  group_by(age, Year) %>%
  mutate(total_abundance=sum(local_abundance))%>% ungroup()

#Calculate total mean abundance per year for each age class (for abundance-weighting the mean abundance)
data <- data %>%
  group_by(age, Year) %>%
  mutate(total_mean_abundance=sum(mean_abundance))%>% ungroup()

##Calculate abundance-weighted weights
#Local abundance, local weight
data <- data %>%
  mutate(LL=(local_weight * (local_abundance/total_abundance)))%>% ungroup()

#Local abundance, mean weight
data <- data %>%
  mutate(LM=(mean_weight * (local_abundance/total_abundance)))%>% ungroup()

#Mean abundance, local weight
data <- data %>%
  mutate(ML=(local_weight * (mean_abundance/total_mean_abundance)))%>% ungroup()

#Sum annual abundance-weighted weights
matrix1 <- aggregate(LL~age+Year, data=data, FUN=sum, na.action=na.omit)
matrix2 <- aggregate(ML~age+Year, data=data, FUN=sum, na.action=na.omit)
matrix3 <- aggregate(LM~age+Year, data=data, FUN=sum, na.action=na.omit)

#Merge together
matrix  <-  matrix1 %>% 
  full_join(matrix2, by=c("age", "Year")) %>% 
  full_join(matrix3, by=c("age", "Year")) #%>% 

###Calculate proportion of variance: Thorson equations
#Mean of models per year and category
matrix<- matrix %>%
  group_by(age) %>%
  mutate(mean_LL=mean(LL, na.rm=T)) %>% ungroup()

matrix<- matrix %>%
  group_by(age) %>%
  mutate(mean_LM=mean(LM, na.rm=T)) %>% ungroup()

matrix<- matrix %>%
  group_by(age) %>%
  mutate(mean_ML=mean(ML, na.rm=T)) %>% ungroup()

#Calculation of full variance
matrix$var_full <- (matrix$LL-matrix$mean_LL)^2

#Mean of full variance
matrix<- matrix %>%
  group_by(age) %>%
  mutate(mean_full_var=mean(var_full)) %>% ungroup()

#Calculation of variances of reduced models
matrix$var_LM <- (matrix$LM-matrix$mean_LM)^2
matrix$var_ML <-(matrix$ML-matrix$mean_ML)^2

#Mean of variances of reduced models
var_LM <- aggregate(var_LM~age, data=matrix, FUN=mean, na.action=na.omit)
var_ML <- aggregate(var_ML~age, data=matrix, FUN=mean, na.action=na.omit)
var_full <- aggregate(var_full~age, data=matrix, FUN=mean, na.action=na.omit)

#Merge together
variance  <-  var_full %>% 
  full_join(var_ML, by=c("age")) %>% 
  full_join(var_LM, by=c("age"))

#Calculation of PVE
variance$PVE_LM <- (1-(variance$var_LM/variance$var_full))
variance$PVE_ML <- (1-(variance$var_ML/variance$var_full))

#Convert for graphing
variance$age <- as.numeric(variance$age)
variance_long <- variance %>% pivot_longer(cols = c("PVE_LM", "PVE_ML"), names_to="model")

ggplot(variance_long, aes(x=age, y=value, group=model, color=model))+geom_line()+ylab("Proportion of Variance Explained")+xlab("Age Class")+scale_color_discrete(labels=c("Local abundance & mean weight", "Mean abundance and local weight"))+ylim(-0.5,1)
