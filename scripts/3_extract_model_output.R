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

#Load model output file 
fit <- readRDS("~/Dropbox/Pollock/model output/model3z_050922_zeros.rds")

#Plots
plots <- plot(fit)

#Index table
table_for_ss3 <- plots$Index$Table

#Density dataframe
density_dataframe <- as.data.frame(summary(fit)$Density_dataframe)

#Save as csv
write.csv(density_dataframe, "density_dataframe_050922.csv")
write.csv(table_for_ss3, "table_for_ss3_050922.csv")