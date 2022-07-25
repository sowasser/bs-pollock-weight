##Run model
# Load packages
install.packages("devtools")
library(devtools)

# Install package
# Install TMB from CRAN
install.packages("TMB")
# Install INLA using currently recommended method
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# Install FishStatsUtils from CRAN
install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
install_github("james-thorson/VAST")

# Load package
library(TMB)
library(VAST)

install_version("Matrix", version = "1.3.3", repos = "http://cran.us.r-project.org")
library(Matrix)
install.packages("TMB")
library(TMB)
install.packages("VAST")
library(VAST)
library(ggplot2)
library(dplyr)

# load data set
setwd("~/Dropbox/Pollock/for_github/formatted_data/final")
example <- readRDS("data_combined.rds")

#Upload extrapolation grid file
user_region <- readRDS("bs_grid.rds")

#Limit to before 2019 if wanted (ie exclude 2021 abundance data)
example <- subset(example, year<2020)
# Format data
b_i = ifelse( !is.na(example$age_cpue_sum),
              example$age_cpue_sum,
              example$weight_combined)
data_type = ifelse( !is.na(example$age_cpue_sum), 0, 1)
c_i = case_when(example$age_bin ==1 & data_type== 0 ~ 0,
                example$age_bin ==2 & data_type== 0 ~ 1,
                example$age_bin ==3 & data_type== 0 ~ 2,
                example$age_bin ==4 & data_type== 0 ~3,
                example$age_bin ==5 & data_type== 0 ~ 4,
                example$age_bin ==6 & data_type== 0 ~ 5,
                example$age_bin ==7 & data_type== 0 ~ 6,
                example$age_bin ==8 & data_type== 0 ~ 7,
                example$age_bin ==9 & data_type== 0 ~ 8,
                example$age_bin ==10 & data_type== 0 ~9,
                example$age_bin ==11 & data_type== 0 ~ 10,
                example$age_bin ==12 & data_type== 0 ~ 11,
                example$age_bin ==13 & data_type== 0 ~ 12,
                example$age_bin ==14 & data_type== 0 ~ 13,
                example$age_bin ==15 & data_type== 0 ~ 14,
                example$age_bin ==1 & data_type== 1 ~ 15,
                example$age_bin ==2 & data_type== 1 ~ 16,
                example$age_bin ==3 & data_type== 1 ~ 17,
                example$age_bin ==4 & data_type== 1 ~ 18,
                example$age_bin ==5 & data_type== 1 ~ 19,
                example$age_bin ==6 & data_type== 1 ~ 20,
                example$age_bin ==7 & data_type== 1 ~ 21,
                example$age_bin ==8 & data_type== 1 ~ 22,
                example$age_bin ==9 & data_type== 1 ~ 23,
                example$age_bin ==10 & data_type== 1 ~ 24,
                example$age_bin ==11 & data_type== 1 ~ 25,
                example$age_bin ==12 & data_type== 1 ~ 26,
                example$age_bin ==13 & data_type== 1 ~ 27,
                example$age_bin ==14 & data_type== 1 ~ 28,
                example$age_bin ==15& data_type== 1 ~ 29)

# Make settings
settings = make_settings( n_x = 500,
                          Region = "User",
                          purpose = "condition_and_density",
                          bias.correct = FALSE,
                          knot_method = "grid",
                          use_anisotropy=FALSE)
settings$FieldConfig = c(Omega1 = "IID", Epsilon1 = "IID", Omega2 = 0, Epsilon2 = 0)
#settings$FieldConfig[c("Omega"),"Component_1"] = 0
settings$RhoConfig["Epsilon1"]=4
#settings$RhoConfig["Beta1"]=4
Expansion_cz = matrix(nrow=30, ncol=2)
Expansion_cz[1:15,1:2]=0
Expansion_cz[16:30, 1]=2
Expansion_cz[16:30,2]=c(0:14)

ObsModel =matrix(nrow=30, ncol=2)
ObsModel[1:15,1]=2
ObsModel[1:15,2]=4
ObsModel[16:30,1]=1
ObsModel[16:30,2]=4

settings$ObsModel = ObsModel
settings$Options['treat_nonencounter_as_zero'] = FALSE

# Run model (abundance)
fit3z = fit_model( settings = settings,
                lower=-Inf, upper=Inf,
                test_fit=FALSE, 
                newtonsteps=1, 
                getsd=T,
                 Lat_i = example$start_latitude,
                 Lon_i = example$start_longitude,
                 t_i = example$year,
                 c_i = c_i,
                 b_i = b_i,
                 a_i = rep(1, nrow(example)),
                 input_grid = user_region,
                 Expansion_cz = Expansion_cz,
                Use_REML = TRUE, 
                build_model=T,
                run_model=F)

#Save model fit
saveRDS(fit3z, file="model3z_050922_zeros.rds")

#Diagnostics
fit <- fit3z
check_fit <- check_fit(fit$parameter_estimates)
summary_list<-summary(fit)

#Save model outputs
plots <- plot(fit)
density_dataframe <- as.data.frame(summary(fit)$Density_dataframe)
saveRDS(density_dataframe, file="density_dataframe_3l_012722.rds")

table_for_ss3 <- plots$Index$Table
write.csv(table_for_ss3, "Table_for_ss3_3l.csv")
