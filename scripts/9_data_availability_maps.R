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

#Upload extrapolation grid file
user_region <- readRDS("bs_grid.rds")

#Model output
fit <- readRDS("~/Dropbox/Pollock/model output/model3z_050922_zeros.rds")

#Map of weight/length/age data availability by year
## quick dirty AK map
ak_map <- subset(map_data("world"), region=='USA' & subregion=='Alaska')

# Make settings
settings = make_settings( n_x = 500,
                          Region = "User",
                          purpose = "condition_and_density",
                          bias.correct = FALSE,
                          knot_method = "grid" )
settings$FieldConfig[c("Omega","Epsilon"),"Component_1"] = "IID"
settings$FieldConfig[c("Omega"),"Component_1"] = 0
settings$RhoConfig["Epsilon1"]=4
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

## Have to duplicate it for each year so can facet below
mdl <- make_map_info(Region = settings$Region,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)
hauls <- CPUE %>% distinct(hauljoin, .keep_all = TRUE)
nyrs <- length(unique(hauls$year))
years <- 1982:2020
ak_map <- cbind(ak_map[rep(1:nrow(ak_map), times=nyrs),],
                Year=rep(years, each=nrow(ak_map)))

gmap <- ggplot(ak_map, aes(x = long, y = lat, group=group)) +
  geom_polygon(fill="black", colour = "white") +
  scale_color_viridis_c(option = "magma") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  coord_cartesian(xlim=mdl$Xlim, ylim=mdl$Ylim)

#Map all years
hauls <- subset(hauls, year<2020)
g <- gmap +
  geom_point(data=hauls, aes(start_longitude, start_latitude,group=NULL), size=0.5)+
  facet_wrap('year')+
  theme( text = element_text(size=14))+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")
g

ggsave("haul_locations.tiff", units="in", width=15, height=8.5, dpi=600)

#Subset just a few years for Figure
hauls_sub<- subset(hauls_sub, year==1983|year==2000|year==2019)
g2 <- gmap +
  geom_point(data=hauls_sub, aes(start_longitude, start_latitude,group=NULL), size=0.5)+
  facet_wrap('year')+
  theme( text = element_text(size=14))+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")
g2

ggsave("haul_locations_subset.tiff", units="in", width=8.5, height=4.25, dpi=600)
