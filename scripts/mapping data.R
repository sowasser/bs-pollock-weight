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
library(scales)

#Download data (calculated model outputs, file name="")
setwd("~/Dropbox/Pollock/rdata")
data <- read.csv("calculated_model_outputs_050922.csv")
fit <- readRDS("~/Dropbox/Pollock/model output/model3z_050922_zeros.rds")

#Subset of age and year data
z1 <- subset(data, age==1|age==9)
z1 <- subset(z1, Year==1982|Year==1990|Year==2000|Year==2010|Year==2019)
z1$Abundance <- z1$abundance_area_expanded.x
z1$abundance_area_expanded.x <- NULL
z1$Weight <- z1$weight
z1$weight <- NULL
z1_long <- z1 %>% pivot_longer(cols = c("Abundance", "Weight"), names_to="type")


# Make settings for mapping
settings = make_settings( n_x = 100,
                          Region = "User",
                          purpose = "condition_and_density",
                          bias.correct = FALSE,
                          knot_method = "grid" )

#Set categories
years <- unique(z1$Year)
nyrs <- length(years)

#Settings from model for mapping
settings = make_settings( n_x = 500,
                          Region = "User",
                          purpose = "condition_and_density",
                          bias.correct = FALSE,
                          knot_method = "grid",
                          use_anisotropy=FALSE)
## Remake map list locally for recreating plots
mdl <- make_map_info(Region = settings$Region,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)
## quick dirty AK map
ak_map <- subset(map_data("world"), region=='USA' & subregion=='Alaska')
## Have to duplicate it for each year so can facet below
ak_map <- cbind(ak_map[rep(1:nrow(ak_map), times=nyrs),],
                Year=rep(years, each=nrow(ak_map)))
gmap <- ggplot(ak_map, aes(x = long, y = lat, group = group)) +
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

z1a <- subset(z1, age==1)
z1b <- subset(z1, age==9)


g <- gmap +
  geom_point(data=z1a, aes(Lon, Lat, color=log(Abundance), group=NULL),
             size=1, stroke=0,shape=16) + facet_wrap("Year", ncol=1, strip.position="left")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position="top",
        plot.title=element_text(hjust=0.5),
        legend.key.size=unit(.5,'cm'))+
  labs(color="log(numbers of fish)")+
  xlab("Longitude")+
  ylab("Latitude")+
  guides(colour = guide_colorbar(title.position = "bottom"))+
  ggtitle("Age-1\nAbundance")

g2 <- gmap +
  geom_point(data=z1b, aes(Lon, Lat, color=log(Abundance), group=NULL),
             size=1, stroke=0,shape=16) + facet_wrap("Year", ncol=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        plot.title=element_text(hjust=0.5),
        legend.position="top",
        legend.key.size=unit(.5,'cm'),
        strip.text=element_blank())+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color="log(numbers of fish)")+
  guides(colour = guide_colorbar(title.position = "bottom"))+
  ggtitle("Age-9\nAbundance")

g3 <- gmap +
  geom_point(data=z1a, aes(Lon, Lat, color=Weight, group=NULL),
             size=1, stroke=0,shape=16) + facet_wrap("Year", ncol=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        plot.title=element_text(hjust=0.5),
        legend.position="top",
        legend.key.size=unit(.5,'cm'),
        strip.text=element_blank())+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color="grams")+
  guides(colour = guide_colorbar(title.position = "bottom"))+
  ggtitle("Age-1\nWeight")

g4 <- gmap +
  geom_point(data=z1b, aes(Lon, Lat, color=Weight, group=NULL),
             size=1, stroke=0,shape=16) + facet_wrap("Year", ncol=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        plot.title=element_text(hjust=0.5),
        strip.text=element_blank(),
        legend.position="top",
        legend.key.size=unit(.7,'cm'),)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color="grams")+
  guides(colour = guide_colorbar(title.position = "bottom"))+
  ggtitle("Age-9\nWeight")

g|g3|g2|g4

ggsave("weight_abundance_maps_2_080122.tiff", units="in", width=10, height=10, dpi=600)

