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

#Load model, non-spatial estimator, and stock assessment weight-at-age matrices
weighted_average <- readRDS("weighted_average.rds")
stock <- read_excel("~/Dropbox/Pollock/data/ianelli_awwaa.xlsx", 
                      sheet = "Sheet2")
model <- readRDS("model_index_weight.rds")

###Combine and graph
weighted_average$age <- as.numeric(weighted_average$age)
model$year <- as.numeric(model$year)
comparison <- bind_rows(model, weighted_average)

#Remove zeroes and make NA
comparison$data <- ifelse(comparison$data<0.01, NA, comparison$data)

##Graph model vs naive estimator (and vs. stock assessment)

#With confidence interval shading from Thorson
ggplot(comparison, aes(x=year, y=data, group=type))+geom_ribbon(aes(ymin=data-SE, ymax=data+SE), fill = "grey")+
  geom_line(aes(linetype=type))+
  facet_grid(vars(age), scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position = "top",
        strip.text.y = element_text(angle=0),
        strip.background =element_blank())+
  guides(linetype=guide_legend(title=""), labels=c("Nonspatial Weighted Average", "Model"))+
  scale_x_continuous(limits = c(1982, 2019), expand = c(0, 0), breaks=c(1982, 1990, 2000, 2010, 2019))+
  ylab("Weight-at-age (g)")+
  xlab("Year")

ggsave("model_average_comparison_2.tiff", units="in", width=8.5, height=15, dpi=600)

#Version w/ no y-axis
ggplot(comparison, aes(x=year, y=data, group=type))+geom_ribbon(aes(ymin=data-SE, ymax=data+SE), fill = "grey")+
  geom_line(aes(linetype=type))+
  facet_grid(vars(age), scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position = "top",
        strip.text.y = element_text(angle=0),
        strip.background =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_linetype_manual(values=c("dotted", "solid"),labels=c("Nonspatial Estimator", "Spatio-temporal Model"))+
  scale_x_continuous(limits = c(1982, 2019), expand = c(0, 0), breaks=c(1982, 1990, 2000, 2010, 2019))+
ylab("Weight-at-age (g)")+
  xlab("Year")

ggsave("model_average_comparison_no_y.tiff", units="in", width=8.5, height=15, dpi=600)

#Subset just a couple age classes
comparison_sub <- subset(comparison, age==1|age==9)

ggplot(comparison_sub, aes(x=year, y=data, group=type))+geom_ribbon(aes(ymin=data-SE, ymax=data+SE), fill = "grey")+
  geom_line(aes(linetype=type))+
  facet_grid(vars(age), scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position = "top",
        legend.title=element_blank(),
        strip.text.y = element_text(angle=0),
        strip.background =element_blank())+
  scale_linetype_manual(values=c("dotted", "solid"),labels=c("Nonspatial Estimator", "Spatio-temporal Model"))+
  scale_x_continuous(limits = c(1982, 2019), expand = c(0, 0), breaks=c(1982, 1990, 2000, 2010, 2019))+
  ylab("Weight-at-age (g)")+
  xlab("Year")

ggsave("model_average_comparison_no_y.tiff", units="in", width=8, height=8, dpi=600)




#####Stock assessment 
#Stock assessment
ianelli_2 <- gather(ianelli, age, weight, X3:X15)

#Extract age number
ianelli_2$age <- str_extract(ianelli_2$age, "\\d+")
ianelli_2$age2 <- as.numeric(ianelli_2$age)
ianelli_2$age <- NULL
ianelli_2$age <- ianelli_2$age2
ianelli_2$age2 <- NULL

ianelli_2$type <- paste("stock")
ianelli_2$metric <- paste("weight")
ianelli_2$data <- ianelli_2$weight
ianelli_2$weight <- NULL
ianelli_2$data <- ianelli_2$data*1000
ianelli_2$year <- ianelli_2$Year
ianelli_2$Year <- NULL

#Add stock assessment and graph comparison
comparison2 <- bind_rows(comparison, ianelli_2)

ggplot(comparison2, aes(x=year, y=data, group=type))+geom_ribbon(aes(ymin=data-SE, ymax=data+SE), fill = "grey")+
  geom_line(aes(linetype=type))+
  facet_wrap(vars(age), scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position = c(0.9, 0.1))+
  guides(linetype=guide_legend(title=""))+
  scale_linetype_manual(values=c("solid", "dotted", "longdash"))+
  ylab("Weight-at-age (g)")+
  xlab("Year")

ggsave("model_average_stock_comparison_050922.tiff", units="in", width=15, height=8.5, dpi=600)
