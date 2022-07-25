library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(raster)
library(TMB)
library(VAST)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("patchwork")
library(patchwork)

#Load data (index weight-at-age w/ spatial and spatio-temporal attribution, file name=="")
setwd("~/Dropbox/Pollock/rdata")
matrix <- readRDS("index_weight_variation.rds")

#Convert to long format
matrix_long <- matrix%>% pivot_longer(cols=3:7, names_to="type")

#Graph all types
ggplot(data=matrix_long, aes(x=year, y=value, group=type))+
  geom_line(aes(group=type, color=type))+  
  scale_colour_manual(labels=c("Full", "No Beta1", "No Beta2", "No Epsilon", "No Omega"), values=c("#CC79A7", "orange", "#009E73", "#E69F00", "#56B4E9"))+ylab("Weight (g)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+facet_wrap("age", scales="free")                                                                                                                                                                                                                                                                                                                                                                      

#Subset matrix to just omega (spatial variation) and epsilon (spatio-temporal variation)
matrix_long_sub <- subset(matrix_long, matrix_long$type=="D_no_omega2"|matrix_long$type=="D_no_epsilon2"|matrix_long$type=="D_full2")

#Graph
#Remove zero estimates and make NA
matrix_long_sub$value <- ifelse(matrix_long_sub$value<0.01, NA, matrix_long_sub$value)

#plot
ggplot(data=matrix_long_sub, aes(x=year, y=value, group=type))+
  geom_line(aes(group=type, linetype=type))+  
  scale_linetype_manual(labels=c("Full Model", "No Spatiotemporal Variation", "No Spatial Variation"),values=c("solid","dotted", "longdash"))+
  ylab("Weight-at-age (g)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position = "top",
        strip.text.y = element_text(angle=0),
        strip.background =element_blank(),)+
  #axis.text.y=element_blank(),  #Remove y axis
  #axis.ticks.y=element_blank())+ #Remove y axis
  guides(linetype=guide_legend(title=""), color=FALSE)+
  scale_x_continuous(limits = c(1982, 2019), expand = c(0, 0), breaks=c(1982, 1990, 2000, 2010, 2019))+
  facet_grid("age", scales="free")                                                                                                                                                                                                                                                                                                                                                                      

ggsave("omega_epsilon_2.tiff", units="in", width=8.5, height=15, dpi=600)

#Plot just age 1 and age 9
matrix_long_sub2 <- subset(matrix_long_sub, age==1|age==9)
ggplot(data=matrix_long_sub2, aes(x=year, y=value, group=type))+
  geom_line(aes(group=type, linetype=type))+  
  scale_linetype_manual(labels=c("Full Model", "No Spatiotemporal Variation", "No Spatial Variation"),values=c("solid","dotted", "longdash"))+
  ylab("Weight-at-age (g)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position = "top",
        strip.text.y = element_text(angle=0),
        strip.background =element_blank())+
        #axis.text.y=element_blank(),  #Remove y axis
        #axis.ticks.y=element_blank())+ #Remove y axis
  guides(linetype=guide_legend(title=""), color=FALSE)+
  scale_x_continuous(limits = c(1982, 2019), expand = c(0, 0), breaks=c(1982, 1990, 2000, 2010, 2019))+
  facet_grid("age", scales="free")                                                                                                                                                                                                                                                                                                                                                                      

ggsave("omega_epsilon_2_sub.tiff", units="in", width=8, height=8, dpi=600)

##Calculate percent difference between reduced and full model

#Remove zero estimates and make NA
matrix$D_full2 <- ifelse(matrix$D_full2<0.01, NA, matrix$D_full2)
matrix$D_no_omega2 <- ifelse(matrix$D_no_omega2<0.01, NA, matrix$D_no_omega2)
matrix$D_no_epsilon2<- ifelse(matrix$D_no_epsilon2<0.01, NA, matrix$D_no_epsilon2)
matrix$D_no_beta12 <- ifelse(matrix$D_no_beta12<0.01, NA, matrix$D_no_beta12)
matrix$D_no_beta22 <- ifelse(matrix$D_no_beta22<0.01, NA, matrix$D_no_beta22)

#Calculate percent difference
matrix$perc_diff_no_omega <- (matrix$D_no_omega2-matrix$D_full2)/matrix$D_full2
matrix$perc_diff_no_epsilon<- (matrix$D_no_epsilon2-matrix$D_full2)/matrix$D_full2
matrix$perc_diff_no_beta2 <- (matrix$D_no_beta22-matrix$D_full2)/matrix$D_full2
matrix$perc_diff_no_beta1 <- (matrix$D_no_beta12-matrix$D_full2)/matrix$D_full2

#Calculate absolute value of percent differences, and summarize
matrix$perc_diff_no_omega2 <- abs(matrix$perc_diff_no_omega)
omega <- aggregate(perc_diff_no_omega2~age, matrix, FUN=mean)
omega$perc_diff_no_omega2 <- omega$perc_diff_no_omega2*100

omega_min <- aggregate(perc_diff_no_omega2~age, matrix, FUN=min)
omega_max <- aggregate(perc_diff_no_omega2~age, matrix, FUN=max)

matrix$perc_diff_no_epsilon2 <- abs(matrix$perc_diff_no_epsilon)
epsilon <- aggregate(perc_diff_no_epsilon2~age, matrix, FUN=mean)
epsilon$perc_diff_no_epsilon2 <- epsilon$perc_diff_no_epsilon2*100

omega_mean <- mean(matrix$perc_diff_no_omega2)
omega_sd <- sd(matrix$perc_diff_no_omega2)

epsilon_mean <- mean(matrix$perc_diff_no_epsilon2)
epsilon_sd <- sd(matrix$perc_diff_no_epsilon2)

#Graph percent differences
matrix_long2 <- matrix%>% pivot_longer(cols=8:11, names_to="type")
matrix_long2$value <- matrix_long2$value*100
ggplot(matrix_long2, aes(x=year, y=value))+geom_col(aes(fill=type, group=type))+facet_wrap("age", scales="free")+ylab("Percent Difference from Full Model")

##Calculation of Proportion of Variance Explained
D_full2= tapply(matrix$D_full2, INDEX=list(matrix$age,matrix$year), FUN=mean)
D_no_omega2 = tapply(matrix$D_no_omega2, INDEX=list(matrix$age,matrix$year), FUN=mean)
D_no_epsilon2 = tapply(matrix$D_no_epsilon2, INDEX=list(matrix$age,matrix$year), FUN=mean)
D_no_beta12 = tapply(matrix$D_no_beta12, INDEX=list(matrix$age,matrix$year), FUN=mean)
D_no_beta22 = tapply(matrix$D_no_beta22, INDEX=list(matrix$age,matrix$year), FUN=mean)

var_full = apply(D_full2, MARGIN=1, FUN=var, na.rm=TRUE)
resid_2 = apply(D_full2-D_no_omega2, MARGIN=1, FUN=var, na.rm=TRUE)
resid_3 = apply(D_full2-D_no_epsilon2, MARGIN=1, FUN=var, na.rm=TRUE)
resid_4 = apply(D_full2-D_no_beta12, MARGIN=1, FUN=var, na.rm=TRUE)
resid_5 = apply(D_full2-D_no_beta22, MARGIN=1, FUN=var, na.rm=TRUE)

PVE_2 = 1 - resid_2/var_full
PVE_3 = 1 - resid_3/var_full
PVE_4 = 1 - resid_4/var_full
PVE_5 = 1 - resid_5/var_full

#Make dataframe of PVE
PVE <- as.data.frame(PVE_2)
PVE$age <- 1:15
PVE$PVE_3 <- PVE_3
PVE$PVE_4 <- PVE_4
PVE$PVE_5 <- PVE_5

#Convert to long for graphing
PVE_long <- PVE %>% pivot_longer(cols = c("PVE_2", "PVE_3", "PVE_4", "PVE_5"), names_to="model")

#Subset to just spatial and spatio-temporal variation
PVE_long2 <- subset(PVE_long, PVE_long$model=="PVE_2"|PVE_long$model=="PVE_3")

#Graph (bar plot)
ggplot(PVE_long2, aes(x=age, y=value, group=model, fill=model))+
  geom_col(position="dodge")+
  ylab("Proportion of Variance Explained")+
  xlab("Age Class")+
  scale_fill_manual(values=c("black","grey"),labels=c("Spatial Variation", "Spatio-temporal Variation"))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_hline(yintercept=1, linetype="dashed")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=18),
        legend.position = c(0.8, .25))+
  annotate(geom="text", x=1.01, y=1.01, label="100% Variance Explained",
           color="black")+
  annotate(geom="text", x=1.01, y=-0.01, label="0% Variance Explained",
           color="black")+
  
  ggsave("PVE_omeg_eps_bar_051222.tiff", units="in", width=20, height=8.5, dpi=600)

#Graph (line plot)
ggplot(PVE_long2, aes(x=age, y=value, group=model, color=model))+
  geom_line()+
  ylab("Proportion of Variance Explained")+
  xlab("Age Class")+
  scale_color_manual(values=c("black","grey"),labels=c("Spatial Variation", "Spatio-temporal Variation"))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_hline(yintercept=1, linetype="dashed")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=18),
        legend.position = c(0.8, .25))+
  annotate(geom="text", x=1.3, y=1.01, label="100% Variance Explained",
           color="black")+
  annotate(geom="text", x=1.3, y=-0.01, label="0% Variance Explained",
           color="black")

ggsave("PVE_omeg_eps_line_051222.tiff", units="in", width=20, height=8.5, dpi=600)