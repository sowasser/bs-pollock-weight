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

setwd("~/Dropbox/Pollock/rdata")
matrix <- read.csv("local_abundance_weight_attribution_index.csv")

#Convert matrix to long format for graphing
matrix_long <- matrix%>% pivot_longer(cols = c("LL", "ML", "LM"), names_to="type")
#Make year numeric for graphing
matrix_long$Year <- as.numeric(matrix_long$Year)
#Graph annual totals
#remove mean

ggplot(data=matrix_long, aes(x=Year, y=value, group=type)) +
  geom_line(aes(group=type, linetype=type))+
  #scale_linetype_manual(labels=c("Local abundance & local weight", "Local abundance and mean weight", "Mean abundance and local weight", "Mean abundance and mean weight"), 
  #values=c("#CC79A7", "#56B4E9", "#009E73", "#E69F00"))+
  ylab("Weight-at-age (g)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size=13),
        legend.position = "top",
        strip.text.y = element_text(angle=0),
        strip.background =element_blank())+
        #axis.text.y=element_blank(),  #Remove y axis
       # axis.ticks.y=element_blank())+ #Remove y axis
  guides(linetype=guide_legend(title=""))+
  scale_linetype_manual(values=c("solid", "dotted", "longdash"),labels=c("Full Model", "Local Abundance & Mean Size", "Mean Abundance & Local Size"))+
  scale_x_continuous(limits = c(1982, 2019), expand = c(0, 0), breaks=c(1982, 1990, 2000, 2010, 2019))+
  facet_grid("age", scales="free")

ggsave("weight_abundance_attribution_2_y_axis.tiff", units="in", width=8.5, height=15, dpi=600)

#Subset of ages
matrix_long_sub <- subset(matrix_long, age==1|age==9)

ggplot(data=matrix_long_sub, aes(x=Year, y=value, group=type)) +
  geom_line(aes(group=type, linetype=type))+
  #scale_linetype_manual(labels=c("Local abundance & local weight", "Local abundance and mean weight", "Mean abundance and local weight", "Mean abundance and mean weight"), 
  #values=c("#CC79A7", "#56B4E9", "#009E73", "#E69F00"))+
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
  guides(linetype=guide_legend(title=""))+
  scale_linetype_manual(values=c("solid", "dotted", "longdash"),labels=c("Full Model", "Local Abundance & Mean Size", "Mean Abundance & Local Size"))+
  scale_x_continuous(limits = c(1982, 2019), expand = c(0, 0), breaks=c(1982, 1990, 2000, 2010, 2019))+
  facet_grid("age", scales="free")

ggsave("weight_abundance_attribution_2_sub.tiff", units="in", width=8, height=8, dpi=600)



#Percent difference compared to model
matrix <- matrix %>%
  mutate(ML_diff=((ML-LL)/LL))

matrix<- matrix %>%
  mutate(LM_diff=((LM-LL)/LL))

#Summarize percent differences
matrix$LM_diff2 <- abs(matrix$LM_diff)
matrix$ML_diff2 <- abs(matrix$ML_diff)
LM_diff<- aggregate(LM_diff2~age, matrix, FUN=mean)
ML_diff <- aggregate(ML_diff2~age,matrix, FUN=mean)
LM_diff$LM_diff2 <- LM_diff$LM_diff2*100
ML_diff$ML_diff2 <- ML_diff$ML_diff2*100

#Plot percent difference
matrix_long_diff <- matrix %>% pivot_longer(cols = c("ML_diff", "LM_diff"), names_to="type")
matrix_long_diff$value <- matrix_long_diff$value * 100
matrix_long_diff$Year <- as.numeric(matrix_long_diff$Year)+1981

ggplot(data=matrix_long_diff, aes(x=Year, y=value, group=type)) +
  geom_col(aes(group=type, fill=type))+
  scale_fill_manual(labels=c("Local abundance & mean weight", "Mean abundance & local weight"), values=c("black", "grey"))+
  ylab("Percent Change")+
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   text = element_text(size=15),
                   legend.position = c(0.9, 0.1))+
  guides(fill=guide_legend(title=""))+
  facet_wrap("age", scales="free")

ggsave("weight_abundance_attribution_perc_change_050922.tiff", units="in", width=15, height=8.5, dpi=600)

#Calculation of PVE
matrix <- load("matrix_all_models.Rdata")
W1_ct = tapply(matrix$LL, INDEX=list(matrix$age,matrix$Year), FUN=mean)
W2_ct = tapply(matrix$ML, INDEX=list(matrix$age,matrix$Year), FUN=mean)
W3_ct = tapply(matrix$LM, INDEX=list(matrix$age,matrix$Year), FUN=mean)

var_full = apply(W1_ct, MARGIN=1, FUN=var, na.rm=TRUE)
resid_2 = apply(W1_ct-W2_ct, MARGIN=1, FUN=var, na.rm=TRUE)
resid_3 = apply(W1_ct-W3_ct, MARGIN=1, FUN=var, na.rm=TRUE)

PVE_2 = 1 - resid_2/var_full
PVE_3 = 1 - resid_3/var_full

PVE <- as.data.frame(PVE_2)
PVE$age <- 1:15
PVE$PVE_3 <- PVE_3
PVE_long <- PVE %>% pivot_longer(cols = c("PVE_2", "PVE_3"), names_to="model")

ggplot(PVE_long, aes(x=age, y=value, group=model, color=model))+
  geom_line()+
  ylab("Proportion of Variance Explained")+
  xlab("Age Class")+
  scale_color_manual(values=c("black","grey"),labels=c("Local weight", "Local abundance"))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_hline(yintercept=1, linetype="dashed")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=18),
        legend.position = c(0.9, 0.1))+
  annotate(geom="text", x=1.3, y=1.02, label="100% Variance Explained",
           color="black")+
  annotate(geom="text", x=1.3, y=-0.02, label="0% Variance Explained",
           color="black")

ggsave("PVE_abund_weight_line_051222.tiff", units="in", width=20, height=8.5, dpi=600)