
#Data loading 


source("C:/Users/19562722/Documents/PHD Research Analysis/Comovement/functions_p3.R")

library(quantmod)
library(dplyr)
library(tidyr)
library(readxl)
library(data.table)
library(ggplot2)
library(here)
library(tidyverse)
library(GGally)
library(xlsx)

library(ggforce)
dpr_all <- read_excel(here("raw data.xlsx"), sheet = "raw data", col_types = c("date",rep("numeric",23)))
dpr_all$Date<-as.Date(dpr_all$Date,format="dd%-mm%-yyyy%")
colName<-c("S&P500","Chicago Wheat","KC Wheat", "Corn","Soybeans","Soybean Meal", "Soybean Oil","Oats","MPLS Wheat","Rough Rice","Coffee","Sugar","Cocoa","Cotton","Orange Juice","Lumber","Live Cattle","Feeder Cattle","Crude Oil", "Heating Oil", "Natural Gas", "Gold","Copper")
colnames(dpr_all)<-c("Date",paste(colName))

dpr_all[,c(3:6,9,10,12,13,15)]<- dpr_all[,c(3:6,9,10,12,13,15)]/100 # converting to $dollar
dpr_all[,-10]<-na.locf(dpr_all[,-10]) #forward fill
dpr_all$`MPLS Wheat`[366:6425]<-na.locf(dpr_all$`MPLS Wheat`[366:6425]) # forward fill for MPLS Wheat for available data range
dpr_all<-as.xts(dpr_all[,-1],order.by=as.Date(dpr_all$Date))
dret_all_p3<-diff(log(dpr_all)) # daily return
dret_all_p3<-dret_all_p3[-1,]


# Plotting daily price, daily return and weekly return series
min_1 <- as.Date("1993-01-05")
max_1 <- as.Date("2019-12-24")

# Daily log-return series
dret_data_plot <- dret_all_p3 %>% as.data.frame() %>% dplyr::mutate(Date=as.Date(index(dret_all_p3))) %>% 
  dplyr::select(everything()) %>%
  gather(key = "series", value = "value",-Date)

dret_data_plot$series <- factor(dret_data_plot$series,levels = unique(dret_data_plot$series))

dret_plot<-dret_data_plot %>% ggplot(aes(x=Date,y=value)) +
  xlab("Date") + ylab("Log-return") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1)) + 
  geom_line(aes(color = series), size = .5,stat = "identity")  +
  theme_minimal() + theme(axis.text.x = element_text(color="black", 
                                                     size=8, angle=20),
                          axis.text.y = element_text(color="black", 
                                                     size=8),legend.position = "none")+
  facet_wrap_paginate(~series, nrow=4, ncol = 3, page=1)


dret_plot2<-dret_data_plot %>% ggplot(aes(x=Date,y=value)) +
  xlab("Date") + ylab("Log-return") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1)) + 
  geom_line(aes(color = series), size = .5,stat = "identity")  +
  theme_minimal() + theme(axis.text.x = element_text(color="black", 
                                                     size=8, angle=20),
                          axis.text.y = element_text(color="black", 
                                                     size=8),legend.position = "none")+
  facet_wrap_paginate(~series, nrow=4, ncol = 3, page=2)



# descriptive statistics

desc.stat.p3.pre<-rbind(descstat.comove(dret_all_p3[1:2773,c(1,19:21,2:5,7,11:14,17:18,22:23)]), descstat.comove(as.data.frame(na.omit(dret_all_p3[366:2773,9]))),descstat.comove(dret_all_p3[1:2773,c(6,8,10,15,16)]))

desc.stat.p3.post<-rbind(descstat.comove(dret_all_p3[2774:6835,c(1,19:21,2:5,7,11:14,17:18,22:23)]), descstat.comove(as.data.frame(na.omit(dret_all_p3[2774:6424,9]))),descstat.comove(dret_all_p3[2774:6835,c(6,8,10,15,16)]))

colname.rowname<-rownames(desc.stat.p3.pre)
colname.rowname.2<-c(colname.rowname,colname.rowname)

desc.stat.p3.all<-cbind(Name=colname.rowname.2,rbind.data.frame(desc.stat.p3.pre,desc.stat.p3.post))


# box-plot

dates<-as.Date(dret_data_plot$Date,"%d-%b-%y")
data_boxplot<- dret_data_plot %>% as.data.frame() %>% dplyr::mutate(Year=factor(format(dates,"%Y"), order=TRUE, levels=unique(c(1993:2019)))) %>% group_by(Year,series) %>% summarise(Mean=mean(na.omit(value)), Min=min(na.omit(value)),Max=max(na.omit(value)),Skewness= moments::skewness(na.omit(value)),Std.Dev.=sd(na.omit(value)),Kurtosis=moments::kurtosis(na.omit(value)))  %>% dplyr::select(Year,Mean,Min,Max,Skewness,Std.Dev.,Kurtosis)
data_boxplot<-na.omit(data_boxplot)


boxplot_mean<- ggplot(data_boxplot,aes(x=Year,y=Mean))+ stat_summary(position = position_dodge(0.5)) 

boxplot_min<- ggplot(data_boxplot,aes(x=Year,y=Min))+ stat_summary(position = position_dodge(0.5))
boxplot_max<- ggplot(data_boxplot,aes(x=Year,y=Max))+ stat_summary(position = position_dodge(0.5))
boxplot_skew<- ggplot(data_boxplot,aes(x=Year,y=Skewness))+ stat_summary(position = position_dodge(0.5))
boxplot_sd<- ggplot(data_boxplot,aes(x=Year,y=Std.Dev.))+ stat_summary(position = position_dodge(0.5))
boxplot_kurt<- ggplot(data_boxplot,aes(x=Year,y=Kurtosis))+ stat_summary(position = position_dodge(0.5))


# Unconditional correlation:


corr_prefin_in_en<-ggcorr(dret_all_p3[1:2773,c(1,19:21)], method = "pairwise", digits= 2,name="Correlation",geom = "tile", low = "#3B9AB2", mid = "#66A61E", high = "#FC4E07", midpoint = 0,label = TRUE, label_round = 2, label_color = "white", limits = c(-1, 1),hjust = 1,layout.exp = 1, legend.position = "bottom") 

corr_prefin_in<-ggcorr(dret_all_p3[1:2773,c(1,2:5,7,11:14,17:18,22:23)], method = "pairwise", digits= 2,name="Correlation",geom = "tile", low = "#3B9AB2", mid = "#66A61E", high = "#FC4E07", midpoint = 0,label = TRUE, label_round = 2, label_color = "white", limits = c(-1, 1),hjust = .83,layout.exp = 1, legend.position = "bottom")

corr_prefin_off<-ggcorr(dret_all_p3[1:2773,c(1,6,8:10,15,16)], method = "pairwise", digits= 2,name="Correlation",geom = "tile", low = "#3B9AB2", mid = "#66A61E", high = "#FC4E07", midpoint = 0,label = TRUE, label_round = 2, label_color = "white", limits = c(-1, 1),hjust = .80,layout.exp = 1, legend.position = "bottom")

corr_fin_in_en<-ggcorr(as.matrix(dret_all_p3[2774:6835,c(1,19:21)]),  method = "pairwise", digits= 2,name="Correlation",geom = "tile", low = "#3B9AB2", mid = "#66A61E", high = "#FC4E07", midpoint = 0,label = TRUE, label_round = 2, label_color = "white", limits = c(-1, 1),hjust = 1,layout.exp = 1, legend.position = "bottom")

corr_fin_in<-ggcorr(as.matrix(dret_all_p3[2774:6835,c(1,2:5,7,11:14,17:18,22:23)]),  method = "pairwise", digits= 2,name="Correlation",geom = "tile", low = "#3B9AB2", mid = "#66A61E", high = "#FC4E07", midpoint = 0,label = TRUE, label_round = 2, label_color = "white", limits = c(-1, 1),hjust = .83,layout.exp = 1, legend.position = "bottom")

corr_fin_off<-ggcorr(as.matrix(dret_all_p3[2774:6835,c(1,6,8:10,15,16)]),  method = "pairwise", digits= 2,name="Correlation",geom = "tile", low = "#3B9AB2", mid = "#66A61E", high = "#FC4E07", midpoint = 0,label = TRUE, label_round = 2, label_color = "white", limits = c(-1, 1),hjust = .80,layout.exp = 1, legend.position = "bottom") 

