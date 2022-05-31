
industry_data<- read.csv("industry.csv")
industry_data$ï..date <- strptime(as.character(industry_data$ï..date),format="%Y-%m-%d %H:%M:%S")
industry_data$ï..date <- as.POSIXct(industry_data$ï..date,tz = "UTC")
library(rpart)              # CART algorithm for decision trees
library(partykit)           # Plotting trees
library(gbm)                  # Boosting algorithms
library(doParallel)     # parallel processing
library(pROC)                 # plot the ROC curve
library(corrplot)  
library(psych)
library(Hmisc)
library(lubridate)
industry_data$Day_of_week <-as.factor(industry_data$Day_of_week)
industry_data$WeekStatus <- as.factor(industry_data$WeekStatus)
industry_data$Load_Type <- as.factor(industry_data$Load_Type)
str(industry_data)

require(gridExtra)
usageplot1 <-qplot(industry_data$ï..date,industry_data$Usage.kWh.,xlab='Time',ylab='Usage kWh',geom="line")+theme_grey(base_size = 18) 
usageplot1
usageplot2 <-qplot(industry_data$ï..date[674:1345],industry_data$Usage.kWh.[674:1345],xlab='Time (1 week)',ylab='Usage kWh',geom="line")+theme_grey(base_size = 18) 
usageplot2
png('Usage_profile_3_Jan1.png',width = 14, height = 10, units = 'in', res = 300)
grid.arrange(usageplot1, usageplot2, nrow=2)
dev.off()

# Plotting a boxplot
png('Industry_usage_histogram_boxplot2_Jan1.png',width = 14, height = 10, units = 'in', res = 300)
par(mfrow=c(2,1))
hist(industry_data$Usage.kWh.,main="",xlab = "Usage kWh",breaks = 40,
     col='lightblue',xlim=c(0,200),ylim=c(0,20000),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
boxplot(industry_data$Usage.kWh.,
        boxfill = "lightblue",horizontal=TRUE,ylim=c(0,200),xlab="Usage kWh",frame=F,
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

# HEAT MAP visualization
# Visualization of the Energy use per week with heat map

industry_data$my <- floor_date(industry_data$ï..date,"month")
industry_data$mhr <- floor_date(industry_data$ï..date,"hour")

library(plyr)
industry_data_Total_per_hour <-  ddply(industry_data, "mhr", summarise,
                                       Usage.kWh.=sum(Usage.kWh.))
industry_data_Total_per_hour
industry_data_Total_per_hour$Day_week <- wday(industry_data_Total_per_hour$mhr,label=TRUE)
industry_data_Total_per_hour_na_removed <- na.omit(industry_data_Total_per_hour)
industry_data_Total_per_hour_na_removed$week_year <- week(industry_data_Total_per_hour_na_removed$mhr)
# first week only

industry_data_Total_per_hour_na_removed_w1 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==1,]
industry_data_Total_per_hour_na_removed_w1
industry_data_Total_per_hour_na_removed_w1$Hour <- hour(industry_data_Total_per_hour_na_removed_w1$mhr)
library(ggplot2)
library(lubridate)
# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg1 <-ggplot(industry_data_Total_per_hour_na_removed_w1,aes(x=Day_week,y=Hour,
                                                          fill=Usage.kWh.)) 
gg1

min(industry_data_Total_per_hour_na_removed_w1$Usage.kWh.)
# 13.47
max(industry_data_Total_per_hour_na_removed_w1$Usage.kWh.)
# 528.8
gg1 <- gg1 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(13,550))
gg1

library(viridis)
library(ggthemes)
gg1 <- gg1 +scale_y_continuous(breaks=seq(0,23,1)) 
gg1
gg1 <- gg1 + coord_equal()
gg1
gg1 <- gg1 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg1 <- gg1 + theme_tufte(base_family="Helvetica")
gg1 <- gg1 + theme(plot.title=element_text(hjust=0))
gg1 <- gg1 + theme(axis.ticks=element_blank())
gg1 <- gg1 + theme(axis.text=element_text(size=9))
gg1 <- gg1 + theme(legend.title=element_text(size=9))
gg1 <- gg1 + theme(legend.text=element_text(size=9))
gg1
ggsave(file="Industry_usage_1.png", plot = gg1,width=400,units="mm",limitsize = FALSE)


Total_per_hour_na_removed_w2 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==2,]
Total_per_hour_na_removed_w2
Total_per_hour_na_removed_w2$Hour <- hour(Total_per_hour_na_removed_w2$mhr)
gg2 <-ggplot(Total_per_hour_na_removed_w2,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w2$Usage.kWh.)
#14
max(Total_per_hour_na_removed_w2$Usage.kWh.)
#515.4
gg2 <- gg2 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(13,550))
gg2

library(ggthemes)
gg2 <- gg2 +scale_y_continuous(breaks=seq(0,23,1))
gg2 <- gg2 + coord_equal()
gg2 <- gg2 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg2 <- gg2 + theme_tufte(base_family="Helvetica")
gg2 <- gg2 + theme(plot.title=element_text(hjust=0))
gg2 <- gg2 + theme(axis.ticks=element_blank())
gg2 <- gg2 + theme(axis.text=element_text(size=9))
gg2 <- gg2 + theme(legend.title=element_text(size=9))
gg2 <- gg2 + theme(legend.text=element_text(size=9))
gg2
ggsave(file="Industry_usage_2.png", plot = gg2,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w3 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==3,]
Total_per_hour_na_removed_w3
Total_per_hour_na_removed_w3$Hour <- hour(Total_per_hour_na_removed_w3$mhr)
gg3 <-ggplot(Total_per_hour_na_removed_w3,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w3$Usage.kWh.)
# 16.21
max(Total_per_hour_na_removed_w3$Usage.kWh.)
# 553.22

gg3 <- gg3 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(14,560))

library(viridis)
library(ggthemes)

gg3 <- gg3 +scale_y_continuous(breaks=seq(0,23,1))
gg3 <- gg3 + coord_equal()
gg3 <- gg3 + labs(x="Day of Week", y="Hour of day")#, title="Appliances energy consumption")
gg3 <- gg3 + theme_tufte(base_family="Helvetica")
gg3 <- gg3 + theme(plot.title=element_text(hjust=0))
gg3 <- gg3 + theme(axis.ticks=element_blank())
gg3 <- gg3 + theme(axis.text=element_text(size=9))
gg3 <- gg3 + theme(legend.title=element_text(size=9))
gg3 <- gg3 + theme(legend.text=element_text(size=9))
gg3
ggsave(file="Industry_usage_3.png", plot = gg3,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w4 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==4,]
Total_per_hour_na_removed_w4
Total_per_hour_na_removed_w4$Hour <- hour(Total_per_hour_na_removed_w4$mhr)
gg4 <-ggplot(Total_per_hour_na_removed_w4,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w4$Usage.kWh.)
# 16.22
max(Total_per_hour_na_removed_w4$Usage.kWh.)
#492.13

gg4 <- gg4 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(14,550))

library(viridis)
library(ggthemes)
gg4 <- gg4 +scale_y_continuous(breaks=seq(0,23,1))
gg4 <- gg4 + coord_equal()
gg4 <- gg4 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg4 <- gg4 + theme_tufte(base_family="Helvetica")
gg4 <- gg4 + theme(plot.title=element_text(hjust=0))
gg4 <- gg4 + theme(axis.ticks=element_blank())
gg4 <- gg4 + theme(axis.text=element_text(size=9))
gg4 <- gg4 + theme(legend.title=element_text(size=9))
gg4 <- gg4 + theme(legend.text=element_text(size=9))
gg4
ggsave(file="Industry_usage_4.png", plot = gg4,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w5 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==5,]
industry_data_Total_per_hour_na_removed_w5
industry_data_Total_per_hour_na_removed_w5$Hour <- hour(industry_data_Total_per_hour_na_removed_w5$mhr)
gg5 <-ggplot(industry_data_Total_per_hour_na_removed_w5,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg5

min(industry_data_Total_per_hour_na_removed_w5$Usage.kWh.)
# 15.76
max(industry_data_Total_per_hour_na_removed_w5$Usage.kWh.)
# 512.42
gg5 <- gg5 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(13,550))
gg5
gg5 <- gg5 +scale_y_continuous(breaks=seq(0,23,1)) 
gg5
gg5 <- gg5 + coord_equal()
gg5
gg5 <- gg5 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg5 <- gg5 + theme_tufte(base_family="Helvetica")
gg5 <- gg5 + theme(plot.title=element_text(hjust=0))
gg5 <- gg5 + theme(axis.ticks=element_blank())
gg5 <- gg5 + theme(axis.text=element_text(size=9))
gg5 <- gg5 + theme(legend.title=element_text(size=9))
gg5 <- gg5 + theme(legend.text=element_text(size=9))
gg5
ggsave(file="Industry_usage_5.png", plot = gg5,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w6 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==6,]
industry_data_Total_per_hour_na_removed_w6
industry_data_Total_per_hour_na_removed_w6$Hour <- hour(industry_data_Total_per_hour_na_removed_w6$mhr)
gg6 <-ggplot(industry_data_Total_per_hour_na_removed_w6,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg6
min(industry_data_Total_per_hour_na_removed_w6$Usage.kWh.)
# 15.74
max(industry_data_Total_per_hour_na_removed_w6$Usage.kWh.)
# 481.43
gg6 <- gg6 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(13,500))
gg6
gg6 <- gg6 +scale_y_continuous(breaks=seq(0,23,1)) 
gg6
gg6 <- gg6 + coord_equal()
gg6
gg6 <- gg6 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg6 <- gg6 + theme_tufte(base_family="Helvetica")
gg6 <- gg6 + theme(plot.title=element_text(hjust=0))
gg6 <- gg6 + theme(axis.ticks=element_blank())
gg6 <- gg6 + theme(axis.text=element_text(size=9))
gg6 <- gg6 + theme(legend.title=element_text(size=9))
gg6 <- gg6 + theme(legend.text=element_text(size=9))
gg6
ggsave(file="Industry_usage_6.png", plot = gg6,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w7 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==7,]
industry_data_Total_per_hour_na_removed_w7
industry_data_Total_per_hour_na_removed_w7$Hour <- hour(industry_data_Total_per_hour_na_removed_w7$mhr)
gg7 <-ggplot(industry_data_Total_per_hour_na_removed_w7,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg7

min(industry_data_Total_per_hour_na_removed_w7$Usage.kWh.)
# 12.28
max(industry_data_Total_per_hour_na_removed_w7$Usage.kWh.)
# 473.05
gg7 <- gg7 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(12,500))
gg7
gg7 <- gg7 +scale_y_continuous(breaks=seq(0,23,1)) 
gg7
gg7 <- gg7 + coord_equal()
gg7
gg7 <- gg7 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg7 <- gg7 + theme_tufte(base_family="Helvetica")
gg7 <- gg7 + theme(plot.title=element_text(hjust=0))
gg7 <- gg7 + theme(axis.ticks=element_blank())
gg7 <- gg7 + theme(axis.text=element_text(size=9))
gg7 <- gg7 + theme(legend.title=element_text(size=9))
gg7 <- gg7 + theme(legend.text=element_text(size=9))
gg7
ggsave(file="Industry_usage_7.png", plot = gg7,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w8 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==8,]
industry_data_Total_per_hour_na_removed_w8
industry_data_Total_per_hour_na_removed_w8$Hour <- hour(industry_data_Total_per_hour_na_removed_w8$mhr)
gg8 <-ggplot(industry_data_Total_per_hour_na_removed_w8,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg8

min(industry_data_Total_per_hour_na_removed_w8$Usage.kWh.)
# 13.22
max(industry_data_Total_per_hour_na_removed_w8$Usage.kWh.)
# 464.04
gg8 <- gg8 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(13,500))
gg8
gg8 <- gg8 +scale_y_continuous(breaks=seq(0,23,1)) 
gg8
gg8 <- gg8 + coord_equal()
gg8
gg8 <- gg8 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg8 <- gg8 + theme_tufte(base_family="Helvetica")
gg8 <- gg8 + theme(plot.title=element_text(hjust=0))
gg8 <- gg8 + theme(axis.ticks=element_blank())
gg8 <- gg8 + theme(axis.text=element_text(size=9))
gg8 <- gg8 + theme(legend.title=element_text(size=9))
gg8 <- gg8 + theme(legend.text=element_text(size=9))
gg8
ggsave(file="Industry_usage_8.png", plot = gg8,width=400,units="mm",limitsize = FALSE)



industry_data_Total_per_hour_na_removed_w9 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==9,]
industry_data_Total_per_hour_na_removed_w9
industry_data_Total_per_hour_na_removed_w9$Hour <- hour(industry_data_Total_per_hour_na_removed_w9$mhr)
gg9 <-ggplot(industry_data_Total_per_hour_na_removed_w9,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg9

min(industry_data_Total_per_hour_na_removed_w9$Usage.kWh.)
# 12.09
max(industry_data_Total_per_hour_na_removed_w9$Usage.kWh.)
# 452.92
gg9 <- gg9 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(12,500))
gg9
gg9 <- gg9 +scale_y_continuous(breaks=seq(0,23,1)) 
gg9
gg9 <- gg9 + coord_equal()
gg9
gg9 <- gg9 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg9 <- gg9 + theme_tufte(base_family="Helvetica")
gg9 <- gg9 + theme(plot.title=element_text(hjust=0))
gg9 <- gg9 + theme(axis.ticks=element_blank())
gg9 <- gg9 + theme(axis.text=element_text(size=9))
gg9 <- gg9 + theme(legend.title=element_text(size=9))
gg9 <- gg9 + theme(legend.text=element_text(size=9))
gg9
ggsave(file="Industry_usage_9.png", plot = gg9,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w10 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==10,]
industry_data_Total_per_hour_na_removed_w10
industry_data_Total_per_hour_na_removed_w10$Hour <- hour(industry_data_Total_per_hour_na_removed_w10$mhr)
gg10 <-ggplot(industry_data_Total_per_hour_na_removed_w10,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg10

min(industry_data_Total_per_hour_na_removed_w10$Usage.kWh.)
# 12.23
max(industry_data_Total_per_hour_na_removed_w10$Usage.kWh.)
# 446.62
gg10 <- gg10 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(12,500))
gg10
gg10 <- gg10 +scale_y_continuous(breaks=seq(0,23,1)) 
gg10
gg10 <- gg10 + coord_equal()
gg10
gg10 <- gg10 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg10 <- gg10 + theme_tufte(base_family="Helvetica")
gg10 <- gg10 + theme(plot.title=element_text(hjust=0))
gg10 <- gg10 + theme(axis.ticks=element_blank())
gg10 <- gg10 + theme(axis.text=element_text(size=9))
gg10 <- gg10 + theme(legend.title=element_text(size=9))
gg10 <- gg10 + theme(legend.text=element_text(size=9))
gg10
ggsave(file="Industry_usage_10.png", plot = gg10,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w11 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==11,]
industry_data_Total_per_hour_na_removed_w11
industry_data_Total_per_hour_na_removed_w11$Hour <- hour(industry_data_Total_per_hour_na_removed_w11$mhr)
library(ggplot2)
library(lubridate)
# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg11 <-ggplot(industry_data_Total_per_hour_na_removed_w11,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg11

min(industry_data_Total_per_hour_na_removed_w11$Usage.kWh.)
# 11.99
max(industry_data_Total_per_hour_na_removed_w11$Usage.kWh.)
# 444.35
gg11 <- gg11 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,500))
gg11

library(viridis)
library(ggthemes)
gg11 <- gg11 +scale_y_continuous(breaks=seq(0,23,1)) 
gg11
gg11 <- gg11 + coord_equal()
gg11
gg11 <- gg11 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg11 <- gg11 + theme_tufte(base_family="Helvetica")
gg11 <- gg11 + theme(plot.title=element_text(hjust=0))
gg11 <- gg11 + theme(axis.ticks=element_blank())
gg11 <- gg11 + theme(axis.text=element_text(size=9))
gg11 <- gg11 + theme(legend.title=element_text(size=9))
gg11 <- gg11 + theme(legend.text=element_text(size=9))
gg11
ggsave(file="Industry_usage_11.png", plot = gg11,width=400,units="mm",limitsize = FALSE)


Total_per_hour_na_removed_w12 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==12,]
Total_per_hour_na_removed_w12
Total_per_hour_na_removed_w12$Hour <- hour(Total_per_hour_na_removed_w12$mhr)
gg12 <-ggplot(Total_per_hour_na_removed_w12,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w12$Usage.kWh.)
#12.06
max(Total_per_hour_na_removed_w12$Usage.kWh.)
#505.84
gg12 <- gg12 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(12,550))
gg12

library(ggthemes)
gg12 <- gg12 +scale_y_continuous(breaks=seq(0,23,1))
gg12 <- gg12 + coord_equal()
gg12 <- gg12 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg12 <- gg12 + theme_tufte(base_family="Helvetica")
gg12 <- gg12 + theme(plot.title=element_text(hjust=0))
gg12 <- gg12 + theme(axis.ticks=element_blank())
gg12 <- gg12 + theme(axis.text=element_text(size=9))
gg12 <- gg12 + theme(legend.title=element_text(size=9))
gg12 <- gg12 + theme(legend.text=element_text(size=9))
gg12
ggsave(file="Industry_usage_12.png", plot = gg12,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w13 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==13,]
Total_per_hour_na_removed_w13
Total_per_hour_na_removed_w13$Hour <- hour(Total_per_hour_na_removed_w13$mhr)
gg13 <-ggplot(Total_per_hour_na_removed_w13,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w13$Usage.kWh.)
# 10.72
max(Total_per_hour_na_removed_w13$Usage.kWh.)
# 396.90

gg13 <- gg13 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(10,400))

library(viridis)
library(ggthemes)

gg13 <- gg13 +scale_y_continuous(breaks=seq(0,23,1))
gg13 <- gg13 + coord_equal()
gg13 <- gg13 + labs(x="Day of Week", y="Hour of day")#, title="Appliances energy consumption")
gg13 <- gg13 + theme_tufte(base_family="Helvetica")
gg13 <- gg13 + theme(plot.title=element_text(hjust=0))
gg13 <- gg13 + theme(axis.ticks=element_blank())
gg13 <- gg13 + theme(axis.text=element_text(size=9))
gg13 <- gg13 + theme(legend.title=element_text(size=9))
gg13 <- gg13 + theme(legend.text=element_text(size=9))
gg13
ggsave(file="Industry_usage_13.png", plot = gg13,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w14 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==14,]
Total_per_hour_na_removed_w14
Total_per_hour_na_removed_w14$Hour <- hour(Total_per_hour_na_removed_w14$mhr)
gg14 <-ggplot(Total_per_hour_na_removed_w14,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w14$Usage.kWh.)
# 11.24
max(Total_per_hour_na_removed_w14$Usage.kWh.)
#416.88

gg14 <- gg14 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(11,450))

library(viridis)
library(ggthemes)
gg14 <- gg14 +scale_y_continuous(breaks=seq(0,23,1))
gg14 <- gg14 + coord_equal()
gg14 <- gg14 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg14 <- gg14 + theme_tufte(base_family="Helvetica")
gg14 <- gg14 + theme(plot.title=element_text(hjust=0))
gg14 <- gg14 + theme(axis.ticks=element_blank())
gg14 <- gg14 + theme(axis.text=element_text(size=9))
gg14 <- gg14 + theme(legend.title=element_text(size=9))
gg14 <- gg14 + theme(legend.text=element_text(size=9))
gg14
ggsave(file="Industry_usage_14.png", plot = gg14,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w15 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==15,]
industry_data_Total_per_hour_na_removed_w15
industry_data_Total_per_hour_na_removed_w15$Hour <- hour(industry_data_Total_per_hour_na_removed_w15$mhr)
gg15 <-ggplot(industry_data_Total_per_hour_na_removed_w15,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg15

min(industry_data_Total_per_hour_na_removed_w15$Usage.kWh.)
# 12.11
max(industry_data_Total_per_hour_na_removed_w15$Usage.kWh.)
# 401.51
gg15 <- gg15 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(12,450))
gg15
gg15 <- gg15 +scale_y_continuous(breaks=seq(0,23,1)) 
gg15
gg15 <- gg15 + coord_equal()
gg15
gg15 <- gg15 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg15 <- gg15 + theme_tufte(base_family="Helvetica")
gg15 <- gg15 + theme(plot.title=element_text(hjust=0))
gg15 <- gg15 + theme(axis.ticks=element_blank())
gg15 <- gg15 + theme(axis.text=element_text(size=9))
gg15 <- gg15 + theme(legend.title=element_text(size=9))
gg15 <- gg15 + theme(legend.text=element_text(size=9))
gg15
ggsave(file="Industry_usage_15.png", plot = gg15,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w16 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==16,]
industry_data_Total_per_hour_na_removed_w16
industry_data_Total_per_hour_na_removed_w16$Hour <- hour(industry_data_Total_per_hour_na_removed_w16$mhr)
gg16 <-ggplot(industry_data_Total_per_hour_na_removed_w16,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg16
min(industry_data_Total_per_hour_na_removed_w16$Usage.kWh.)
# 11.77
max(industry_data_Total_per_hour_na_removed_w16$Usage.kWh.)
# 445.35
gg16 <- gg16 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,500))
gg16
gg16 <- gg16 +scale_y_continuous(breaks=seq(0,23,1)) 
gg16
gg16 <- gg16 + coord_equal()
gg16
gg16 <- gg16 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg16 <- gg16 + theme_tufte(base_family="Helvetica")
gg16 <- gg16 + theme(plot.title=element_text(hjust=0))
gg16 <- gg16 + theme(axis.ticks=element_blank())
gg16 <- gg16 + theme(axis.text=element_text(size=9))
gg16 <- gg16 + theme(legend.title=element_text(size=9))
gg16 <- gg16 + theme(legend.text=element_text(size=9))
gg16
ggsave(file="Industry_usage_16.png", plot = gg16,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w17 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==17,]
industry_data_Total_per_hour_na_removed_w17
industry_data_Total_per_hour_na_removed_w17$Hour <- hour(industry_data_Total_per_hour_na_removed_w17$mhr)
gg17 <-ggplot(industry_data_Total_per_hour_na_removed_w17,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg17

min(industry_data_Total_per_hour_na_removed_w17$Usage.kWh.)
# 10.4
max(industry_data_Total_per_hour_na_removed_w17$Usage.kWh.)
# 440.06
gg17 <- gg17 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,500))
gg17
gg17 <- gg17 +scale_y_continuous(breaks=seq(0,23,1)) 
gg17
gg17 <- gg17 + coord_equal()
gg17
gg17 <- gg17 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg17 <- gg17 + theme_tufte(base_family="Helvetica")
gg17 <- gg17 + theme(plot.title=element_text(hjust=0))
gg17 <- gg17 + theme(axis.ticks=element_blank())
gg17 <- gg17 + theme(axis.text=element_text(size=9))
gg17 <- gg17 + theme(legend.title=element_text(size=9))
gg17 <- gg17 + theme(legend.text=element_text(size=9))
gg17
ggsave(file="Industry_usage_17.png", plot = gg17,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w18 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==18,]
industry_data_Total_per_hour_na_removed_w18
industry_data_Total_per_hour_na_removed_w18$Hour <- hour(industry_data_Total_per_hour_na_removed_w18$mhr)
gg18 <-ggplot(industry_data_Total_per_hour_na_removed_w18,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg18

min(industry_data_Total_per_hour_na_removed_w18$Usage.kWh.)
# 11.41
max(industry_data_Total_per_hour_na_removed_w18$Usage.kWh.)
# 466.95
gg18 <- gg18 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,500))
gg18
gg18 <- gg18 +scale_y_continuous(breaks=seq(0,23,1)) 
gg18
gg18 <- gg18 + coord_equal()
gg18
gg18 <- gg18 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg18 <- gg18 + theme_tufte(base_family="Helvetica")
gg18 <- gg18 + theme(plot.title=element_text(hjust=0))
gg18 <- gg18 + theme(axis.ticks=element_blank())
gg18 <- gg18 + theme(axis.text=element_text(size=9))
gg18 <- gg18 + theme(legend.title=element_text(size=9))
gg18 <- gg18 + theme(legend.text=element_text(size=9))
gg18
ggsave(file="Industry_usage_18.png", plot = gg18,width=400,units="mm",limitsize = FALSE)



industry_data_Total_per_hour_na_removed_w19 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==19,]
industry_data_Total_per_hour_na_removed_w19
industry_data_Total_per_hour_na_removed_w19$Hour <- hour(industry_data_Total_per_hour_na_removed_w19$mhr)
gg19 <-ggplot(industry_data_Total_per_hour_na_removed_w19,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg19

min(industry_data_Total_per_hour_na_removed_w19$Usage.kWh.)
# 11.52
max(industry_data_Total_per_hour_na_removed_w19$Usage.kWh.)
# 483.37
gg19 <- gg19 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,500))
gg19
gg19 <- gg19 +scale_y_continuous(breaks=seq(0,23,1)) 
gg19
gg19 <- gg19 + coord_equal()
gg19
gg19 <- gg19 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg19 <- gg19 + theme_tufte(base_family="Helvetica")
gg19 <- gg19 + theme(plot.title=element_text(hjust=0))
gg19 <- gg19 + theme(axis.ticks=element_blank())
gg19 <- gg19 + theme(axis.text=element_text(size=9))
gg19 <- gg19 + theme(legend.title=element_text(size=9))
gg19 <- gg19 + theme(legend.text=element_text(size=9))
gg19
ggsave(file="Industry_usage_19.png", plot = gg19,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w20 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==20,]
industry_data_Total_per_hour_na_removed_w20
industry_data_Total_per_hour_na_removed_w20$Hour <- hour(industry_data_Total_per_hour_na_removed_w20$mhr)
gg20 <-ggplot(industry_data_Total_per_hour_na_removed_w20,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg20

min(industry_data_Total_per_hour_na_removed_w20$Usage.kWh.)
# 11.51
max(industry_data_Total_per_hour_na_removed_w20$Usage.kWh.)
# 456.99
gg20 <- gg20 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,500))
gg20
gg20 <- gg20 +scale_y_continuous(breaks=seq(0,23,1)) 
gg20
gg20 <- gg20 + coord_equal()
gg20
gg20 <- gg20 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg20 <- gg20 + theme_tufte(base_family="Helvetica")
gg20 <- gg20 + theme(plot.title=element_text(hjust=0))
gg20 <- gg20 + theme(axis.ticks=element_blank())
gg20 <- gg20 + theme(axis.text=element_text(size=9))
gg20 <- gg20 + theme(legend.title=element_text(size=9))
gg20 <- gg20 + theme(legend.text=element_text(size=9))
gg20
ggsave(file="Industry_usage_20.png", plot = gg20,width=400,units="mm",limitsize = FALSE)




industry_data_Total_per_hour_na_removed_w21 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==21,]
industry_data_Total_per_hour_na_removed_w21
industry_data_Total_per_hour_na_removed_w21$Hour <- hour(industry_data_Total_per_hour_na_removed_w21$mhr)

# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg21 <-ggplot(industry_data_Total_per_hour_na_removed_w21,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg21

min(industry_data_Total_per_hour_na_removed_w21$Usage.kWh.)
# 11.02
max(industry_data_Total_per_hour_na_removed_w21$Usage.kWh.)
# 456.12
gg21 <- gg21 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,500))
gg21

library(viridis)
library(ggthemes)
gg21 <- gg1 +scale_y_continuous(breaks=seq(0,23,1)) 
gg21
gg21 <- gg21 + coord_equal()
gg21
gg21 <- gg21 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg21 <- gg21 + theme_tufte(base_family="Helvetica")
gg21 <- gg21 + theme(plot.title=element_text(hjust=0))
gg21 <- gg21 + theme(axis.ticks=element_blank())
gg21 <- gg21 + theme(axis.text=element_text(size=9))
gg21 <- gg21 + theme(legend.title=element_text(size=9))
gg21 <- gg21 + theme(legend.text=element_text(size=9))
gg21
ggsave(file="Industry_usage_21.png", plot = gg21,width=400,units="mm",limitsize = FALSE)


Total_per_hour_na_removed_w22 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==22,]
Total_per_hour_na_removed_w22
Total_per_hour_na_removed_w22$Hour <- hour(Total_per_hour_na_removed_w22$mhr)
gg22 <-ggplot(Total_per_hour_na_removed_w22,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w22$Usage.kWh.)
#11.43
max(Total_per_hour_na_removed_w22$Usage.kWh.)
#404.03
gg22 <- gg22 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(11,410))
gg22

library(ggthemes)
gg22 <- gg22 +scale_y_continuous(breaks=seq(0,23,1))
gg22 <- gg22 + coord_equal()
gg22 <- gg22 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg22 <- gg22 + theme_tufte(base_family="Helvetica")
gg22 <- gg22 + theme(plot.title=element_text(hjust=0))
gg22 <- gg22 + theme(axis.ticks=element_blank())
gg22 <- gg22 + theme(axis.text=element_text(size=9))
gg22 <- gg22 + theme(legend.title=element_text(size=9))
gg22 <- gg22 + theme(legend.text=element_text(size=9))
gg22
ggsave(file="Industry_usage_22.png", plot = gg22,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w223 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==23,]
Total_per_hour_na_removed_w223
Total_per_hour_na_removed_w223$Hour <- hour(Total_per_hour_na_removed_w223$mhr)
gg23 <-ggplot(Total_per_hour_na_removed_w223,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w223$Usage.kWh.)
# 11.2
max(Total_per_hour_na_removed_w223$Usage.kWh.)
# 428.83

gg23 <- gg23 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(11,450))

library(viridis)
library(ggthemes)

gg23 <- gg23 +scale_y_continuous(breaks=seq(0,23,1))
gg23 <- gg23 + coord_equal()
gg23 <- gg23 + labs(x="Day of Week", y="Hour of day")#, title="Appliances energy consumption")
gg23 <- gg23 + theme_tufte(base_family="Helvetica")
gg23 <- gg23 + theme(plot.title=element_text(hjust=0))
gg23 <- gg23 + theme(axis.ticks=element_blank())
gg23 <- gg23 + theme(axis.text=element_text(size=9))
gg23 <- gg23 + theme(legend.title=element_text(size=9))
gg23 <- gg23 + theme(legend.text=element_text(size=9))
gg23
ggsave(file="Industry_usage_23.png", plot = gg23,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w24 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==24,]
Total_per_hour_na_removed_w24
Total_per_hour_na_removed_w24$Hour <- hour(Total_per_hour_na_removed_w24$mhr)
gg24 <-ggplot(Total_per_hour_na_removed_w24,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w24$Usage.kWh.)
# 11.03
max(Total_per_hour_na_removed_w24$Usage.kWh.)
#403.85

gg24 <- gg24 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(11,450))

library(viridis)
library(ggthemes)
gg24 <- gg24 +scale_y_continuous(breaks=seq(0,23,1))
gg24 <- gg24 + coord_equal()
gg24 <- gg24 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg24 <- gg24 + theme_tufte(base_family="Helvetica")
gg24 <- gg24 + theme(plot.title=element_text(hjust=0))
gg24 <- gg24 + theme(axis.ticks=element_blank())
gg24 <- gg24 + theme(axis.text=element_text(size=9))
gg24 <- gg24 + theme(legend.title=element_text(size=9))
gg24 <- gg24 + theme(legend.text=element_text(size=9))
gg24
ggsave(file="Industry_usage_24.png", plot = gg24,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w25 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==25,]
industry_data_Total_per_hour_na_removed_w25
industry_data_Total_per_hour_na_removed_w25$Hour <- hour(industry_data_Total_per_hour_na_removed_w25$mhr)
gg25 <-ggplot(industry_data_Total_per_hour_na_removed_w25,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg25

min(industry_data_Total_per_hour_na_removed_w25$Usage.kWh.)
# 10.23
max(industry_data_Total_per_hour_na_removed_w25$Usage.kWh.)
# 391.22
gg25 <- gg25 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,400))
gg25
gg25 <- gg25 +scale_y_continuous(breaks=seq(0,23,1)) 
gg25
gg25 <- gg25 + coord_equal()
gg25
gg25 <- gg25 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg25 <- gg25 + theme_tufte(base_family="Helvetica")
gg25 <- gg25 + theme(plot.title=element_text(hjust=0))
gg25 <- gg25 + theme(axis.ticks=element_blank())
gg25 <- gg25 + theme(axis.text=element_text(size=9))
gg25 <- gg25 + theme(legend.title=element_text(size=9))
gg25 <- gg25 + theme(legend.text=element_text(size=9))
gg25
ggsave(file="Industry_usage_25.png", plot = gg25,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w26 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==26,]
industry_data_Total_per_hour_na_removed_w26
industry_data_Total_per_hour_na_removed_w26$Hour <- hour(industry_data_Total_per_hour_na_removed_w26$mhr)
gg26 <-ggplot(industry_data_Total_per_hour_na_removed_w26,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg26
min(industry_data_Total_per_hour_na_removed_w26$Usage.kWh.)
# 10.44
max(industry_data_Total_per_hour_na_removed_w26$Usage.kWh.)
# 397.7
gg26 <- gg26 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,400))
gg26
gg26 <- gg26 +scale_y_continuous(breaks=seq(0,23,1)) 
gg26
gg26 <- gg26 + coord_equal()
gg26
gg26 <- gg26 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg26 <- gg26 + theme_tufte(base_family="Helvetica")
gg26 <- gg26 + theme(plot.title=element_text(hjust=0))
gg26 <- gg26 + theme(axis.ticks=element_blank())
gg26 <- gg26 + theme(axis.text=element_text(size=9))
gg26 <- gg26 + theme(legend.title=element_text(size=9))
gg26 <- gg26 + theme(legend.text=element_text(size=9))
gg26
ggsave(file="Industry_usage_26.png", plot = gg26,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w27 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==27,]
industry_data_Total_per_hour_na_removed_w27
industry_data_Total_per_hour_na_removed_w27$Hour <- hour(industry_data_Total_per_hour_na_removed_w27$mhr)
gg27 <-ggplot(industry_data_Total_per_hour_na_removed_w27,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg27

min(industry_data_Total_per_hour_na_removed_w27$Usage.kWh.)
# 11.08
max(industry_data_Total_per_hour_na_removed_w27$Usage.kWh.)
# 412.38
gg27 <- gg27 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,420))
gg27
gg27 <- gg27 +scale_y_continuous(breaks=seq(0,23,1)) 
gg27
gg27 <- gg27 + coord_equal()
gg27
gg27 <- gg27 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg27 <- gg27 + theme_tufte(base_family="Helvetica")
gg27 <- gg27 + theme(plot.title=element_text(hjust=0))
gg27 <- gg27 + theme(axis.ticks=element_blank())
gg27 <- gg27 + theme(axis.text=element_text(size=9))
gg27 <- gg27 + theme(legend.title=element_text(size=9))
gg27 <- gg27 + theme(legend.text=element_text(size=9))
gg27
ggsave(file="Industry_usage_27.png", plot = gg27,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w28 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==28,]
industry_data_Total_per_hour_na_removed_w28
industry_data_Total_per_hour_na_removed_w28$Hour <- hour(industry_data_Total_per_hour_na_removed_w28$mhr)
gg28 <-ggplot(industry_data_Total_per_hour_na_removed_w28,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg28

min(industry_data_Total_per_hour_na_removed_w28$Usage.kWh.)
# 10.59
max(industry_data_Total_per_hour_na_removed_w28$Usage.kWh.)
# 411.2
gg28 <- gg28 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,420))
gg28
gg28 <- gg28 +scale_y_continuous(breaks=seq(0,23,1)) 
gg28
gg28 <- gg28 + coord_equal()
gg28
gg28 <- gg28 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg28 <- gg28 + theme_tufte(base_family="Helvetica")
gg28 <- gg28 + theme(plot.title=element_text(hjust=0))
gg28 <- gg28 + theme(axis.ticks=element_blank())
gg28 <- gg28 + theme(axis.text=element_text(size=9))
gg28 <- gg28 + theme(legend.title=element_text(size=9))
gg28 <- gg28 + theme(legend.text=element_text(size=9))
gg28
ggsave(file="Industry_usage_28.png", plot = gg28,width=400,units="mm",limitsize = FALSE)



industry_data_Total_per_hour_na_removed_w29 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==29,]
industry_data_Total_per_hour_na_removed_w29
industry_data_Total_per_hour_na_removed_w29$Hour <- hour(industry_data_Total_per_hour_na_removed_w29$mhr)
gg29 <-ggplot(industry_data_Total_per_hour_na_removed_w29,aes(x=Day_week,y=Hour,
                                                            fill=Usage.kWh.)) 
gg29

min(industry_data_Total_per_hour_na_removed_w29$Usage.kWh.)
# 10.69
max(industry_data_Total_per_hour_na_removed_w29$Usage.kWh.)
# 375.95
gg29 <- gg29 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,380))
gg29
gg29 <- gg29 +scale_y_continuous(breaks=seq(0,23,1)) 
gg29
gg29 <- gg29 + coord_equal()
gg29
gg29 <- gg29 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg29 <- gg29 + theme_tufte(base_family="Helvetica")
gg29 <- gg29 + theme(plot.title=element_text(hjust=0))
gg29 <- gg29 + theme(axis.ticks=element_blank())
gg29 <- gg29 + theme(axis.text=element_text(size=9))
gg29 <- gg29 + theme(legend.title=element_text(size=9))
gg29 <- gg29 + theme(legend.text=element_text(size=9))
gg29
ggsave(file="Industry_usage_29.png", plot = gg29,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w30 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==30,]
industry_data_Total_per_hour_na_removed_w30
industry_data_Total_per_hour_na_removed_w30$Hour <- hour(industry_data_Total_per_hour_na_removed_w30$mhr)
gg30 <-ggplot(industry_data_Total_per_hour_na_removed_w30,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg30

min(industry_data_Total_per_hour_na_removed_w30$Usage.kWh.)
# 10.69
max(industry_data_Total_per_hour_na_removed_w30$Usage.kWh.)
# 399.63
gg30 <- gg30 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,400))
gg30
gg30 <- gg30 +scale_y_continuous(breaks=seq(0,23,1)) 
gg30
gg30 <- gg30 + coord_equal()
gg30
gg30 <- gg30 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg30 <- gg30 + theme_tufte(base_family="Helvetica")
gg30 <- gg30 + theme(plot.title=element_text(hjust=0))
gg30 <- gg30 + theme(axis.ticks=element_blank())
gg30 <- gg30 + theme(axis.text=element_text(size=9))
gg30 <- gg30 + theme(legend.title=element_text(size=9))
gg30 <- gg30 + theme(legend.text=element_text(size=9))
gg30
ggsave(file="Industry_usage_30.png", plot = gg30,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w31 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==31,]
industry_data_Total_per_hour_na_removed_w31
industry_data_Total_per_hour_na_removed_w31$Hour <- hour(industry_data_Total_per_hour_na_removed_w31$mhr)
library(ggplot2)
library(lubridate)
# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg31 <-ggplot(industry_data_Total_per_hour_na_removed_w31,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg31

min(industry_data_Total_per_hour_na_removed_w31$Usage.kWh.)
# 10.44
max(industry_data_Total_per_hour_na_removed_w31$Usage.kWh.)
# 351.57
gg31 <- gg31 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,360))
gg31

library(viridis)
library(ggthemes)
gg31 <- gg31 +scale_y_continuous(breaks=seq(0,23,1)) 
gg31
gg31 <- gg31 + coord_equal()
gg31
gg31 <- gg31 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg31 <- gg31 + theme_tufte(base_family="Helvetica")
gg31 <- gg31 + theme(plot.title=element_text(hjust=0))
gg31 <- gg31 + theme(axis.ticks=element_blank())
gg31 <- gg31 + theme(axis.text=element_text(size=9))
gg31 <- gg31 + theme(legend.title=element_text(size=9))
gg31 <- gg31 + theme(legend.text=element_text(size=9))
gg31
ggsave(file="Industry_usage_31.png", plot = gg31,width=400,units="mm",limitsize = FALSE)


Total_per_hour_na_removed_w32 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==32,]
Total_per_hour_na_removed_w32
Total_per_hour_na_removed_w32$Hour <- hour(Total_per_hour_na_removed_w32$mhr)
gg32 <-ggplot(Total_per_hour_na_removed_w32,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w32$Usage.kWh.)
#10.3
max(Total_per_hour_na_removed_w32$Usage.kWh.)
#429.18
gg32 <- gg32 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(10,430))
gg32

library(ggthemes)
gg32 <- gg32 +scale_y_continuous(breaks=seq(0,23,1))
gg32 <- gg32 + coord_equal()
gg32 <- gg32 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg32 <- gg32 + theme_tufte(base_family="Helvetica")
gg32 <- gg32 + theme(plot.title=element_text(hjust=0))
gg32 <- gg32 + theme(axis.ticks=element_blank())
gg32 <- gg32 + theme(axis.text=element_text(size=9))
gg32 <- gg32 + theme(legend.title=element_text(size=9))
gg32 <- gg32 + theme(legend.text=element_text(size=9))
gg32
ggsave(file="Industry_usage_32.png", plot = gg32,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w33 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==33,]
Total_per_hour_na_removed_w33
Total_per_hour_na_removed_w33$Hour <- hour(Total_per_hour_na_removed_w33$mhr)
gg33 <-ggplot(Total_per_hour_na_removed_w33,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w33$Usage.kWh.)
# 10.12
max(Total_per_hour_na_removed_w33$Usage.kWh.)
# 412.24

gg33 <- gg33 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(10,420))

library(viridis)
library(ggthemes)

gg33 <- gg33 +scale_y_continuous(breaks=seq(0,23,1))
gg33 <- gg33 + coord_equal()
gg33 <- gg33 + labs(x="Day of Week", y="Hour of day")#, title="Appliances energy consumption")
gg33 <- gg33 + theme_tufte(base_family="Helvetica")
gg33 <- gg33 + theme(plot.title=element_text(hjust=0))
gg33 <- gg33 + theme(axis.ticks=element_blank())
gg33 <- gg33 + theme(axis.text=element_text(size=9))
gg33 <- gg33 + theme(legend.title=element_text(size=9))
gg33 <- gg33 + theme(legend.text=element_text(size=9))
gg33
ggsave(file="Industry_usage_33.png", plot = gg33,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w34 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==34,]
Total_per_hour_na_removed_w34
Total_per_hour_na_removed_w34$Hour <- hour(Total_per_hour_na_removed_w34$mhr)
gg34 <-ggplot(Total_per_hour_na_removed_w34,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w34$Usage.kWh.)
# 10.3
max(Total_per_hour_na_removed_w34$Usage.kWh.)
#395.82

gg34 <- gg34 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(10,400))

library(viridis)
library(ggthemes)
gg34 <- gg34 +scale_y_continuous(breaks=seq(0,23,1))
gg34 <- gg34 + coord_equal()
gg34 <- gg34 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg34 <- gg34 + theme_tufte(base_family="Helvetica")
gg34 <- gg34 + theme(plot.title=element_text(hjust=0))
gg34 <- gg34 + theme(axis.ticks=element_blank())
gg34 <- gg34 + theme(axis.text=element_text(size=9))
gg34 <- gg34 + theme(legend.title=element_text(size=9))
gg34 <- gg34 + theme(legend.text=element_text(size=9))
gg34
ggsave(file="Industry_usage_34.png", plot = gg34,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w35 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==35,]
industry_data_Total_per_hour_na_removed_w35
industry_data_Total_per_hour_na_removed_w35$Hour <- hour(industry_data_Total_per_hour_na_removed_w35$mhr)
gg35 <-ggplot(industry_data_Total_per_hour_na_removed_w35,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg35

min(industry_data_Total_per_hour_na_removed_w35$Usage.kWh.)
# 10.22
max(industry_data_Total_per_hour_na_removed_w35$Usage.kWh.)
# 369.03
gg35 <- gg35 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,370))
gg35
gg35 <- gg35 +scale_y_continuous(breaks=seq(0,23,1)) 
gg35
gg35 <- gg35 + coord_equal()
gg35
gg35 <- gg35 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg35 <- gg35 + theme_tufte(base_family="Helvetica")
gg35 <- gg35 + theme(plot.title=element_text(hjust=0))
gg35 <- gg35 + theme(axis.ticks=element_blank())
gg35 <- gg35 + theme(axis.text=element_text(size=9))
gg35 <- gg35 + theme(legend.title=element_text(size=9))
gg35 <- gg35 + theme(legend.text=element_text(size=9))
gg35
ggsave(file="Industry_usage_35.png", plot = gg35,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w36 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==36,]
industry_data_Total_per_hour_na_removed_w36
industry_data_Total_per_hour_na_removed_w36$Hour <- hour(industry_data_Total_per_hour_na_removed_w36$mhr)
gg36 <-ggplot(industry_data_Total_per_hour_na_removed_w36,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg36
min(industry_data_Total_per_hour_na_removed_w36$Usage.kWh.)
# 10
max(industry_data_Total_per_hour_na_removed_w36$Usage.kWh.)
# 440.35
gg36 <- gg36 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,450))
gg36
gg36 <- gg36 +scale_y_continuous(breaks=seq(0,23,1)) 
gg36
gg36 <- gg36 + coord_equal()
gg36
gg36 <- gg36 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg36 <- gg36 + theme_tufte(base_family="Helvetica")
gg36 <- gg36 + theme(plot.title=element_text(hjust=0))
gg36 <- gg36 + theme(axis.ticks=element_blank())
gg36 <- gg36 + theme(axis.text=element_text(size=9))
gg36 <- gg36 + theme(legend.title=element_text(size=9))
gg36 <- gg36 + theme(legend.text=element_text(size=9))
gg36
ggsave(file="Industry_usage_36.png", plot = gg36,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w37 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==37,]
industry_data_Total_per_hour_na_removed_w37
industry_data_Total_per_hour_na_removed_w37$Hour <- hour(industry_data_Total_per_hour_na_removed_w37$mhr)
gg37 <-ggplot(industry_data_Total_per_hour_na_removed_w37,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg37

min(industry_data_Total_per_hour_na_removed_w37$Usage.kWh.)
# 10.3
max(industry_data_Total_per_hour_na_removed_w37$Usage.kWh.)
# 432.5
gg37 <- gg37 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,450))
gg37
gg37 <- gg37 +scale_y_continuous(breaks=seq(0,23,1)) 
gg37
gg37 <- gg37 + coord_equal()
gg37
gg37 <- gg37 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg37 <- gg37 + theme_tufte(base_family="Helvetica")
gg37 <- gg37 + theme(plot.title=element_text(hjust=0))
gg37 <- gg37 + theme(axis.ticks=element_blank())
gg37 <- gg37 + theme(axis.text=element_text(size=9))
gg37 <- gg37 + theme(legend.title=element_text(size=9))
gg37 <- gg37 + theme(legend.text=element_text(size=9))
gg37
ggsave(file="Industry_usage_37.png", plot = gg37,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w38 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==38,]
industry_data_Total_per_hour_na_removed_w38
industry_data_Total_per_hour_na_removed_w38$Hour <- hour(industry_data_Total_per_hour_na_removed_w38$mhr)
gg38 <-ggplot(industry_data_Total_per_hour_na_removed_w38,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg38

min(industry_data_Total_per_hour_na_removed_w38$Usage.kWh.)
# 10.08
max(industry_data_Total_per_hour_na_removed_w38$Usage.kWh.)
# 438.55
gg38 <- gg38 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,450))
gg38
gg38 <- gg38 +scale_y_continuous(breaks=seq(0,23,1)) 
gg38
gg38 <- gg38 + coord_equal()
gg38
gg38 <- gg38 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg38 <- gg38 + theme_tufte(base_family="Helvetica")
gg38 <- gg38 + theme(plot.title=element_text(hjust=0))
gg38 <- gg38 + theme(axis.ticks=element_blank())
gg38 <- gg38 + theme(axis.text=element_text(size=9))
gg38 <- gg38 + theme(legend.title=element_text(size=9))
gg38 <- gg38 + theme(legend.text=element_text(size=9))
gg38
ggsave(file="Industry_usage_38.png", plot = gg38,width=400,units="mm",limitsize = FALSE)



industry_data_Total_per_hour_na_removed_w39 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==39,]
industry_data_Total_per_hour_na_removed_w39
industry_data_Total_per_hour_na_removed_w39$Hour <- hour(industry_data_Total_per_hour_na_removed_w39$mhr)
gg39 <-ggplot(industry_data_Total_per_hour_na_removed_w39,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg39

min(industry_data_Total_per_hour_na_removed_w39$Usage.kWh.)
# 9.86
max(industry_data_Total_per_hour_na_removed_w39$Usage.kWh.)
# 433.76
gg39 <- gg39 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(9,450))
gg39
gg39 <- gg39 +scale_y_continuous(breaks=seq(0,23,1)) 
gg39
gg39 <- gg39 + coord_equal()
gg39
gg39 <- gg39 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg39 <- gg39 + theme_tufte(base_family="Helvetica")
gg39 <- gg39 + theme(plot.title=element_text(hjust=0))
gg39 <- gg39 + theme(axis.ticks=element_blank())
gg39 <- gg39 + theme(axis.text=element_text(size=9))
gg39 <- gg39 + theme(legend.title=element_text(size=9))
gg39 <- gg39 + theme(legend.text=element_text(size=9))
gg39
ggsave(file="Industry_usage_39.png", plot = gg39,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w40 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==40,]
industry_data_Total_per_hour_na_removed_w40
industry_data_Total_per_hour_na_removed_w40$Hour <- hour(industry_data_Total_per_hour_na_removed_w40$mhr)
gg40 <-ggplot(industry_data_Total_per_hour_na_removed_w40,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg40

min(industry_data_Total_per_hour_na_removed_w40$Usage.kWh.)
# 9.97
max(industry_data_Total_per_hour_na_removed_w40$Usage.kWh.)
# 420.41
gg40 <- gg40 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(9,500))
gg40
gg40 <- gg40 +scale_y_continuous(breaks=seq(0,23,1)) 
gg40
gg40 <- gg40 + coord_equal()
gg40
gg40 <- gg40 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg40 <- gg40 + theme_tufte(base_family="Helvetica")
gg40 <- gg40 + theme(plot.title=element_text(hjust=0))
gg40 <- gg40 + theme(axis.ticks=element_blank())
gg40 <- gg40 + theme(axis.text=element_text(size=9))
gg40 <- gg40 + theme(legend.title=element_text(size=9))
gg40 <- gg40 + theme(legend.text=element_text(size=9))
gg40
ggsave(file="Industry_usage_40.png", plot = gg40,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w41 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==41,]
industry_data_Total_per_hour_na_removed_w41
industry_data_Total_per_hour_na_removed_w41$Hour <- hour(industry_data_Total_per_hour_na_removed_w41$mhr)
library(ggplot2)
library(lubridate)
# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg41 <-ggplot(industry_data_Total_per_hour_na_removed_w41,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg41

min(industry_data_Total_per_hour_na_removed_w41$Usage.kWh.)
# 10.19
max(industry_data_Total_per_hour_na_removed_w41$Usage.kWh.)
# 450.79
gg41 <- gg41 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,451))
gg41

library(viridis)
library(ggthemes)
gg41 <- gg41 +scale_y_continuous(breaks=seq(0,23,1)) 
gg41
gg41 <- gg41 + coord_equal()
gg41
gg41 <- gg41 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg41 <- gg41 + theme_tufte(base_family="Helvetica")
gg41 <- gg41 + theme(plot.title=element_text(hjust=0))
gg41 <- gg41 + theme(axis.ticks=element_blank())
gg41 <- gg41 + theme(axis.text=element_text(size=9))
gg41 <- gg41 + theme(legend.title=element_text(size=9))
gg41 <- gg41 + theme(legend.text=element_text(size=9))
gg41
ggsave(file="Industry_usage_41.png", plot = gg41,width=400,units="mm",limitsize = FALSE)


Total_per_hour_na_removed_w42 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==42,]
Total_per_hour_na_removed_w42
Total_per_hour_na_removed_w42$Hour <- hour(Total_per_hour_na_removed_w42$mhr)
gg42 <-ggplot(Total_per_hour_na_removed_w42,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w42$Usage.kWh.)
#11.13
max(Total_per_hour_na_removed_w42$Usage.kWh.)
#454.68
gg42 <- gg42 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(10,460))
gg42

library(ggthemes)
gg42 <- gg42 +scale_y_continuous(breaks=seq(0,23,1))
gg42 <- gg42 + coord_equal()
gg42 <- gg42 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg42 <- gg42 + theme_tufte(base_family="Helvetica")
gg42 <- gg42 + theme(plot.title=element_text(hjust=0))
gg42 <- gg42 + theme(axis.ticks=element_blank())
gg42 <- gg42 + theme(axis.text=element_text(size=9))
gg42 <- gg42 + theme(legend.title=element_text(size=9))
gg42 <- gg42 + theme(legend.text=element_text(size=9))
gg42
ggsave(file="Industry_usage_42.png", plot = gg42,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w43 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==43,]
Total_per_hour_na_removed_w43
Total_per_hour_na_removed_w43$Hour <- hour(Total_per_hour_na_removed_w43$mhr)
gg43 <-ggplot(Total_per_hour_na_removed_w43,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w43$Usage.kWh.)
# 11.02
max(Total_per_hour_na_removed_w43$Usage.kWh.)
# 438.41

gg43 <- gg43 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(10,440))

library(viridis)
library(ggthemes)

gg43 <- gg43 +scale_y_continuous(breaks=seq(0,23,1))
gg43 <- gg43 + coord_equal()
gg43 <- gg43 + labs(x="Day of Week", y="Hour of day")#, title="Appliances energy consumption")
gg43 <- gg43 + theme_tufte(base_family="Helvetica")
gg43 <- gg43 + theme(plot.title=element_text(hjust=0))
gg43 <- gg43 + theme(axis.ticks=element_blank())
gg43 <- gg43 + theme(axis.text=element_text(size=9))
gg43 <- gg43 + theme(legend.title=element_text(size=9))
gg43 <- gg43 + theme(legend.text=element_text(size=9))
gg43
ggsave(file="Industry_usage_43.png", plot = gg43,width=400,units="mm",limitsize = FALSE)

Total_per_hour_na_removed_w44 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==44,]
Total_per_hour_na_removed_w44
Total_per_hour_na_removed_w44$Hour <- hour(Total_per_hour_na_removed_w44$mhr)
gg44 <-ggplot(Total_per_hour_na_removed_w44,aes(x=Day_week,y=Hour,fill=Usage.kWh.))
min(Total_per_hour_na_removed_w44$Usage.kWh.)
# 11.08
max(Total_per_hour_na_removed_w44$Usage.kWh.)
#424.87

gg44 <- gg44 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(10,430))

library(viridis)
library(ggthemes)
gg44 <- gg44 +scale_y_continuous(breaks=seq(0,23,1))
gg44 <- gg44 + coord_equal()
gg44 <- gg44 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg44 <- gg44 + theme_tufte(base_family="Helvetica")
gg44 <- gg44 + theme(plot.title=element_text(hjust=0))
gg44 <- gg44 + theme(axis.ticks=element_blank())
gg44 <- gg44 + theme(axis.text=element_text(size=9))
gg44 <- gg44 + theme(legend.title=element_text(size=9))
gg44 <- gg44 + theme(legend.text=element_text(size=9))
gg44
ggsave(file="Industry_usage_44.png", plot = gg44,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w45 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==45,]
industry_data_Total_per_hour_na_removed_w45
industry_data_Total_per_hour_na_removed_w45$Hour <- hour(industry_data_Total_per_hour_na_removed_w45$mhr)
gg45 <-ggplot(industry_data_Total_per_hour_na_removed_w45,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg45

min(industry_data_Total_per_hour_na_removed_w45$Usage.kWh.)
# 10.69
max(industry_data_Total_per_hour_na_removed_w45$Usage.kWh.)
# 403.49
gg45 <- gg45 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,410))
gg45
gg45 <- gg45 +scale_y_continuous(breaks=seq(0,23,1)) 
gg45
gg45 <- gg45 + coord_equal()
gg45
gg45 <- gg45 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg45 <- gg45 + theme_tufte(base_family="Helvetica")
gg45 <- gg45 + theme(plot.title=element_text(hjust=0))
gg45 <- gg45 + theme(axis.ticks=element_blank())
gg45 <- gg45 + theme(axis.text=element_text(size=9))
gg45 <- gg45 + theme(legend.title=element_text(size=9))
gg45 <- gg45 + theme(legend.text=element_text(size=9))
gg45
ggsave(file="Industry_usage_45.png", plot = gg45,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w46 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==46,]
industry_data_Total_per_hour_na_removed_w46
industry_data_Total_per_hour_na_removed_w46$Hour <- hour(industry_data_Total_per_hour_na_removed_w46$mhr)
gg46 <-ggplot(industry_data_Total_per_hour_na_removed_w46,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg46
min(industry_data_Total_per_hour_na_removed_w46$Usage.kWh.)
# 10.62
max(industry_data_Total_per_hour_na_removed_w46$Usage.kWh.)
# 474
gg46 <- gg46 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,480))
gg46
gg46 <- gg46 +scale_y_continuous(breaks=seq(0,23,1)) 
gg46
gg46 <- gg46 + coord_equal()
gg46
gg46 <- gg46 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg46 <- gg46 + theme_tufte(base_family="Helvetica")
gg46 <- gg46 + theme(plot.title=element_text(hjust=0))
gg46 <- gg46 + theme(axis.ticks=element_blank())
gg46 <- gg46 + theme(axis.text=element_text(size=9))
gg46 <- gg46 + theme(legend.title=element_text(size=9))
gg46 <- gg46 + theme(legend.text=element_text(size=9))
gg46
ggsave(file="Industry_usage_46.png", plot = gg46,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w47 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==47,]
industry_data_Total_per_hour_na_removed_w47
industry_data_Total_per_hour_na_removed_w47$Hour <- hour(industry_data_Total_per_hour_na_removed_w47$mhr)
gg47 <-ggplot(industry_data_Total_per_hour_na_removed_w47,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg47

min(industry_data_Total_per_hour_na_removed_w47$Usage.kWh.)
# 11.37
max(industry_data_Total_per_hour_na_removed_w47$Usage.kWh.)
# 542.1
gg47 <- gg47 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,550))
gg47
gg47 <- gg47 +scale_y_continuous(breaks=seq(0,23,1)) 
gg47
gg47 <- gg47 + coord_equal()
gg47
gg47 <- gg47 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg47 <- gg47 + theme_tufte(base_family="Helvetica")
gg47 <- gg47 + theme(plot.title=element_text(hjust=0))
gg47 <- gg47 + theme(axis.ticks=element_blank())
gg47 <- gg47 + theme(axis.text=element_text(size=9))
gg47 <- gg47 + theme(legend.title=element_text(size=9))
gg47 <- gg47 + theme(legend.text=element_text(size=9))
gg47
ggsave(file="Industry_usage_47.png", plot = gg47,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w48 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==48,]
industry_data_Total_per_hour_na_removed_w48
industry_data_Total_per_hour_na_removed_w48$Hour <- hour(industry_data_Total_per_hour_na_removed_w48$mhr)
gg48 <-ggplot(industry_data_Total_per_hour_na_removed_w48,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg48

min(industry_data_Total_per_hour_na_removed_w48$Usage.kWh.)
# 11.51
max(industry_data_Total_per_hour_na_removed_w48$Usage.kWh.)
# 546.59
gg48 <- gg48 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(10,550))
gg48
gg48 <- gg48 +scale_y_continuous(breaks=seq(0,23,1)) 
gg48
gg48 <- gg48 + coord_equal()
gg48
gg48 <- gg48 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg48 <- gg48 + theme_tufte(base_family="Helvetica")
gg48 <- gg48 + theme(plot.title=element_text(hjust=0))
gg48 <- gg48 + theme(axis.ticks=element_blank())
gg48 <- gg48 + theme(axis.text=element_text(size=9))
gg48 <- gg48 + theme(legend.title=element_text(size=9))
gg48 <- gg48 + theme(legend.text=element_text(size=9))
gg48
ggsave(file="Industry_usage_48.png", plot = gg48,width=400,units="mm",limitsize = FALSE)



industry_data_Total_per_hour_na_removed_w49 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==49,]
industry_data_Total_per_hour_na_removed_w49
industry_data_Total_per_hour_na_removed_w49$Hour <- hour(industry_data_Total_per_hour_na_removed_w49$mhr)
gg49 <-ggplot(industry_data_Total_per_hour_na_removed_w49,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg49

min(industry_data_Total_per_hour_na_removed_w49$Usage.kWh.)
# 12.05
max(industry_data_Total_per_hour_na_removed_w49$Usage.kWh.)
# 377.93
gg49 <- gg49 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,380))
gg49
gg49 <- gg49 +scale_y_continuous(breaks=seq(0,23,1)) 
gg49
gg49 <- gg49 + coord_equal()
gg49
gg49 <- gg49 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg49 <- gg49 + theme_tufte(base_family="Helvetica")
gg49 <- gg49 + theme(plot.title=element_text(hjust=0))
gg49 <- gg49 + theme(axis.ticks=element_blank())
gg49 <- gg49 + theme(axis.text=element_text(size=9))
gg49 <- gg49 + theme(legend.title=element_text(size=9))
gg49 <- gg49 + theme(legend.text=element_text(size=9))
gg49
ggsave(file="Industry_usage_49.png", plot = gg49,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w50 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==50,]
industry_data_Total_per_hour_na_removed_w50
industry_data_Total_per_hour_na_removed_w50$Hour <- hour(industry_data_Total_per_hour_na_removed_w50$mhr)
gg50 <-ggplot(industry_data_Total_per_hour_na_removed_w50,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg50

min(industry_data_Total_per_hour_na_removed_w50$Usage.kWh.)
# 12.89
max(industry_data_Total_per_hour_na_removed_w50$Usage.kWh.)
# 465.77
gg50 <- gg50 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(12,470))
gg50
gg50 <- gg50 +scale_y_continuous(breaks=seq(0,23,1)) 
gg50
gg50 <- gg50 + coord_equal()
gg50
gg50 <- gg50 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg50 <- gg50 + theme_tufte(base_family="Helvetica")
gg50 <- gg50 + theme(plot.title=element_text(hjust=0))
gg50 <- gg50 + theme(axis.ticks=element_blank())
gg50 <- gg50 + theme(axis.text=element_text(size=9))
gg50 <- gg50 + theme(legend.title=element_text(size=9))
gg50 <- gg50 + theme(legend.text=element_text(size=9))
gg50
ggsave(file="Industry_usage_50.png", plot = gg50,width=400,units="mm",limitsize = FALSE)


industry_data_Total_per_hour_na_removed_w51 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==51,]
industry_data_Total_per_hour_na_removed_w51
industry_data_Total_per_hour_na_removed_w51$Hour <- hour(industry_data_Total_per_hour_na_removed_w51$mhr)
gg51 <-ggplot(industry_data_Total_per_hour_na_removed_w51,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg51

min(industry_data_Total_per_hour_na_removed_w51$Usage.kWh.)
# 13
max(industry_data_Total_per_hour_na_removed_w51$Usage.kWh.)
# 492.3
gg51 <- gg51 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(12,495))
gg51
gg51 <- gg51 +scale_y_continuous(breaks=seq(0,23,1)) 
gg51
gg51 <- gg51 + coord_equal()
gg51
gg51 <- gg51 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg51 <- gg51 + theme_tufte(base_family="Helvetica")
gg51 <- gg51 + theme(plot.title=element_text(hjust=0))
gg51 <- gg51 + theme(axis.ticks=element_blank())
gg51 <- gg51 + theme(axis.text=element_text(size=9))
gg51 <- gg51 + theme(legend.title=element_text(size=9))
gg51 <- gg51 + theme(legend.text=element_text(size=9))
gg51
ggsave(file="Industry_usage_51.png", plot = gg51,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w52 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==52,]
industry_data_Total_per_hour_na_removed_w52
industry_data_Total_per_hour_na_removed_w52$Hour <- hour(industry_data_Total_per_hour_na_removed_w52$mhr)
gg52 <-ggplot(industry_data_Total_per_hour_na_removed_w52,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg52

min(industry_data_Total_per_hour_na_removed_w52$Usage.kWh.)
# 11.8
max(industry_data_Total_per_hour_na_removed_w52$Usage.kWh.)
# 269
gg52 <- gg52 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,280))
gg52
gg52 <- gg52 +scale_y_continuous(breaks=seq(0,23,1)) 
gg52
gg52 <- gg52 + coord_equal()
gg52
gg52 <- gg52 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg52 <- gg52 + theme_tufte(base_family="Helvetica")
gg52 <- gg52 + theme(plot.title=element_text(hjust=0))
gg52 <- gg52 + theme(axis.ticks=element_blank())
gg52 <- gg52 + theme(axis.text=element_text(size=9))
gg52 <- gg52 + theme(legend.title=element_text(size=9))
gg52 <- gg52 + theme(legend.text=element_text(size=9))
gg52
ggsave(file="Industry_usage_52.png", plot = gg52,width=400,units="mm",limitsize = FALSE)

industry_data_Total_per_hour_na_removed_w53 <- industry_data_Total_per_hour_na_removed[industry_data_Total_per_hour_na_removed$week_year ==53,]
industry_data_Total_per_hour_na_removed_w53
industry_data_Total_per_hour_na_removed_w53$Hour <- hour(industry_data_Total_per_hour_na_removed_w53$mhr)
gg53 <-ggplot(industry_data_Total_per_hour_na_removed_w53,aes(x=Day_week,y=Hour,
                                                              fill=Usage.kWh.)) 
gg53

min(industry_data_Total_per_hour_na_removed_w53$Usage.kWh.)
# 11.98
max(industry_data_Total_per_hour_na_removed_w53$Usage.kWh.)
# 15.8
gg53 <- gg53 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(11,16))
gg53
gg53 <- gg53 +scale_y_continuous(breaks=seq(0,23,1)) 
gg53
gg53 <- gg53 + coord_equal()
gg53
gg53 <- gg53 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg53 <- gg53 + theme_tufte(base_family="Helvetica")
gg53 <- gg53 + theme(plot.title=element_text(hjust=0))
gg53 <- gg53 + theme(axis.ticks=element_blank())
gg53 <- gg53 + theme(axis.text=element_text(size=9))
gg53 <- gg53 + theme(legend.title=element_text(size=9))
gg53 <- gg53 + theme(legend.text=element_text(size=9))
gg53
ggsave(file="Industry_usage_53.png", plot = gg53,width=400,units="mm",limitsize = FALSE)



require(gridExtra)
g1 <- grid.arrange(gg1, gg2,gg3, gg4,gg5, ncol=5)

g2 <- grid.arrange(gg6, gg7,gg8, gg9,gg10, ncol=5)

g3 <- grid.arrange(gg11, gg12,gg13, gg14,gg15, ncol=5)

g4 <- grid.arrange(gg16, gg17,gg18, gg19,gg20, ncol=5)

g5 <- grid.arrange(gg21, gg22,gg23, gg24,gg25, ncol=5)

g6 <- grid.arrange(gg26, gg27,gg28, gg29,gg30, ncol=5)

g7 <- grid.arrange(gg31, gg32,gg33, gg34,gg35, ncol=5)

g8 <- grid.arrange(gg36, gg37,gg38, gg39,gg40, ncol=5)

g9 <- grid.arrange(gg41, gg42,gg43, gg44,gg45, ncol=5)

g10 <- grid.arrange(gg46, gg47,gg48, gg49,gg50, ncol=5)

g11 <- grid.arrange(gg51, gg52,gg53, ncol=5)


ggsave(file="week1-5.png", plot = g1,width=400,units="mm",limitsize = FALSE)
ggsave(file="week6-10.png", plot = g2,width=400,units="mm",limitsize = FALSE)
ggsave(file="week11-15.png", plot = g3,width=400,units="mm",limitsize = FALSE)
ggsave(file="week16-20.png", plot = g4,width=400,units="mm",limitsize = FALSE)
ggsave(file="week21-25.png", plot = g5,width=400,units="mm",limitsize = FALSE)
ggsave(file="week26-30.png", plot = g6,width=400,units="mm",limitsize = FALSE)
ggsave(file="week31-35.png", plot = g7,width=400,units="mm",limitsize = FALSE)
ggsave(file="week36-40.png", plot = g8,width=400,units="mm",limitsize = FALSE)
ggsave(file="week41-45.png", plot = g9,width=400,units="mm",limitsize = FALSE)
ggsave(file="week46-50.png", plot = g10,width=400,units="mm",limitsize = FALSE)
ggsave(file="week51-53.png", plot = g11,width=400,units="mm",limitsize = FALSE)


require(gridExtra)
industry_data$my <- NULL
industry_data$mhr <- NULL
names(industry_data)

library(caret)
set.seed(22)
train_index <- createDataPartition(industry_data$Usage.kWh.,p=0.75,list=FALSE)
train_data <- industry_data[train_index,]
dim(train_data)
write.table(format(train_data, digits=19),file = "training.csv", sep = ",", row.names=FALSE)

test_data <- industry_data[-train_index,]
dim(test_data)
write.table(format(test_data, digits=19),file = "testing.csv", sep = ",", row.names=FALSE)

# Restarting R here 
.rs.restartR()

train_data$rv1 <- sample(100, size = nrow(train_data), replace = TRUE)
train_data$rv2 <- sample(100, size = nrow(train_data), replace = TRUE)
names(train_data)
library(Boruta)

set.seed(12345)
train_data_ss<- train_data[,c(2:13)][sample(1:nrow(train_data), 7000,
                                            replace=FALSE),]
dim(train_data_ss)

set.seed(12345)
Boruta.Usage_ss    <- Boruta(Usage.kWh.~., data=train_data_ss,  doTrace = 2,ntree = 115)
Boruta.Usage_ss

print(Boruta.Usage_ss)
plot(Boruta.Usage_ss)
png('Borutaplot_Industry.png',width = 14, height = 10, units = 'in', res = 300)
png('Borutaplot.png',width = 14, height = 10, units = 'in', res = 300)
par( mar=c(7.5, 5, 2, 1)) 
plot(Boruta.Usage_ss,cex.axis=1,las=2,xlab="")
dev.off()

# Since rv1 and rv2 are not relevant, then removing them from the data sets
train_data <- subset(train_data,select=-c(rv1,rv2))

# adding dummy variables to use rfe from caret

library(dummies)
new_train_data <- dummy.data.frame(train_data,names=c("WeekStatus","Day_of_week","Load_Type"))
str(new_train_data)
dim(new_train_data)
#28033x20
new_test_data <- dummy.data.frame(test_data,names=c("WeekStatus","Day_of_week","Load_Type"))
str(new_test_data)
dim(new_test_data)
# 7007x20
# using rfe # remember that all the fields need to be numeric
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
results <- rfe(new_train_data[,c(3:20)], new_train_data[,2] , sizes=c(1:18), rfeControl=control)
results_time <- proc.time() - ptm
results$optVariables
results$optsize
results$perfNames
results
plot(results)
plot(results, type=c("g", "o"))

png('Number of variables.png',width = 8, height = 6, units = 'in', res = 300)
plot(results, type=c("g", "o"))
dev.off()

library(Metrics)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

MAE <- function(y, yhat) {
  mean(abs((y - yhat)))
}
MSE <- function(y,yhat)
{
  mean((y-yhat)**2)
}
MAPE2 <- function(y, yhat) {
  
  n <- length(y)
  su <- 0
  for (i in 1:n) {
    su <-su+ abs((y[i]-yhat[i])/y[i])
    
  }
  print (su/n)
}



MAE2 <- function(y, yhat) {
  
  n <- length(y)
  su <- 0
  for (i in 1:n) {
    su <-su+ abs((y[i]-yhat[i]))
    
  }
  print (su/n)
}

Rsqu <- function(y, yhat) {
  
  mdata <- mean(y)
  
  1 - sum((y-yhat)^2)/(sum((y-mdata)^2))
  
}

Rmse_f <- function(y, yhat) {
  
  sqrt(mean((y-yhat)^2))
  
}


MAPE(c(1,2,3,4),c(0,2,3,4))


# tRAINING THE rf MODEL
fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  # 10,3
                           verboseIter = TRUE,returnResamp = "all")
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
rf_default <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],
                    method="rf",metric='RMSE',
                    trControl=fitControl,importance = TRUE )
rf_default_time <- proc.time() - ptm
rf_default 
rf_default_time
print(rf_default)
plot(rf_default)
plot(varImp(rf_default))

library(gridBase)
library(gridExtra)
library(lattice)
x <-1:500
y <- sqrt(rf_default$finalModel$mse)
n_trees <- xyplot(y ~ x, 
                  ylab="RMSE", xlab="Number of Trees")
dev.off()
rf_g <-plot(1:500,sqrt(rf_default$finalModel$mse),type='l',col='blue',axes=TRUE,xlab="Number of Trees",ylab="RMSE")

tress_g <- plot(rf_default)
class(tress_g)
panel_plot <- grid.arrange(n_trees, tress_g,ncol=2)
ggsave(file="rf_model.png", panel_plot)


rmse(train_data$Usage.kWh.,predict(rf_default,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(rf_default,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(rf_default,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(rf_default,new_train_data[,c(2:20)]))*10
Rsqu(train_data$Usage.kWh.,predict(rf_default,new_train_data[,c(2:20)]))
Error.rate(train_data$Usage.kWh.,predict(rf_default,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(rf_default,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(rf_default,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(rf_default,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(rf_default,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(rf_default,new_test_data[,c(2:20)]))

# training LM

lmcvFit <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],  method="glm",trControl = fitControl,
                 metric='RMSE')

lmcvFit
summary(lmcvFit)
residuals<-resid(lmcvFit)
predictedValues<-predict(lmcvFit)
plot(new_train_data$Usage.kWh.,residuals,xlab="Industry Energy Consumption", ylab="Residuals")
abline(0,0)
png('lm_model.png',width = 8, height = 6, units = 'in', res = 300)
dev.off()
lmcvFit$results
plot(lmcvFit$finalModel)
plot(varImp(lmcvFit))
rmse(train_data$Usage.kWh.,predict(lmcvFit,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(lmcvFit,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(lmcvFit,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(lmcvFit,new_train_data[,c(2:20)]))*10
Rsqu(train_data$Usage.kWh.,predict(lmcvFit,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(lmcvFit,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(lmcvFit,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(lmcvFit,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(lmcvFit,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(lmcvFit,new_test_data[,c(2:20)]))


# training gbm model
fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  
                           verboseIter = TRUE,returnResamp = "all")
gbmGrid <-  expand.grid(interaction.depth = c(1,2,4),
                        n.trees = seq(100,7300,400),
                        shrinkage = 0.1,
                        n.minobsinnode = c(10))

set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
gbm_model <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],  method="gbm",
                   metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time <- proc.time() - ptm
gbm_model$call
gbm_model$bestTune
plot(gbm_model)
plot(varImp(gbm_model))
Rmse_f(train_data$Usage.kWh.,predict(gbm_model,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(gbm_model,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(gbm_model,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(gbm_model,new_train_data[,c(2:20)]))*100
Rsqu(train_data$Usage.kWh.,predict(gbm_model,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(gbm_model,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(gbm_model,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(gbm_model,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(gbm_model,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(gbm_model,new_test_data[,c(2:20)]))

# some more plots here
plot(new_train_data$Usage.kWh.,predictedValues)
lmcvFit$finalModel
varImp(lmcvFit)
length(varImp(lmcvFit))
plot(varImp(lmcvFit))



# Training SVM model

grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
svm_model <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],  method="svmRadial",
                   metric='RMSE',trControl = fitControl, preProc=c("center","scale"),
                   tuneGrid = grid)
svm_time <- proc.time() - ptm
svm_time
print(svm_model)
plot(svm_model)


grid2 <- expand.grid(sigma = c(0.1,0.25,0.30),
                     C = c( 1.1,1.5,1.8,2)
)
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()
svm_model2 <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],  method="svmRadial",
                    metric='RMSE',trControl = fitControl, preProc=c("center","scale"),
                    tuneGrid = grid2)
svm_time2 <- proc.time() - ptm
svm_time2
print(svm_model2)
plot(svm_model2)
png('svm_model.png',width = 8, height = 6, units = 'in', res = 300)
plot(svm_model2)
dev.off()


# svm 3 


grid3 <- expand.grid(sigma = c(0.35,0.4,0.1),
                     C = c(1,3.0,4.0,5.0,8.0,10,12,13)
)
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()
svm_model3 <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],method="svmRadial",
                    metric='RMSE',trControl = fitControl, preProc=c("center","scale"),
                    tuneGrid = grid3)
svm_time3 <- proc.time() - ptm
svm_time3
print(svm_model3)
plot(svm_model3)

plot(varImp(svm_model3))
Rmse_f(train_data$Usage.kWh.,predict(svm_model3,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(svm_model3,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(svm_model3,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(svm_model3,new_train_data[,c(2:20)]))*100
Rsqu(train_data$Usage.kWh.,predict(svm_model3,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(svm_model3,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(svm_model3,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(svm_model3,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(svm_model3,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(svm_model3,new_test_data[,c(2:20)]))



fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  # 10,3
                           verboseIter = TRUE,returnResamp = "all")
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
knn <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],
             method="knn",metric='RMSE',
             trControl=fitControl,importance = TRUE,preProc = c("center", "scale") )
knn_time <- proc.time() - ptm
knn 
knn_time
print(knn)
plot(knn)
plot(varImp(knn))

Rmse_f(train_data$Usage.kWh.,predict(knn,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(knn,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(knn,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(knn,new_train_data[,c(2:20)]))*100
Rsqu(train_data$Usage.kWh.,predict(knn,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(knn,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(knn,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(knn,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(knn,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(knn,new_test_data[,c(2:20)]))

library(rpart)
fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  # 10,3
                           verboseIter = TRUE,returnResamp = "all")
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
CART <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],
             method="rpart2",metric='RMSE', maxdepth=4,
             trControl=fitControl,preProc = c("center", "scale"))
CART_time <- proc.time() - ptm
CART
plot(CART)
plot(varImp(CART))
rvalues <- resamples(list(svm=svm_model3,gbm=rf_default,lm=lmcvFit))
rvalues$values
dotplot(rvalues,metric = "RMSE")
plot(varImp(CART))

Rmse_f(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))*100
Rsqu(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))

library(CHAID)


library(kernlab)
fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  # 10,3
                           verboseIter = TRUE,returnResamp = "all")
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
gauss <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],
              method="gaussprLinear",metric='RMSE',
              trControl=fitControl)
gauss_time <- proc.time() - ptm
gauss
plot(gauss)
plot(varImp(CART))
rvalues <- resamples(list(svm=svm_model3,gbm=rf_default,lm=lmcvFit))
rvalues$values
dotplot(rvalues,metric = "RMSE")
plot(varImp(CART))

Rmse_f(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))*100
Rsqu(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))


plot(varImp(rf_default))
plot(varImp(svm_model))
plot(varImp(rf_default))

rvalues <- resamples(list(SVM_Radial=rf_default,GBM=rf_default,Lm=lmcvFit,RF=rf_default))
rvalues$values

RMSE_ALL <- dotplot(rvalues,metric = "RMSE")
RSQ_ALL <- dotplot(rvalues,metric="Rsquared")
panel_plot_models <- grid.arrange(RMSE_ALL, RSQ_ALL,ncol=2)
panel_plot_models

ggsave(file="panel_plot_models2.png", panel_plot_models)

##

# Evaluating the RMSE for different models


RF_imp   <- plot(varImp(rf_default),main="RF Variable Importance")
SVM_imp  <- plot(varImp(rf_default),main="SVM Variable Importance")
gbm_imp  <- plot(varImp(rf_default),main="GBM Variable Importance")


panelVIMP_plot_models <- grid.arrange(RF_imp, gbm_imp,SVM_imp,ncol=3)

ggsave(file="panel_plot_VIMP2.png", panelVIMP_plot_models)


# 

#Now only with temperature and weather data (no lights from submetering)
names(new_train_data)

new_train_data_b <- subset(new_train_data,select=-c(lights))

str(new_train_data_b)
names(new_train_data_b)



new_test_data_b <- subset(new_test_data,select=-c(lights))
names(new_test_data_b)

#Training gbm model without light


fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  
                           verboseIter = TRUE,returnResamp = "all")

gbmGrid <-  expand.grid(interaction.depth = c(1,3,5),
                        n.trees = seq(100,10901,400),
                        shrinkage = 0.1,
                        n.minobsinnode = c(10))


set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

rf_default_no_lights <- train(Appliances~., data=new_train_data_b[,c(2:36)],  method="gbm",
                             metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time_no_lights <- proc.time() - ptm

plot(rf_default_no_lights)

rf_default_no_lights$call


dim(new_train_data_b)













gridrvm <- expand.grid(sigma = c(0.35,0.4,0.1)
)
set.seed(1)
registerDoParallel(8)
getDoParWorkers()

ptm <- proc.time()
rvm_model3 <- train(Usage.kWh.~., data=new_train_data[,c(2:20)],method="rvmRadial",
                    metric='RMSE',trControl = fitControl, preProc=c("center","scale"),
                    tuneGrid = gridrvm)
rvm_time3 <- proc.time() - ptm
rvm_time3
print(rvm_model3)
plot(rvm_model3)


library(RSNNS)
fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  # 10,3
                           verboseIter = TRUE,returnResamp = "all")
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
nnetGrid <-  expand.grid(size = seq(from = 1, to = 1000, by = 100))

mlp <- train(Usage.kWh.~., 
                 data=new_train_data[,c(2:20)],
                 method = "mlp",
                 metric = 'RMSE',
                 trControl = fitControl,
                 tuneGrid = nnetGrid,
                 lineOut= T )
dt_time <- proc.time() - ptm
dt_default 
dt_default_time
print(dt_default)
plot(dt_default)
plot(varImp(bpnn))
Rmse_f(train_data$Usage.kWh.,predict(bpnn,new_train_data[,c(2:20)]))
MAE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MSE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))
MAPE(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))*100
Rsqu(train_data$Usage.kWh.,predict(CART,new_train_data[,c(2:20)]))

Rmse_f(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MAE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MSE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))
MAPE(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))*100
Rsqu(test_data$Usage.kWh.,predict(CART,new_test_data[,c(2:20)]))





plot(varImp(lmcvFit))
plot(varImp(knn))
plot(varImp(svm_model3))
plot(varImp(rf_default))


library(caret)
library(plyr)
library(recipes)
library(dplyr)






# no weather data but with T, and RH

#Now only with temperature and weather data (no lights from submetering)
names(new_train_data)

new_train_data_c <- subset(new_train_data,select=-c(lights,T_out,
                                                    Press_mm_hg,RH_out,
                                                    Windspeed,Visibility,
                                                    Tdewpoint))

str(new_train_data_c)
names(new_train_data_c)



new_test_data_c <- subset(new_test_data,select=-c(lights,T_out,
                                                  Press_mm_hg,RH_out,
                                                  Windspeed,Visibility,
                                                  Tdewpoint))
names(new_test_data_c)

# lightst and weather data , no T and Humidity
names(new_train_data)

new_train_data_d <- subset(new_train_data,select=-c(T1,RH_1,T2,RH_2,
                                                    T3,RH_3,T4,RH_4,
                                                    T5,RH_5,T6,RH_6,
                                                    T7,RH_7,T8,RH_8,
                                                    T9,RH_9))

str(new_train_data_d)
names(new_train_data_d)



new_test_data_d <- subset(new_test_data,select=-c(T1,RH_1,T2,RH_2,
                                                  T3,RH_3,T4,RH_4,
                                                  T5,RH_5,T6,RH_6,
                                                  T7,RH_7,T8,RH_8,
                                                  T9,RH_9))

names(new_test_data_d)

names(new_test_data)


new_train_data_e <- subset(new_train_data,select=-c(T1,RH_1,T2,RH_2,
                                                    T3,RH_3,T4,RH_4,
                                                    T5,RH_5,T6,RH_6,
                                                    T7,RH_7,T8,RH_8,
                                                    T9,RH_9,lights))


new_test_data_e <- subset(new_test_data,select=-c(T1,RH_1,T2,RH_2,
                                                  T3,RH_3,T4,RH_4,
                                                  T5,RH_5,T6,RH_6,
                                                  T7,RH_7,T8,RH_8,
                                                  T9,RH_9,lights))

names(new_train_data_e)
names(new_test_data_e)





### training with c and d data sets

set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

rf_default_no_lights_no_weather <- train(Appliances~., data=new_train_data_c[,c(2:30)],  method="gbm",
                                        metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time_no_lights_no_weather <- proc.time() - ptm

plot(rf_default_no_lights_no_weather)


names(new_train_data_c)
dim(new_train_data_c)

# d data set
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

rf_default_lights_weather <- train(Appliances~., data=new_train_data_d[,c(2:19)],  method="gbm",
                                  metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time_lights_weather <- proc.time() - ptm

plot(rf_default_lights_weather)


names(new_train_data_d)
dim(new_train_data_e)


#e data set
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

rf_default_weather <- train(Appliances~., data=new_train_data_e[,c(2:18)],  method="gbm",
                           metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
rf_default_time_weather <- proc.time() - ptm

plot(rf_default_weather)

rf_default_weather$coefnames




m1 <- plot(varImp(rf_default_lights_weather),main="GBM Lght & Weath")
m2 <- plot(varImp(rf_default_no_lights_no_weather),main="GBM NO Lght & NO Weath")
m3 <- plot(varImp(rf_default_no_lights),main="GBM NO Lght")
m4 <- plot(varImp(rf_default_weather),main="GBM Only Weath")


panelVIMP_plots_2 <- grid.arrange(m1,m2,m3,m4,ncol=4)

ggsave(file="panel_plot_VIMP_data_subsets.png", 
       panelVIMP_plots_2,
       width=14,height=8)


rf_default_no_lights$coefnames

rf_default_no_lights_no_weather$coefnames
rf_default_lights_weather$coefnames
rf_default_weather$coefnames


rvalues2 <- resamples(list(GBM_NO_LIGHTS=rf_default_no_lights,
                           GBM_NO_LIGHTS_NO_WEATHER=rf_default_no_lights_no_weather,
                           GBM_LIGHTS_WEATHER=rf_default_lights_weather,
                           GBM_WEATHER=rf_default_weather))

dotplot(rvalues2,metric="Rsquared")
dotplot(rvalues2,metric="RMSE")



dotplot(rvalues2)



RMSE_GBM_subsets <- dotplot(rvalues2,metric = "RMSE")
RSQ_GBM_Subsets <- dotplot(rvalues2,metric="Rsquared")

panel_plot_GBMmodels_confidence <- grid.arrange(RMSE_GBM_subsets , RSQ_GBM_Subsets,ncol=2)
panel_plot_GBMmodels_confidence

ggsave(file="panel_plot_rf_defaults.png", panel_plot_GBMmodels_confidence,
       width=14,height=8)



# Testing in the training and test sets RMSE
#gbm models with no lights
set.seed(1)
rmse(new_train_data_b$Appliances,predict(rf_default_no_lights,new_train_data_b))
# 17.9028

set.seed(1)
MAE(new_train_data_b$Appliances,predict(rf_default_no_lights,new_train_data_b))
#12.24

set.seed(1)
MAPE(new_train_data_b$Appliances,predict(rf_default_no_lights,new_train_data_b))*100
# 16.66

set.seed(1)
Rsqu(new_train_data_b$Appliances,predict(rf_default_no_lights,new_train_data_b))


set.seed(1)
rmse(new_test_data_b$Appliances,predict(rf_default_no_lights,new_test_data_b))
#66.2134

set.seed(1)
MAE(new_test_data_b$Appliances,predict(rf_default_no_lights,new_test_data_b))
# 35.25


set.seed(1)
MAPE(new_test_data_b$Appliances,predict(rf_default_no_lights,new_test_data_b))*100

set.seed(1)
Rsqu(new_test_data_b$Appliances,predict(rf_default_no_lights,new_test_data_b))
#0.575


#rf_default_no_lights_no_weather
set.seed(1)
rmse(new_train_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_train_data_c))
# 18.83

set.seed(1)
MAE(new_train_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_train_data_c))
# 12.85

set.seed(1)
MAPE(new_train_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_train_data_c))*100
# 17.44

set.seed(1)
Rsqu(new_train_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_train_data_c))
#0.97


set.seed(1)
rmse(new_test_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_test_data_c))
# 68.59

set.seed(1)
MAE(new_test_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_test_data_c))
# 36.21

set.seed(1)
MAPE(new_test_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_test_data_c))*100
#39.23
set.seed(1)
Rsqu(new_test_data_c$Appliances,predict(rf_default_no_lights_no_weather,new_test_data_c))


# rf_default_lights_weather
set.seed(1)
rmse(new_train_data_d$Appliances,predict(rf_default_lights_weather,new_train_data_d))
#27.47
set.seed(1)
MAE(new_train_data_d$Appliances,predict(rf_default_lights_weather,new_train_data_d))
# 18.29

set.seed(1)
MAPE(new_train_data_d$Appliances,predict(rf_default_lights_weather,new_train_data_d))*100
# 23.71
set.seed(1)
Rsqu(new_train_data_d$Appliances,predict(rf_default_lights_weather,new_train_data_d))
# 0.93


set.seed(1)
rmse(new_test_data_d$Appliances,predict(rf_default_lights_weather,new_test_data_d))
#72.64

set.seed(1)
MAE(new_test_data_d$Appliances,predict(rf_default_lights_weather,new_test_data_d))
# 40.32

set.seed(1)
MAPE(new_test_data_d$Appliances,predict(rf_default_lights_weather,new_test_data_d))*100

set.seed(1)
Rsqu(new_test_data_d$Appliances,predict(rf_default_lights_weather,new_test_data_d))
# 0.49



# rf_default_weather
set.seed(1)
rmse(new_train_data_e$Appliances,predict(rf_default_weather,new_train_data_e))
# 28.29
set.seed(1)
MAE(new_train_data_e$Appliances,predict(rf_default_weather,new_train_data_e))
# 18.85


set.seed(1)
MAPE(new_train_data_e$Appliances,predict(rf_default_weather,new_train_data_e))*100
# 24.41198

set.seed(1)
Rsqu(new_train_data_e$Appliances,predict(rf_default_weather,new_train_data_e))
# 0.92


set.seed(1)
rmse(new_test_data_e$Appliances,predict(rf_default_weather,new_test_data_e))
# 72.45

set.seed(1)
MAE(new_test_data_e$Appliances,predict(rf_default_weather,new_test_data_e))
# 40.72


set.seed(1)
MAPE(new_test_data_e$Appliances,predict(rf_default_weather,new_test_data_e))*100
# 46.53

set.seed(1)
Rsqu(new_test_data_e$Appliances,predict(rf_default_weather,new_test_data_e))
# 0.49



