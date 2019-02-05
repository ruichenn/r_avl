library(tidyverse)
library(ggplot2)
library(shiny)
library(devtools)
library(maps)
library(socviz)
library(lubridate)
library(data.table)
library(dplyr)
library(reshape2)
data1 <- read.csv("javaproject/AVL Data/Mt Auburn St/r71_in.csv")
data2 <- read.csv("javaproject/AVL Data/Mt Auburn St/r71_out.csv")
data3 <- read.csv("javaproject/AVL Data/Mt Auburn St/r73_in.csv")
data4 <- read.csv("javaproject/AVL Data/Mt Auburn St/r73_out.csv")
data5 <- read.csv("javaproject/AVL Data/South Mass Ave/r1_in.csv")
data6 <- read.csv("javaproject/AVL Data/South Mass Ave/r1_out.csv")

View(data1)
View(data2)
data1 <- as.tibble(data1)
str(data1)
data2 <- as.tibble(data2)
str(data2)
data3 <- as.tibble(data3)
str(data3)
data4 <- as.tibble(data4)
str(data4)
data5 <- as.tibble(data5)
str(data5)
data6 <- as.tibble(data6)
str(data6)
#the change in median travel times on the corridors, followed closely by the change in travel time reliability
# the spread in the distribution of the travel times by period



# route 71 inbound corridor
# converting & travel time
data1$X2062 <- as.POSIXct(data1$X2062, format = "%Y-%m-%d %H:%M:%S")
data1$X2076 <- as.POSIXct(data1$X2076, format = "%Y-%m-%d %H:%M:%S")
data1$X2066 <- as.POSIXct(data1$X2066, format = "%Y-%m-%d %H:%M:%S")
data1$X2068 <- as.POSIXct(data1$X2068, format = "%Y-%m-%d %H:%M:%S")
data1$c_traveltime <- difftime(data1$X2076,data1$X2062,tz="EST",units='secs')
data1$c_traveltime <- abs(as.numeric(data1$c_traveltime))


# get time cutoffs
data1$time <- strftime(data1$X2062, format = "%H:%M:%S") 
data1$time <- as.POSIXct(data1$time,format = "%H:%M:%S")
am_bb <- as.POSIXct("07:30:00",format = "%H:%M:%S")
am_be <- as.POSIXct("09:30:00",format = "%H:%M:%S" )
data1$peak[am_bb<=data1$time & data1$time <= am_be] <- "AMPeak"
mid_b <- as.POSIXct("12:00:00",format = "%H:%M:%S")
mid_e <- as.POSIXct("14:00:00",format = "%H:%M:%S")
data1$peak[mid_b<=data1$time & data1$time <= mid_e] <- "Midday"
pm_b <- as.POSIXct("16:30:00",format = "%H:%M:%S")
pm_e <- as.POSIXct("18:30:00",format = "%H:%M:%S")
data1$peak[pm_b<=data1$time & data1$time <= pm_e] <- "PMPeak"

#median travel time 
#spread reliability
data1 %>% filter(prepostm == "0")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "0")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "0")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "1")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "1")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "1")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
#plotting ok
data1$prepostm <- as.character(data1$prepostm)
class(data1$prepostm)
p71in <- data1 %>% ggplot(aes(x=peak,y=c_traveltime,fill= prepostm))
p71in + geom_boxplot(
                     position = position_dodge(1),notch = FALSE)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text = element_text(size = 11, family = "Helvetica"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 8))+
ggtitle("Route 71 Inbound:corridor travel time per trip")+
scale_y_continuous(name = "corridor travel time (secs)", limits=c(200, 3100),breaks = seq(200, 3100, 200))+
scale_x_discrete(name = "time of day")+
  labs(fill="0= before implementation, 1= after implementation")
  

# route 71 inbound stop pairs
data1$p_traveltime <- difftime(data1$X2068,data1$X2062,tz="EST",units='secs')
data1$p_traveltime <- abs(data1$p_traveltime)
d10 <- data1 %>% melt(data1,id.vars='prepostm', measure.vars=c('p_traveltime'),na.rm = TRUE)
d10$value <- abs(d10$value)
#median travel time
r71in <- data1 %>% group_by(prepostm) %>% summarise(median(p_traveltime,na.rm = TRUE))
View(r71in)
summary(d11)
#spread reliability
data1 %>% filter(prepostm == "0") %>% summarise(max(p_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "0") %>% summarise(min(p_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "1") %>% summarise(max(p_traveltime,na.rm = TRUE))
data1 %>% filter(prepostm == "1") %>% summarise(min(p_traveltime,na.rm = TRUE))


# route 71 outbound corridor
# converting & travel time
data2$X2076 <- as.POSIXct(data2$X2076, format = "%Y-%m-%d %H:%M:%S")
data2$X2026 <- as.POSIXct(data2$X2026, format = "%Y-%m-%d %H:%M:%S")
data2$X2028 <- as.POSIXct(data2$X2028, format = "%Y-%m-%d %H:%M:%S")
data2$X2032 <- as.POSIXct(data2$X2032, format = "%Y-%m-%d %H:%M:%S")
data2$c_traveltime <- difftime(data2$X2032,data2$X2076,tz="EST",units='secs')
data2$c_traveltime <- abs(as.numeric(data2$c_traveltime))

# get time cutoffs
data2$time <- strftime(data2$X2076, format = "%H:%M:%S") 
data2$time <- as.POSIXct(data2$time,format = "%H:%M:%S")
data2$peak[am_bb<=data2$time & data2$time <= am_be] <- "AMPeak"
data2$peak[mid_b<=data2$time & data2$time <= mid_e] <- "Midday"
data2$peak[pm_b<=data2$time & data2$time <= pm_e] <- "PMPeak"

#median travel time 
#spread reliability
data2 %>% filter(prepostm == "0")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "0")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "0")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "1")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "1")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "1")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
#plotting ok
data2$prepostm <- as.character(data2$prepostm)
class(data2$prepostm)
p71o <- data2 %>% ggplot(aes(x=peak,y=c_traveltime,fill= prepostm))
p71o + geom_boxplot(
  position = position_dodge(1),notch = FALSE)+ 
  theme_minimal()+
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text = element_text(size = 11, family = "Helvetica"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 8))+
  ggtitle("Route 71 Outbound:corridor travel time per trip")+
  scale_y_continuous(name = "corridor travel time (secs)", limits=c(200, 2600),breaks = seq(200, 2600, 200))+
  scale_x_discrete(name = "time of day")+
  labs(fill="0= before implementation, 1= after implementation")



# route 71 outbound stop pairs
data2$p_traveltime <- difftime(data2$X2026,data2$X2032,tz="EST",units='secs')
data2$p_traveltime <- abs(data2$p_traveltime)
#median travel time
r71out <- data2 %>% group_by(prepostm) %>% summarise(median(p_traveltime,na.rm = TRUE))
View(r71out)
summary(d11)
#spread reliability
data2 %>% filter(prepostm == "0") %>% summarise(max(p_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "0") %>% summarise(min(p_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "1") %>% summarise(max(p_traveltime,na.rm = TRUE))
data2 %>% filter(prepostm == "1") %>% summarise(min(p_traveltime,na.rm = TRUE))



# route 73 inbound corridor
# converting & travel time
data3$X2117 <- as.POSIXct(data3$X2117, format = "%Y-%m-%d %H:%M:%S")
data3$X2066 <- as.POSIXct(data3$X2066, format = "%Y-%m-%d %H:%M:%S")
data3$X2068 <- as.POSIXct(data3$X2068, format = "%Y-%m-%d %H:%M:%S")
data3$X2076 <- as.POSIXct(data3$X2076, format = "%Y-%m-%d %H:%M:%S")
data3$c_traveltime <- difftime(data3$X2117,data3$X2076,tz="EST",units='secs')
data3$c_traveltime <- abs(as.numeric(data3$c_traveltime))

# get time cutoffs
data3$time <- strftime(data3$X2117, format = "%H:%M:%S") 
data3$time <- as.POSIXct(data3$time,format = "%H:%M:%S")
data3$peak[am_bb<=data3$time & data3$time <= am_be] <- "AMPeak"
data3$peak[mid_b<=data3$time & data3$time <= mid_e] <- "Midday"
data3$peak[pm_b<=data3$time & data3$time <= pm_e] <- "PMPeak"

#median travel time 
#spread reliability
data3 %>% filter(prepostm == "0")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "0")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "0")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "1")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "1")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "1")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
#plotting ok
data3$prepostm <- as.character(data3$prepostm)
class(data3$prepostm)
p73i <- data3 %>% ggplot(aes(x=peak,y=c_traveltime,fill= prepostm))
p73i + geom_boxplot(
  position = position_dodge(1),notch = FALSE)+ 
  theme_minimal()+
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text = element_text(size = 11, family = "Helvetica"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 8))+
  ggtitle("Route 73 Inbound:corridor travel time per trip")+
  scale_y_continuous(name = "corridor travel time (secs)", limits=c(200, 3600),breaks = seq(200, 3600, 200))+
  scale_x_discrete(name = "time of day")+
  labs(fill="0= before implementation, 1= after implementation")

# route 73 inbound stop pairs
data3$p_traveltime <- difftime(data3$X2117,data3$X2068,tz="EST",units='secs')
data3$p_traveltime <- abs(data3$p_traveltime)
#median travel time
r73i <- data3 %>% group_by(prepostm) %>% summarise(median(p_traveltime,na.rm = TRUE))
View(r73i)
#spread reliability
data3 %>% filter(prepostm == "0") %>% summarise(max(p_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "0") %>% summarise(min(p_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "1") %>% summarise(max(p_traveltime,na.rm = TRUE))
data3 %>% filter(prepostm == "1") %>% summarise(min(p_traveltime,na.rm = TRUE))

# route 73 outbound corridor
# converting & travel time
data4$X2118 <- as.POSIXct(data4$X2118, format = "%Y-%m-%d %H:%M:%S")
data4$X2026 <- as.POSIXct(data4$X2026, format = "%Y-%m-%d %H:%M:%S")
data4$X2028 <- as.POSIXct(data4$X2028, format = "%Y-%m-%d %H:%M:%S")
data4$X2076 <- as.POSIXct(data4$X2076, format = "%Y-%m-%d %H:%M:%S")
data4$c_traveltime <- difftime(data4$X2076,data4$X2118,tz="EST",units='secs')
data4$c_traveltime <- abs(as.numeric(data4$c_traveltime))

# get time cutoffs
data4$time <- strftime(data4$X2076, format = "%H:%M:%S") 
data4$time <- as.POSIXct(data4$time,format = "%H:%M:%S")
data4$peak[am_bb<=data4$time & data4$time <= am_be] <- "AMPeak"
data4$peak[mid_b<=data4$time & data4$time <= mid_e] <- "Midday"
data4$peak[pm_b<=data4$time & data4$time <= pm_e] <- "PMPeak"

#median travel time 
#spread reliability
data4 %>% filter(prepostm == "0")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "0")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "0")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "1")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "1")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "1")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
#plotting ok
data4$prepostm <- as.character(data4$prepostm)
class(data4$prepostm)
p73o <- data4 %>% ggplot(aes(x=peak,y=c_traveltime,fill= prepostm))
p73o + geom_boxplot(
  position = position_dodge(1),notch = FALSE)+ 
  theme_minimal()+
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text = element_text(size = 11, family = "Helvetica"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 8))+
  ggtitle("Route 73 Outbound:corridor travel time per trip")+
  scale_y_continuous(name = "corridor travel time (secs)", limits=c(200, 1700),breaks = seq(200, 1700, 200))+
  scale_x_discrete(name = "time of day")+
  labs(fill="0= before implementation, 1= after implementation")



# route 73 outbound stop pairs
data4$p_traveltime <- difftime(data4$X2026,data4$X2118,tz="EST",units='secs')
data4$p_traveltime <- abs(data4$p_traveltime)
#median travel time
r73o <- data4 %>% group_by(prepostm) %>% summarise(median(p_traveltime,na.rm = TRUE))
View(r73o)
#spread reliability
data4 %>% filter(prepostm == "0") %>% summarise(max(p_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "0") %>% summarise(min(p_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "1") %>% summarise(max(p_traveltime,na.rm = TRUE))
data4 %>% filter(prepostm == "1") %>% summarise(min(p_traveltime,na.rm = TRUE))

# route 1 inbound corridor
# converting & travel time
data5$X72 <- as.POSIXct(data5$X72, format = "%Y-%m-%d %H:%M:%S")
data5$X73 <- as.POSIXct(data5$X73, format = "%Y-%m-%d %H:%M:%S")
data5$X75 <- as.POSIXct(data5$X75, format = "%Y-%m-%d %H:%M:%S")
data5$X77 <- as.POSIXct(data5$X77, format = "%Y-%m-%d %H:%M:%S")
data5$c_traveltime <- difftime(data5$X72,data5$X77,tz="EST",units='secs')
data5$c_traveltime <- abs(as.numeric(data5$c_traveltime))

# get time cutoffs
data5$time <- strftime(data5$X72, format = "%H:%M:%S") 
data5$time <- as.POSIXct(data5$time,format = "%H:%M:%S")
data5$peak[am_bb<=data5$time & data5$time <= am_be] <- "AMPeak"
data5$peak[mid_b<=data5$time & data5$time <= mid_e] <- "Midday"
data5$peak[pm_b<=data5$time & data5$time <= pm_e] <- "PMPeak"

#median travel time 
#spread reliability
data5 %>% filter(prepostm == "0")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "0")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "0")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "1")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "1")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "1")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
#plotting ok
data5$prepostm <- as.character(data5$prepostm)
class(data5$prepostm)
p1i <- data5 %>% ggplot(aes(x=peak,y=c_traveltime,fill= prepostm))
p1i + geom_boxplot(
  position = position_dodge(1),notch = FALSE)+ 
  theme_minimal()+
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text = element_text(size = 11, family = "Helvetica"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 8))+
  ggtitle("Route 1 Inbound:corridor travel time per trip")+
  scale_y_continuous(name = "corridor travel time (secs)", limits=c(300, 3900),breaks = seq(300, 3900, 300))+
  scale_x_discrete(name = "time of day")+
  labs(fill="0= before implementation, 1= after implementation")

# route 1 inbound stop pairs
data5$p_traveltime <- difftime(data5$X73,data5$X77,tz="EST",units='secs')
data5$p_traveltime <- abs(data5$p_traveltime)
#median travel time
r1i <- data5 %>% group_by(prepostm) %>% summarise(median(p_traveltime,na.rm = TRUE))
View(r1i)
#spread reliability
data5 %>% filter(prepostm == "0") %>% summarise(max(p_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "0") %>% summarise(min(p_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "1") %>% summarise(max(p_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "1") %>% summarise(min(p_traveltime,na.rm = TRUE))


View(data6)
# route 1 outbound corridor
# converting & travel time
data6$X95 <- as.POSIXct(data6$X95, format = "%Y-%m-%d %H:%M:%S")
data6$X97 <- as.POSIXct(data6$X97, format = "%Y-%m-%d %H:%M:%S")
data6$X101 <- as.POSIXct(data6$X101, format = "%Y-%m-%d %H:%M:%S")
data6$X102 <- as.POSIXct(data6$X102, format = "%Y-%m-%d %H:%M:%S")
data6$c_traveltime <- difftime(data6$X95,data6$X102,tz="EST",units='secs')
data6$c_traveltime <- abs(as.numeric(data6$c_traveltime))

# get time cutoffs
data6$time <- strftime(data6$X95, format = "%H:%M:%S") 
data6$time <- as.POSIXct(data6$time,format = "%H:%M:%S")
data6$peak[am_bb<=data6$time & data6$time <= am_be] <- "AMPeak"
data6$peak[mid_b<=data6$time & data6$time <= mid_e] <- "Midday"
data6$peak[pm_b<=data6$time & data6$time <= pm_e] <- "PMPeak"

#median travel time 
#spread reliability
data6 %>% filter(prepostm == "0")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data6 %>% filter(prepostm == "0")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data6 %>% filter(prepostm == "0")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data6 %>% filter(prepostm == "1")%>% filter(peak =="AMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data6 %>% filter(prepostm == "1")%>% filter(peak =="Midday") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime,na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
data6 %>% filter(prepostm == "1")%>% filter(peak =="PMPeak") %>% 
  summarise(median=median(c_traveltime, na.rm = TRUE), 
            min=min(c_traveltime, na.rm = TRUE), max=max(c_traveltime,na.rm = TRUE))
#plotting ok
data6$prepostm <- as.character(data6$prepostm)
class(data6$prepostm)
p1o <- data6 %>% ggplot(aes(x=peak,y=c_traveltime,fill= prepostm))
p1o + geom_boxplot(
  position = position_dodge(1),notch = FALSE)+ 
  theme_minimal()+
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text = element_text(size = 11, family = "Helvetica"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 8))+
  ggtitle("Route 1 Outbound:corridor travel time per trip")+
  scale_y_continuous(name = "corridor travel time (secs)", limits=c(300, 3900),breaks = seq(300, 3900, 300))+
  scale_x_discrete(name = "time of day")+
  labs(fill="0= before implementation, 1= after implementation")

# route 1 outbound stop pairs
data6$p_traveltime <- difftime(data6$X95,data6$X101,tz="EST",units='secs')
data6$p_traveltime <- abs(data6$p_traveltime)
#median travel time
r1out <- data6 %>% group_by(prepostm) %>% summarise(median(p_traveltime,na.rm = TRUE))
View(r1out)
#spread reliability
data6 %>% filter(prepostm == "0") %>% summarise(max(p_traveltime,na.rm = TRUE))
data6 %>% filter(prepostm == "0") %>% summarise(min(p_traveltime,na.rm = TRUE))
data6 %>% filter(prepostm == "1") %>% summarise(max(p_traveltime,na.rm = TRUE))
data5 %>% filter(prepostm == "1") %>% summarise(min(p_traveltime,na.rm = TRUE))


