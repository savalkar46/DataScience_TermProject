### Codes used for plotting and analysis
### Network Maps
library(tidyverse)
library(sna)
library(maps)
library(ggrepel)
library(ggnetwork)

RCP45 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/RCP_8.5_Analog_Count_Map.csv")
usa_map <- map_data('state') 
options(ggrepel.max.overlaps = 0)

RCP45_1 <- RCP45 |> filter(RCP45$Nearest_ID==1)
myMap1 <- ggplot() +
  # Plot map
  geom_map(data = usa_map, map = usa_map, aes(map_id = region),
           color = 'gray15',
           fill = 'white')+ 
  geom_segment(data = RCP45_1, alpha = 0.5,color = "#EC7014",
               aes(x = Lon.from, y = Lat.from,
                   xend = Lon.to , yend = Lat.to), arrow = arrow(length = unit(0.5, "cm")))+
  scale_size(range = c(1))+
  xlim(c(-125, -68)) + ylim(c(25, 50)) +
  geom_point(data = RCP45_1,alpha = 0.5,
             aes(x = Lon.from,
                 y = Lat.from,
                 size = Nearest_ID,
                 colour = State_County),size=6)+
  geom_point(data = RCP45_1,
             aes(x = Lon.to,
                 y = Lat.to,
                 colour = State_County),size=6)+
  geom_text_repel(data = RCP45_1 %>% 
                    select(State_County, 
                           Lon.from, 
                           Lat.from) %>% 
                    unique,
                  colour = 'dodgerblue1',
                  aes(x = Lon.from, y = Lat.from, label = State_County)) + 
  coord_equal() +
  theme_bw()
myMap1

RCP45_2 <- RCP45 |> filter(RCP45$Nearest_ID==2)
myMap2 <- ggplot() +
  # Plot map
  geom_map(data = usa_map, map = usa_map, aes(map_id = region),
           color = 'gray15',
           fill = 'white')+ 
  geom_segment(data = RCP45_2, alpha = 0.5,color = "black",
               aes(x = Lon.from, y = Lat.from,
                   xend = Lon.to , yend = Lat.to,
                   colour= "#EC7014",
                   size = Nearest_ID), arrow = arrow(length = unit(0.25, "cm"))) +scale_size(range = c(1))+
  xlim(c(-125, -68)) + ylim(c(25, 50)) +
  geom_point(data = RCP45_2,
             aes(x = Lon.from,
                 y = Lat.from,
                 size = Nearest_ID,
                 colour = State_County),size=5)+
  geom_point(data = RCP45_2,
             aes(x = Lon.to,
                 y = Lat.to,
                 colour = State_County),size=5)+
  geom_text_repel(data = RCP45_2 %>% 
                    select(State_County, 
                           Lon.from, 
                           Lat.from) %>% 
                    unique,
                  colour = 'dodgerblue1',
                  aes(x = Lon.from, y = Lat.from, label = State_County)) + 
  coord_equal() +
  theme_bw()
myMap2

RCP45_3 <- RCP45 |> filter(RCP45$Nearest_ID==3)
myMap3 <- ggplot() +
  # Plot map
  geom_map(data = usa_map, map = usa_map, aes(map_id = region),
           color = 'gray15',
           fill = 'white')+ 
  geom_segment(data = RCP45_3, alpha = 0.5,color = "black",
               aes(x = Lon.from, y = Lat.from,
                   xend = Lon.to , yend = Lat.to,
                   colour= "#EC7014",
                   size = Nearest_ID), arrow = arrow(length = unit(0.25, "cm"))) +scale_size(range = c(1))+
  xlim(c(-125, -68)) + ylim(c(25, 50)) +
  geom_point(data = RCP45_3,
             aes(x = Lon.from,
                 y = Lat.from,
                 size = Nearest_ID,
                 colour = State_County),size=4)+
  geom_point(data = RCP45_3,
             aes(x = Lon.to,
                 y = Lat.to,
                 colour = State_County),size=4)+
  geom_text_repel(data = RCP45_3 %>% 
                    select(State_County, 
                           Lon.from, 
                           Lat.from) %>% 
                    unique,
                  colour = 'dodgerblue1',
                  aes(x = Lon.from, y = Lat.from, label = State_County)) + 
  coord_equal() +
  theme_bw()
myMap3

RCP45_4 <- RCP45 |> filter(RCP45$Nearest_ID==4)
myMap4 <- ggplot() +
  # Plot map
  geom_map(data = usa_map, map = usa_map, aes(map_id = region),
           color = 'gray15',
           fill = 'white')+ 
  geom_segment(data = RCP45_4, alpha = 0.5,color = "black",
               aes(x = Lon.from, y = Lat.from,
                   xend = Lon.to , yend = Lat.to,
                   colour= "#EC7014",
                   size = Nearest_ID), arrow = arrow(length = unit(0.25, "cm"))) +scale_size(range = c(1))+
  xlim(c(-125, -68)) + ylim(c(25, 50)) +
  geom_point(data = RCP45_4,
             aes(x = Lon.from,
                 y = Lat.from,
                 size = Nearest_ID,
                 colour = State_County),size=3)+
  geom_point(data = RCP45_4,
             aes(x = Lon.to,
                 y = Lat.to,
                 colour = State_County),size=3)+
  geom_text_repel(data = RCP45_4 %>% 
                    select(State_County, 
                           Lon.from, 
                           Lat.from) %>% 
                    unique,
                  colour = 'dodgerblue1',
                  aes(x = Lon.from, y = Lat.from, label = State_County)) + 
  coord_equal() +
  theme_bw()
myMap4

RCP45_5 <- RCP45 |> filter(RCP45$Nearest_ID==5)
myMap5 <- ggplot() +
  # Plot map
  geom_map(data = usa_map, map = usa_map, aes(map_id = region),
           color = 'gray15',
           fill = 'white')+ 
  geom_segment(data = RCP45_5, alpha = 0.5,color = "black",
               aes(x = Lon.from, y = Lat.from,
                   xend = Lon.to , yend = Lat.to,
                   colour= "#EC7014",
                   size = Nearest_ID), arrow = arrow(length = unit(0.25, "cm"))) +scale_size(range = c(1))+
  xlim(c(-125, -68)) + ylim(c(25, 50)) +
  geom_point(data = RCP45_5,
             aes(x = Lon.from,
                 y = Lat.from,
                 size = Nearest_ID,
                 colour = State_County),size=2)+
  geom_point(data = RCP45_5,
             aes(x = Lon.to,
                 y = Lat.to,
                 colour = State_County),size=2)+
  geom_text_repel(data = RCP45_5 %>% 
                    select(State_County, 
                           Lon.from, 
                           Lat.from) %>% 
                    unique,
                  colour = 'dodgerblue1',
                  aes(x = Lon.from, y = Lat.from, label = State_County)) + 
  coord_equal() +
  theme_bw()
myMap5


myMap <- ggplot() +
  # Plot map
  geom_map(data = usa_map, map = usa_map, aes(map_id = region),
           color = 'gray15',
           fill = 'white')+ 
  geom_segment(data = RCP45, alpha = 1,color = "blue",
               aes(x = Lon.from, y = Lat.from,
                   xend = Lon.to , yend = Lat.to), arrow = arrow(length = unit(0.1, "cm")))+
  scale_size(range = c(1))+
  xlim(c(-125, -68)) + ylim(c(25, 50)) +
  geom_point(data = RCP45,alpha = 0.5,
             aes(x = Lon.from,
                 y = Lat.from,
                 size = Nearest_ID,
                 colour = State_County),size=2)+
  geom_point(data = RCP45,
             aes(x = Lon.to,
                 y = Lat.to,
                 colour = State_County),size=2)+
  geom_text_repel(data = RCP45 %>% 
                    select(State_County, 
                           Lon.from, 
                           Lat.from) %>% 
                    unique,
                  colour = 'dodgerblue1',
                  aes(x = Lon.from, y = Lat.from, label = State_County)) + 
  coord_equal() + labs(x="Latitude", y="Longitude")+
  theme_bw()+facet_wrap(~Nearest_ID)+  ggtitle("Future of county under consideration for RCP8.5")

#### Violin Plots

library(RColorBrewer)
library(tidyverse)
library(ggplot2)
library(reshape2)
West_Future<-Data1 |> filter(State_Coun %in%c("ID_Canyon ","ID_Bingham ","ID_Bonneville ","ID_Twin Falls ","ID_Power ",
                                              "ID_Cassia ","ID_Jefferson ","ID_Jerome ","ID_Madison ","ID_Minidoka ",
                                              "OR_Baker ","OR_Harney ","OR_Klamath ","OR_Lake ","OR_Malheur ","OR_Marion ",
                                              "OR_Morrow ","OR_Sherman ","OR_Umatilla ","OR_Union ","WA_Adams ","WA_Benton ",
                                              "WA_Douglas ","WA_Franklin ","WA_Grant ","WA_Lincoln ","WA_Spokane ","WA_Walla Walla ",
                                              "WA_Whitman ","WA_Yakima "),year%in%c(2040:2070), climate_proj%in%c("rcp45"))

K1 <- West_Future |> group_by(year,model,State_Coun) |> summarise(sum(frostfree_day))
colnames(K1)<- c("year","model","State_Coun", "Future")

K2 <- K1 |> group_by(year,State_Coun) |> summarise(mean(Future))
colnames(K2)<- c("year","State_Coun", "Future")

### From Gridmet data C matrix
Data2 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Cmatrix_90_20_v6.csv")
West_Hist<-Data2 |> filter(State_Coun %in%c("ID_Canyon ","ID_Bingham ","ID_Bonneville ","ID_Twin Falls ","ID_Power ",
                                            "ID_Cassia ","ID_Jefferson ","ID_Jerome ","ID_Madison ","ID_Minidoka ",
                                            "OR_Baker ","OR_Harney ","OR_Klamath ","OR_Lake ","OR_Malheur ","OR_Marion ",
                                            "OR_Morrow ","OR_Sherman ","OR_Umatilla ","OR_Union ","WA_Adams ","WA_Benton ",
                                            "WA_Douglas ","WA_Franklin ","WA_Grant ","WA_Lincoln ","WA_Spokane ","WA_Walla Walla ",
                                            "WA_Whitman ","WA_Yakima "),year%in%c(1990:2020))
str(West_Hist)

check <-West_Hist |> group_by(year,State_Coun) |> mutate(Historic=frostfree_day_Fall_median+frostfree_day_Spring_median+frostfree_day_Summer_median+
                                                           frostfree_day_Winter_median)
West_Historical <- check |> select(year,State_Coun ,model,Historic)
K2 <- K2 |> mutate(model="RCP45")
West_Historical<-West_Historical[,c(1,2,4,3)]
colnames(West_Historical)[3]<- "Frostfree_day"
colnames(K2)[3]<- "Frostfree_day"


Final <- rbind(West_Historical,K2)
Final$Future <- Final$State_Coun
##colnames(Final)<- c("Future","Historical")
##Final1 <- melt(Final)

ggplot(Final, aes(x=model, y=Frostfree_day,fill=model)) + 
  geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()

##Historic analogs for 1st nearest location

Data3 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Cmatrix_90_20_v6.csv")
Analog_Hist<-Data3 |> filter(State_Coun %in%c("ID_Twin Falls ","OR_Clackamas","WA_Walla Walla ","WA_Walla Walla ",
                                              "UT_Uintah","WA_Walla Walla ","OK_Logan", "NV_Lincoln", "ID_Ada", "ID_Ada",
                                              "WA_Grant ", "ID_Jerome ","ID_Ada","NV_Lincoln","ID_Ada","ID_Ada",
                                              "NV_Churchill", "OR_Gilliam", "ID_Nez Perce", "ID_Ada", "ID_Ada",
                                              "WA_Walla Walla ","WA_Walla Walla ","WA_Walla Walla","WA_Walla Walla ",
                                              "ID_Elmore", "ID_Elmore", "WA_Walla Walla ", "OR_Gilliam", "WA_Walla Walla "),year%in%c(1990:2020))
###write.csv(Analog_Hist, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Analog_Historical.csv")

check2 <-Analog_Hist |> group_by(year,State_Coun) |> mutate(Historic=frostfree_day_Fall_median+frostfree_day_Spring_median+frostfree_day_Summer_median+
                                                              frostfree_day_Winter_median)
Analog_Hist <- check2 |> select(year,State_Coun ,model,Historic)

Analog_Hist_Update <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Analog_Historical.csv")


All_data <- rbind(Final,Analog_Hist_Update)
unique(All_data$model)
###write.csv(All_data,"E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Alldata_RCP45.csv")
library(ggrepel)

All_data <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Alldata_RCP45.csv")
All_data$model = factor(All_data$model, levels=c("gridmet","Analog", "RCP45" ))
ggplot(All_data, aes(x=model, y=Frostfree_day,fill=model)) + 
  geom_text(data = All_data, aes(y = 150, label = Future,color=Future),position = position_dodge(width = 2))+
  geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())

All_data_RCP85 <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Network/Database/Alldata_RCP85.csv")
unique(All_data_RCP85$model)
All_data_RCP85$model = factor(All_data_RCP85$model, levels=c("gridmet","Analog", "RCP85" ))
ggplot(All_data_RCP85, aes(x=model, y=Frostfree_day,fill=model)) + 
  geom_text(data = All_data_RCP85, aes(y = 150, label = Future,color=Future),position = position_dodge(width = 2))+
  geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())

ggplot((All_data |>  filter(State =="Idaho")), aes(x=model, y=Frostfree_day,fill=model)) + 
  geom_text((All_data |>  filter(State =="Idaho")),mapping= aes(y = 175, label = Future,color=Future),position = position_dodge(width = 2))+
  geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+facet_wrap(~State_Coun)+theme_bw()+
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank())



ggplot((All_data |>  filter(State =="Oregon", State_Coun=="OR_Sherman")), aes(x=model, y=Frostfree_day,fill=model)) + 
  geom_text((All_data |>  filter(State =="Oregon", State_Coun=="OR_Sherman")),mapping= aes(y = 175, label = Future,color=Future),position = position_dodge(width = 2))+
  geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+theme_bw()+ 
  labs(x="Model under consideration", y="Variable Frost free day")+
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Future of county under consideration for RCP4.5")


ggplot((All_data_RCP85 |>  filter(State =="Oregon", State_Coun=="ID_Lake")), aes(x=model, y=Frostfree_day,fill=model)) + 
  geom_text((All_data_RCP85|>  filter(State =="Oregon", State_Coun=="ID_Lake")),mapping= aes(y = 175, label = Future,color=Future),position = position_dodge(width = 2))+
  geom_violin(trim=FALSE)+coord_flip()+geom_boxplot(width=0.1)+theme_bw()+ 
  labs(x="Model under consideration", y="Variable Frost free day")+
  theme(legend.position = "none",axis.ticks.x=element_blank(),axis.text.x=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Future of county under consideration for RCP8.5")


### Census Ag 2017- data clean code

library(tidyverse)
library(dplyr)

Ag2017 <- read.delim("C:/Users/supriya.savalkar/Downloads/2017_cdqt_data.txt/2017_cdqt_data.txt")
Ag2017
unique(Ag2017$SHORT_DESC)

Washington <- filter(Ag2017, STATE_NAME=="WASHINGTON", SECTOR_DESC=="CROPS")
#write.csv(Washington, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/Washington2017.csv")
##Washington <- filter(Ag2017, STATE_NAME=="WASHINGTON", SECTOR_DESC=="CROPS", SHORT_DESC=="CROP TOTALS - SALES, MEASURED IN $")
##Washington <- filter(Ag2017, STATE_NAME=="WASHINGTON", SECTOR_DESC=="CROPS", COMMODITY_DESC=="FIELD CROPS, OTHER", SHORT_DESC=="FIELD CROPS, OTHER, INCL HAY - SALES, MEASURED IN $"  )
unique(Washington$SHORT_DESC)
Idaho <- filter(Ag2017, STATE_NAME=="IDAHO", SECTOR_DESC=="CROPS", SHORT_DESC=="CROP TOTALS - SALES, MEASURED IN $")
ID<-subset(Idaho,COUNTY_NAME!="NULL" )
#write.csv(ID, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/Idaho2017.csv")
Oregon <- filter(Ag2017, STATE_NAME=="OREGON", SECTOR_DESC=="CROPS", SHORT_DESC=="CROP TOTALS - SALES, MEASURED IN $")
OR<-subset(Oregon,COUNTY_NAME!="NULL" )
#write.csv(OR, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/Oregon2017.csv")

######Washington
WA <-filter(Ag2017, STATE_NAME=="WASHINGTON", SECTOR_DESC=="CROPS" )
WA1<-subset(WA,COUNTY_NAME!="NULL" )
##write.csv(WA1, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/WA1.csv")
unique(WA1$SHORT_DESC)
unique(WA1$SHORT_DESC)


WA_Acres <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/WA2.csv") 
unique(WA_Acres$Detail)
WA_Acres1<-filter(WA_Acres, Detail==" ACRES HARVESTED")

#WA_Acres1$Value <- as.numeric(WA_Acres1$Value)
WA_Harvest <-aggregate(VALUE ~ COUNTY_NAME, data=WA_Acres1, FUN = sum)
#write.csv(WA_Harvest, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/WA_Harvest.csv")
##WA_Grown <-aggregate(VALUE ~ COUNTY_NAME, data=WA_Acres1, FUN = sum)
##WA_Acres1 <-tapply(WA_Acres$VALUE, WA_Acres$COUNTY_NAME, FUN=sum)
##WA_Value <- aggregate(WA_Acres$Value, by=list(COUNTY_NAME=WA_Acres$COUNTY_NAME), FUN=sum())

###########Oregon
OR <-filter(Ag2017, STATE_NAME=="OREGON", SECTOR_DESC=="CROPS" )
OR1<-subset(OR,COUNTY_NAME!="NULL" )
#write.csv(OR1, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/OR1.csv")
unique(OR1$SHORT_DESC)

OR_Acres <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/OR2.csv") 
unique(OR_Acres$Detail)
OR_Acres1<-filter(OR_Acres, Detail==" ACRES HARVESTED")
OR_Harvest <-aggregate(VALUE ~ COUNTY_NAME, data=OR_Acres1, FUN = sum)
#write.csv(OR_Harvest, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/OR_Harvest.csv")

###Idaho
ID <-filter(Ag2017, STATE_NAME=="IDAHO", SECTOR_DESC=="CROPS" )
ID1<-subset(ID,COUNTY_NAME!="NULL" )
#write.csv(ID1, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/ID1.csv")
unique(ID1$SHORT_DESC)

ID_Acres <- read.csv("E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/ID2.csv") 
unique(ID_Acres$Detail)
ID_Acres1<-filter(ID_Acres, Detail==" ACRES HARVESTED")
ID_Harvest <-aggregate(VALUE ~ COUNTY_NAME, data=ID_Acres1, FUN = sum)
#write.csv(ID_Harvest, "E:/CourseWork/Fall2021_CPT_S575_DataScience/Term_Project/Census_Data/ID_Harvest.csv")

## Count
RCP4.5 <- read.csv("C:/SUPRIYA/WSU_2021/DataScience/Analysis/RCP_4.5_Analog.csv")
str(RCP4.5)
L1 <-RCP4.5 %>%  group_by(State_County) %>% count(Loc_NN_1)%>% slice(which.max(n))
L2 <-RCP4.5 %>%  group_by(State_County) %>% count(Loc_NN_2)%>% slice(which.max(n))
L3 <-RCP4.5 %>%  group_by(State_County) %>% count(Loc_NN_3)%>% slice(which.max(n))
L4 <-RCP4.5 %>%  group_by(State_County) %>% count(Loc_NN_4)%>% slice(which.max(n))
L5 <-RCP4.5 %>%  group_by(State_County) %>% count(Loc_NN_5)%>% slice(which.max(n))
L5a <-RCP4.5 %>%  group_by(State_County) %>% count(Loc_NN_5)

RCP4.5_Count <- cbind(L1,L2,L3,L4,L5)
RCP4.5_Count <- RCP4.5_Count %>% select(,-c(4,7,10,13))
write.csv(RCP4.5_Count,"C:/SUPRIYA/WSU_2021/DataScience/Analysis/RCP_4.5_Analog_Count.csv")

RCP8.5 <- read.csv("C:/SUPRIYA/WSU_2021/DataScience/Analysis/RCP_8.5_Analog.csv")
str(RCP8.5)
M1 <-RCP8.5 %>%  group_by(State_County) %>% count(Loc_NN_1)%>% slice(which.max(n))
M2 <-RCP8.5 %>%  group_by(State_County) %>% count(Loc_NN_2)%>% slice(which.max(n))
M3 <-RCP8.5 %>%  group_by(State_County) %>% count(Loc_NN_3)%>% slice(which.max(n))
M4 <-RCP8.5 %>%  group_by(State_County) %>% count(Loc_NN_4)%>% slice(which.max(n))
M5 <-RCP8.5 %>%  group_by(State_County) %>% count(Loc_NN_5)%>% slice(which.max(n))

RCP8.5_Count <- cbind(M1,M2,M3,M4,M5)
RCP8.5_Count <- RCP8.5_Count %>% select(,-c(4,7,10,13))
write.csv(RCP8.5_Count,"C:/SUPRIYA/WSU_2021/DataScience/Analysis/RCP_8.5_Analog_Count.csv")





