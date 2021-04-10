#### Loading the dtata & Libraries ####
#dfmain <- read.csv('nypd_burg_1.csv')
library(lubridate)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(maptools)
library(rgeos)
library(rgdal)
library(plotly)
#fix levels
month_order <- c('January','February','March','April','May','June','July','August',
                 'September','October','November','December')
day_order <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')

nyc_map <- readOGR(dsn = "nypp.shp", stringsAsFactors = F) #map of nyc #https://rpubs.com/huanfaChen/ggplotShapefile

####Saving/Loading dataframe####
#save(df,file = 'dfsave1_3.Rda')
#save(new_df,file = 'new_df.rda')
load('dfsave1_3.Rda')
load('new_df.rda')
load('precinct_cor.rda') #geom_text(data = precinct_cor, aes(x=X_COORD_CD,y=Y_COORD_CD, label = ADDR_PCT_CD), size = 4) 

#### cleaning the columns format ####
#Combing date and time for occurance start
dfmain <- dfmain %>% rowwise() %>% 
  mutate(CMPLNT_FR = paste(CMPLNT_FR_DT,CMPLNT_FR_TM,sep=' '))
dfmain$CMPLNT_FR  <- as.POSIXct(dfmain$CMPLNT_FR,format="%m/%d/%Y %H:%M:%S")

#Combing date and time for occurance end
dfmain <- dfmain %>% rowwise() %>% 
  mutate(CMPLNT_TO = paste(CMPLNT_TO_DT,CMPLNT_TO_TM,sep=' '))
dfmain$CMPLNT_TO  <- as.POSIXct(dfmain$CMPLNT_TO,format="%m/%d/%Y %H:%M:%S")

#fiing date time Report date
dfmain$RPT_DT<- as.POSIXct( dfmain$RPT_DT, format = '%m/%d/%Y')

#Fix percinct formmat
dfmain$ADDR_PCT_CD <- as.factor(dfmain$ADDR_PCT_CD)

#Selecting columns taht are being used
dfmain <- select(dfmain,CMPLNT_NUM,CMPLNT_FR,CMPLNT_TO,
                   ADDR_PCT_CD,RPT_DT,PD_DESC_TYP,PD_DESC_TM, CRM_ATPT_CPTD_CD,BORO_NM,
                   LOC_OF_OCCUR_DESC,PREM_TYP_DESC,Latitude,Longitude,Lat_Lon,
                   VIC_AGE_GROUP,VIC_RACE,VIC_SEX,SUSP_AGE_GROUP,X_COORD_CD,Y_COORD_CD)



#### Cleaning the data set ####
#changing blacnk to NA
df$SUSP_AGE_GROUP[df$SUSP_AGE_GROUP == ""] <- NA
df$VIC_SEX[df$VIC_SEX == ""] <- NA

#removing the one row with wrong value in CMPLT_TO dat
df <- filter(df,CMPLNT_NUM != 907472938)

#fixing the dates that seems to be entered wrong ()
for(i in 1:nrow(df)){
  if(year(df$CMPLNT_FR[i]) <= 2004 & (!is.na(df$CMPLNT_FR[i]))){
    year(df$CMPLNT_FR)[i] <- year(df$CMPLNT_TO)[i]
  }
}

# puting UNKNOWN values for teh foloowing columnns with missing values
df$SUSP_AGE_GROUP[df$SUSP_AGE_GROUP == 2017] <- 'UNKNOWN'
library(Hmisc) 
df$LOC_OF_OCCUR_DESC <- with(df,impute(LOC_OF_OCCUR_DESC,'UNKNOWN'))
df$PREM_TYP_DESC <- with(df,impute(PREM_TYP_DESC,'UNKNOWN'))
df$SUSP_AGE_GROUP <- with(df,impute(SUSP_AGE_GROUP,'UNKNOWN'))

# cleaninf new_df dataset
new_df$PREM_TYP_DESC <- droplevels(new_df$PREM_TYP_DESC)
new_df$near_rep <- as.factor(new_df$near_rep)

# creating precinct map
dftemp <- df %>% filter(!is.na(X_COORD_CD) &!is.na(Y_COORD_CD) & !is.na(ADDR_PCT_CD))
precinct_cor <- sqldf("select ADDR_PCT_CD as ADDR_PCT_CD,avg(X_COORD_CD) as X_COORD_CD,avg(Y_COORD_CD) as Y_COORD_CD
            from dftemp
            group by ADDR_PCT_CD")
rm(dftemp)


#### (df1)Total Burglaries, comercial and residential####
df_temp <- df
df_temp$RPT_year <- year(df_temp$RPT_DT)
df1 <- sqldf("select RPT_year,PD_DESC_TYP,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where PD_DESC_TYP == 'COMMERCIAL' or PD_DESC_TYP == 'RESIDENCE' or
             PD_DESC_TYP == 'TRUCK'
             group by RPT_year,PD_DESC_TYP")
rm(df_temp)

#Plotting df1
pdf1 <- ggplot(df1,aes(x=RPT_year,y=Total_Burglary,color = factor(PD_DESC_TYP)))+
  geom_line(size = 2) + theme_clean() +
  scale_x_continuous(breaks=c(2006:2018), labels=c(2006:2018),limits=c(2006,2018)) +
  xlab('Year') + ylab('Total number of burglaries') + ggtitle('Burglaries from 2006 to 2018')+
  labs(color = "Location type") 
print(pdf1)

#### (df2)Burglraies based on the month####
df_temp <- df
df_temp$RPT_month <- months(as.Date(df_temp$RPT_DT))

df2 <- sqldf("select RPT_month,PD_DESC_TYP,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where PD_DESC_TYP == 'COMMERCIAL' or PD_DESC_TYP == 'RESIDENCE' or
             PD_DESC_TYP == 'TRUCK'
             group by RPT_month,PD_DESC_TYP")
rm(df_temp)
#Plotting df2
pdf2 <- ggplot(df2,aes(x=factor(RPT_month,levels = month_order),y=Total_Burglary,fill = factor(PD_DESC_TYP)))+
  geom_bar(stat='identity',position = 'dodge') + theme_clean() +
  xlab('Month') + ylab('Total number of burglaries') + ggtitle('Burglary month from 2006 to 2018')+
  labs(fill = "Location type")  
print(pdf2)

#### (df3)Burglraies based on the day (CMPLNT_FR)####
df_temp <- df
df_temp$RPT_day <- wday(df_temp$CMPLNT_FR,label = T) #column name is rpt but it is based of CMPLNT_FR

df3 <- sqldf("select RPT_day,PD_DESC_TYP,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where PD_DESC_TYP == 'COMMERCIAL' or PD_DESC_TYP == 'RESIDENCE' or
             PD_DESC_TYP == 'TRUCK'
             group by RPT_day,PD_DESC_TYP")
rm(df_temp)
#Plotting df3 without any missing values
pdf3 <- ggplot(df3 %>% filter(!is.na(RPT_day)),aes(x=factor(RPT_day,levels = day_order),
                            y=Total_Burglary,fill = factor(PD_DESC_TYP)))+
  geom_bar(stat='identity',position = 'dodge',na.rm = TRUE) + theme_clean() +
  xlab('Day of the week') + ylab('Total number of burglaries') + 
  ggtitle('Burglaries based on the day of the week')+
  labs(fill = "Time")
print(pdf3)

#### (df4)Burglraies based on the time of day (CMPLNT_FR)####
df_temp <- df
df_temp$CMPLNT_time <- hour(df_temp$CMPLNT_FR)

df4 <- sqldf("select CMPLNT_time,PD_DESC_TYP,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where PD_DESC_TYP == 'COMMERCIAL' or PD_DESC_TYP == 'RESIDENCE' or
             PD_DESC_TYP == 'TRUCK'
             group by CMPLNT_time,PD_DESC_TYP")
rm(df_temp)

#Plotting df4 without any missing values
pdf4 <- ggplot(df4 %>% filter(!is.na(CMPLNT_time)),aes(x=factor(CMPLNT_time),
                                                       y=Total_Burglary,fill = factor(PD_DESC_TYP)))+
  geom_bar(stat='identity',position = 'dodge',na.rm = TRUE) + theme_clean() +
  xlab('Time of day') + ylab('Total number of burglaries') + 
  ggtitle('Burglaries based on Time of day ')+
  labs(color = "Location type") 
print(pdf4)
#### (df4_R/C/T)Burglraies based on the time of day and place####
df_temp <- df
df_temp$RPT_day <- wday(df_temp$CMPLNT_FR,label = T) #column name is rpt but it is based of CMPLNT_FR

df4_R <- sqldf("select RPT_day,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where PD_DESC_TYP == 'RESIDENCE' and (PD_DESC_TM == 'DAY' or PD_DESC_TM == 'NIGHT')
             group by RPT_day,PD_DESC_TM")
rm(df_temp)
#Plotting df4 without any missing values
pdf4_R <- ggplot(df4_R %>% filter(!is.na(RPT_day)),aes(x=factor(RPT_day,levels = day_order),
                                                   y=Total_Burglary,fill = factor(PD_DESC_TM)))+
  geom_bar(stat='identity',position = 'dodge',na.rm = TRUE) + theme_clean() +
  xlab('Day of the week') + ylab('Total number of burglaries') + 
  ggtitle('Burglaries based on the day of the week for Residence')+
  labs(fill = "Time") 


####
df_temp <- df
df_temp$RPT_day <- wday(df_temp$CMPLNT_FR,label = T)
df4_C <- sqldf("select RPT_day,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where PD_DESC_TYP == 'COMMERCIAL' and (PD_DESC_TM == 'DAY' or PD_DESC_TM == 'NIGHT')
             group by RPT_day,PD_DESC_TM")
rm(df_temp)
#Plotting df1 without any missing values
pdf4_C <- ggplot(df4_C %>% filter(!is.na(RPT_day)),aes(x=factor(RPT_day,levels = day_order),
                                                     y=Total_Burglary,fill = factor(PD_DESC_TM)))+
  geom_bar(stat='identity',position = 'dodge',na.rm = TRUE) + theme_clean() +
  xlab('Day of the week') + ylab('Total number of burglaries') + 
  ggtitle('Burglaries based on the day of the week for Commercial')+
  labs(fill = "Time") 

###
df_temp <- df
df_temp$RPT_day <- wday(df_temp$CMPLNT_FR,label = T)
df4_T <- sqldf("select RPT_day,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where PD_DESC_TYP == 'TRUCK' and (PD_DESC_TM == 'DAY' or PD_DESC_TM == 'NIGHT')
             group by RPT_day,PD_DESC_TM")
rm(df_temp)
#Plotting df4 without any missing values
pdf4_T <- ggplot(df4_T %>% filter(!is.na(RPT_day)),aes(x=factor(RPT_day,levels = day_order),
                                                       y=Total_Burglary,fill = factor(PD_DESC_TM)))+
  geom_bar(stat='identity',position = 'dodge',na.rm = TRUE) + theme_clean() +
  xlab('Day of the week') + ylab('Total number of burglaries') + 
  ggtitle('Burglaries based on the day of the week for Truck')+
  labs(fill = "Time") 

#printing all 3 graphs next to each other 
grid_pdf4 <- grid.arrange(pdf4_R,pdf4_C,pdf4_T, ncol = 3,nrow = 1)

#### (df5)residential Burglraies based on the time of day (CMPLNT_FR)####
df_temp <- df
df_temp$RPT_year <- year(df_temp$RPT_DT)
df5 <- sqldf("select RPT_year,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where (PD_DESC_TM == 'DAY' or PD_DESC_TM == 'NIGHT') and PD_DESC_TYP == 'RESIDENCE'
             group by RPT_year,PD_DESC_TM")
rm(df_temp)

#plot df5
pdf5 <- ggplot(df5,aes(x=RPT_year,y=Total_Burglary)) + geom_line(aes(color=PD_DESC_TM)) +
  labs(title = 'Burglaries based on time of day between 2006  and 2019' )+
  labs(x = 'Time of day') +
  labs(y = 'Total number of burglaries') +
  labs(fill = "Location type") +
  scale_x_continuous(breaks=c(2006:2018), labels=c(2006:2018),limits=c(2006,2018)) +
  geom_vline(xintercept=c(2008,2013))
print(pdf5)


#### (df6) residential time,day and week/weekend####
df_temp <- df
df_temp <- df_temp %>% mutate(weekday = ifelse((wday(CMPLNT_FR,label = T)=='Sat')|
                                                 (wday(CMPLNT_FR,label = T)=='Sun'),'Weekend','weekday'))

df6 <- sqldf("select PREM_TYP_DESC,weekday,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where (PREM_TYP_DESC = 'RESIDENCE - APT. HOUSE' or PREM_TYP_DESC ='RESIDENCE-HOUSE'
             or PREM_TYP_DESC = 'RESIDENCE - PUBLIC HOUSING')
             and
             (PD_DESC_TM = 'DAY' or PD_DESC_TM = 'NIGHT')
             group by PREM_TYP_DESC,weekday,PD_DESC_TM")
df6 <- na.omit(df6)
df6$weekday <- as.factor(df6$weekday)
rm(df_temp)
#plot
#apartments
pdf6_1 <- ggplot(df6 %>% filter(PREM_TYP_DESC == 'RESIDENCE - APT. HOUSE')
                 ,aes(x=weekday,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  ggtitle('Burglaries at Apartments')+
  labs(fill = "Time")

#houses
pdf6_2 <- ggplot(df6 %>% filter(PREM_TYP_DESC == 'RESIDENCE-HOUSE')
                 ,aes(x=weekday,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  ggtitle('Burglaries at houses')+
  labs(fill = "Time")

#Public apartments
pdf6_3 <- ggplot(df6 %>% filter(PREM_TYP_DESC == 'RESIDENCE - PUBLIC HOUSING')
                 ,aes(x=weekday,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  ggtitle('Burglaries at Public housing-apartments')+
  labs(fill = "Time")

#all 3 graphs together
grid_pdf6 <- grid.arrange(pdf6_1,pdf6_2,pdf6_3, ncol = 3,nrow = 1)

#### (df7) victim age and time of day & week/weekend (residential)####
df_temp <- df
df_temp <- df_temp %>% mutate(weekday = ifelse((wday(CMPLNT_FR,label = T)=='Sat')|
                                                 (wday(CMPLNT_FR,label = T)=='Sun'),'Weekend','weekday'))

df7 <- sqldf("select VIC_AGE_GROUP,weekday,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where (PREM_TYP_DESC = 'RESIDENCE - APT. HOUSE' or PREM_TYP_DESC ='RESIDENCE-HOUSE'
             or PREM_TYP_DESC = 'RESIDENCE - PUBLIC HOUSING')
             and
             (PD_DESC_TM = 'DAY' or PD_DESC_TM = 'NIGHT')
             and (VIC_AGE_GROUP = '25-44' or VIC_AGE_GROUP = '45-64' or VIC_AGE_GROUP = '18-24' or
             VIC_AGE_GROUP = '65+' or VIC_AGE_GROUP = '<18')
             group by weekday,PD_DESC_TM,VIC_AGE_GROUP")
df7 <- na.omit(df7)
df7$weekday <- as.factor(df7$weekday)
rm(df_temp)

pdf7_weekday <- ggplot(df7 %>% filter(weekday == 'weekday')
                       ,aes(x=VIC_AGE_GROUP,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  ggtitle('Victims age and crime during weekday')+
  labs(fill = "Time")
print(pdf7_weekday)

pdf7_weekend <- ggplot(df7 %>% filter(weekday == 'Weekend')
                       ,aes(x=VIC_AGE_GROUP,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  ggtitle('Victims age and crime during weekend')+
  labs(fill = "Time")
print(pdf7_weekend)

grid.pdf7 <- grid.arrange(pdf7_weekday,pdf7_weekend, ncol = 2,nrow = 1)
print(pdf7_weekday)

#### (df8) suspects age and tiem of day ####
df_temp <- df
df8 <- sqldf("select SUSP_AGE_GROUP,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where (PD_DESC_TM = 'DAY' or PD_DESC_TM = 'NIGHT')
             and (SUSP_AGE_GROUP = '25-44' or SUSP_AGE_GROUP = '45-64' or SUSP_AGE_GROUP = '18-24' or
             SUSP_AGE_GROUP = '65+' or SUSP_AGE_GROUP = '<18')
             group by PD_DESC_TM,SUSP_AGE_GROUP")
df8 <- na.omit(df8)
rm(df_temp)

#plot
pdf8 <- ggplot(df8,aes(x=SUSP_AGE_GROUP,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  labs(title ='suspects age and crime time')+
  labs(fill = "Time")
print(pdf8)

#### (df9) suspects age adn threee propety type ####
df_temp <- df
df9 <- sqldf("select SUSP_AGE_GROUP,PD_DESC_TYP,PD_DESC_TM,count(CMPLNT_NUM) as Total_Burglary
             from df_temp
             where (PD_DESC_TYP == 'COMMERCIAL' or PD_DESC_TYP == 'RESIDENCE' or
             PD_DESC_TYP == 'TRUCK')
             and
             (PD_DESC_TM = 'DAY' or PD_DESC_TM = 'NIGHT')
             and (SUSP_AGE_GROUP = '25-44' or SUSP_AGE_GROUP = '45-64' or SUSP_AGE_GROUP = '18-24' or
             SUSP_AGE_GROUP = '65+' or SUSP_AGE_GROUP = '<18')
             group by PD_DESC_TYP,PD_DESC_TM,SUSP_AGE_GROUP")
df9 <- na.omit(df9)
rm(df_temp)

#plot
pdf9_r <- ggplot(df9 %>% filter(PD_DESC_TYP == 'RESIDENCE'),aes(x=SUSP_AGE_GROUP,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  labs(title ='suspects age and crime time for residential')+
  labs(fill = "Time")


pdf9_c <- ggplot(df9 %>% filter(PD_DESC_TYP == 'COMMERCIAL'),aes(x=SUSP_AGE_GROUP,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  labs(title ='suspects age and crime time for Commercial')+
  labs(fill = "Time")


pdf9_t <- ggplot(df9 %>% filter(PD_DESC_TYP == 'TRUCK'),aes(x=SUSP_AGE_GROUP,y=Total_Burglary))+
  geom_bar(aes(fill=factor(PD_DESC_TM)),stat='identity',position = 'dodge') + theme_clean() +
  xlab(element_blank()) + ylab('Total number of burglaries') + 
  labs(title ='suspects age and crime time for Truck')+
  labs(fill = "Time")

grid.pdf9 <- grid.arrange(pdf9_r,pdf9_c,pdf9_t, ncol = 3,nrow = 1)

#### near formula ####
robocop5<- function(df,idnum,xrpt){
  df0 <- filter(df,CMPLNT_NUM ==idnum)
  df0$k <- 1
  df0 <- select(df0,CMPLNT_NUM,RPT_DT,Latitude,Longitude,k)
  colnames(df0) <- c('CMPLNT_NUM.x','RPT_DT.x','Latitude.x','Longitude.x','k')
  
  intx <- interval(ymd(xrpt) - days(14),ymd(xrpt) + days(14))
  df1 <- filter(df,RPT_DT %within%intx)
  
  if(dim(df1)[1] == 0){return(0)}else{df1$k <- 1
  df1 <- select(df1,CMPLNT_NUM,RPT_DT,Latitude,Longitude,k)
  colnames(df1) <- c('CMPLNT_NUM.y','RPT_DT.y','Latitude.y','Longitude.y','k')
  df1 <- df1 %>% right_join(df0, by = "k")  %>% select(-k)  
  df1 <-  filter(df1,CMPLNT_NUM.x != CMPLNT_NUM.y)
  df1 <- filter(df1, ymd(RPT_DT.x) != ymd(RPT_DT.y))
  df1 <- filter(df1,(abs(Latitude.x-Latitude.y)+abs(Longitude.x-Longitude.y)) <= 0.004)}
  if(dim(df1)[1] == 0){return(0)}else{
    
    df2 <- df1 %>% rowwise() %>% mutate(nearrep = 1)}
  if(1 %in% df2$nearrep){return(1)}else{return(0)}
}

#### near repeat restaurant all
nearrep_restaurant <- filter (df,PREM_TYP_DESC == 'RESTAURANT/DINER')
nearrep_restaurant <- nearrep_restaurant %>% rowwise() %>% 
  mutate(near_rep = robocop5(df = nearrep_restaurant,CMPLNT_NUM,RPT_DT))
save(nearrep_restaurant,file = 'nearrep_restaurant.rda')

#### near repeat bodega 
nearrep_bodega <- filter (df,PREM_TYP_DESC == 'GROCERY/BODEGA')
nearrep_bodega <- nearrep_bodega %>% rowwise() %>% 
  mutate(near_rep = robocop5(df = nearrep_bodega,CMPLNT_NUM,RPT_DT))
save(nearrep_bodega,file = 'nearrep_bodega.rda')

#### near repeat CONSTRUCTION SITE all 
nearrep_construction <- filter (df,PREM_TYP_DESC == 'CONSTRUCTION SITE')
nearrep_construction <- nearrep_construction %>% rowwise() %>% 
  mutate(near_rep = robocop5(df = nearrep_construction,CMPLNT_NUM,RPT_DT))
save(nearrep_construction,file = 'nearrep_construction.rda')

#### residential app 2017
intx_app_2017_total <- interval(ymd('2017-01-01') - days(14),ymd('2017-12-31') + days(14))
intx_app_2017 <- interval(ymd('2017-01-01'),ymd('2017-12-31') )
bd <- filter(df,PREM_TYP_DESC == 'RESIDENCE - APT. HOUSE' & 
               RPT_DT %within%intx_app_2017_total)
bd1 <- filter(bd,RPT_DT %within% intx_app_2017)

res_app_2017 <- mutate(bd1,near_rep = robocop5(bd,CMPLNT_NUM,RPT_DT))
rm(bd,bd1)

#### resedentia housing 2017 

intx_app_2017_total <- interval(ymd('2017-01-01') - days(14),ymd('2017-12-31') + days(14))
intx_app_2017 <- interval(ymd('2017-01-01'),ymd('2017-12-31'))
bd <- filter(df,PREM_TYP_DESC == 'RESIDENCE-HOUSE' & 
               RPT_DT %within%intx_app_2017_total)
bd1 <- filter(bd,RPT_DT %within% intx_app_2017)

res_house_2017 <- mutate(bd1,near_rep = robocop5(bd,CMPLNT_NUM,RPT_DT))
rm(bd,bd1)

#### resedentia public housing 2017 

intx_app_2017_total <- interval(ymd('2017-01-01') - days(14),ymd('2017-12-31') + days(14))
intx_app_2017 <- interval(ymd('2017-01-01'),ymd('2017-12-31'))
bd <- filter(df,PREM_TYP_DESC == 'RESIDENCE - PUBLIC HOUSING' & 
               RPT_DT %within%intx_app_2017_total)
bd1 <- filter(bd,RPT_DT %within% intx_app_2017)

res_housepublic_2017 <- mutate(bd1,near_rep = robocop5(bd,CMPLNT_NUM,RPT_DT))
rm(bd,bd1)
#### near rep bar nightclub
nearrep_bar <- filter (df,PREM_TYP_DESC == 'BAR/NIGHT CLUB')
nearrep_bar <- nearrep_bar %>% rowwise() %>% 
  mutate(near_rep = robocop5(nearrep_bar,CMPLNT_NUM,RPT_DT))

#### combining ner repeates ####
new_df <- union(nearrep_restaurant,nearrep_bodega) %>%
  union(nearrep_construction) %>% union(res_app_2017) %>% 
  union(res_house_2017) %>% 
  union(res_housepublic_2017)
#add teh x y cord
bd <- df %>% select(X_COORD_CD,Y_COORD_CD,CMPLNT_NUM)
new_df <- inner_join(new_df,bd,by='CMPLNT_NUM')
rm(bd)

#### (df10) density of burglaries res_apartment 2006-2018 ####
pdf10 <- ggplot(filter(df,PREM_TYP_DESC == 'RESIDENCE - APT. HOUSE'),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_density2d() +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  theme_map()
print(pdf10)

pdf10_2 <- ggplot(filter(df,PREM_TYP_DESC == 'RESIDENCE - APT. HOUSE' & (year(RPT_DT)== 2017)),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_point(aes(color=factor(PD_DESC_TM))) +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  theme_map()
print(pdf10_2)

pdf10_3 <- ggplot(filter(new_df,PREM_TYP_DESC == 'RESIDENCE - APT. HOUSE'  &
                           PD_DESC_TM != 'UNKNOWN'),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_point(aes(color=factor(near_rep))) +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  geom_text(data = precinct_cor, aes(x=X_COORD_CD,y=Y_COORD_CD, label = ADDR_PCT_CD), size = 4) +
  theme_map()
print(pdf10_3)

####(df11) Restaurant near repeat 2013-2017 ####
pdf11_2017 <- ggplot(filter(new_df,PREM_TYP_DESC == 'RESTAURANT/DINER'  & near_rep==1 &
                              PD_DESC_TM != 'UNKNOWN'& year(RPT_DT)==2017),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_point(aes(color=factor(PD_DESC_TM))) +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  geom_text(data = precinct_cor, aes(x=X_COORD_CD,y=Y_COORD_CD, label = ADDR_PCT_CD), size = 4) +
  theme_map()+labs(title ='RESTAURANT/DINER near repeat reports in 2017') +labs(color = "Time")+
  theme(legend.position = 'top')
print(pdf11_2017)

pdf11_2016 <- ggplot(filter(new_df,PREM_TYP_DESC == 'RESTAURANT/DINER'  & near_rep==1 &
                              PD_DESC_TM != 'UNKNOWN'& year(RPT_DT)==2016),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_point(aes(color=factor(PD_DESC_TM))) +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  geom_text(data = precinct_cor, aes(x=X_COORD_CD,y=Y_COORD_CD, label = ADDR_PCT_CD), size = 4) +
  theme_map() +labs(title ='RESTAURANT/DINER near repeat reports in 2016') +labs(color = "Time")+
  theme(legend.position = 'top')
print(pdf11_2016)

pdf11_2015 <- ggplot(filter(new_df,PREM_TYP_DESC == 'RESTAURANT/DINER'  & near_rep==1 &
                              PD_DESC_TM != 'UNKNOWN'& year(RPT_DT)==2015),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_point(aes(color=factor(PD_DESC_TM))) +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  geom_text(data = precinct_cor, aes(x=X_COORD_CD,y=Y_COORD_CD, label = ADDR_PCT_CD), size = 4) +
  theme_map() +labs(title ='RESTAURANT/DINER near repeat reports in 2015') +labs(color = "Time") +
  theme(legend.position = 'top')
print(pdf11_2015)

pdf11_2014 <- ggplot(filter(new_df,PREM_TYP_DESC == 'RESTAURANT/DINER'  & near_rep==1 &
                              PD_DESC_TM != 'UNKNOWN'& year(RPT_DT)==2014),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_point(aes(color=factor(PD_DESC_TM))) +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  geom_text(data = precinct_cor, aes(x=X_COORD_CD,y=Y_COORD_CD, label = ADDR_PCT_CD), size = 4) +
  theme_map() +labs(title ='RESTAURANT/DINER near repeat reports in 2014') +labs(color = "Time") +
  theme(legend.position = 'top')
print(pdf11_2014)

pdf11_2013 <- ggplot(filter(new_df,PREM_TYP_DESC == 'RESTAURANT/DINER'  & near_rep==1 &
                              PD_DESC_TM != 'UNKNOWN'& year(RPT_DT)==2013),aes(x=X_COORD_CD,y=Y_COORD_CD)) + 
  geom_point(aes(color=factor(PD_DESC_TM))) +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group),colour = "black", fill = NA) +
  geom_text(data = precinct_cor, aes(x=X_COORD_CD,y=Y_COORD_CD, label = ADDR_PCT_CD), size = 4) +
  theme_map() +labs(title ='RESTAURANT/DINER near repeat reports in 2013') +labs(color = "Time") +
  theme(legend.position = 'top')
print(pdf11_2013)

#### (df12) near repeat by perm type ####
df12<- new_df %>% filter(year(RPT_DT)==2017)
df12 <- as.data.frame(table(df12$PREM_TYP_DESC,df12$near_rep))
df12<- reshape(df12, direction = "wide", idvar = "Var1", timevar = "Var2")
colnames(df12)<- c('PREM_TYP_DESC','not_near','near')
df12 <- df12 %>%mutate(percent = near/(near+not_near))
df12 <- df12 %>% mutate(total =near+not_near)

pdf12 <- ggplot(df12,aes(x=PREM_TYP_DESC)) + 
  geom_bar(aes(y=total,fill='Total burglaries'),stat = 'identity',position ="identity") + 
  geom_bar(aes(y=near,fill='Near repeat'),stat = 'identity',position ="identity")+
  xlab('Location type') + ylab('Total number of burglaries') +
  labs(title ='Total burglaries for location in 2017 and near repeat incidents')+
  labs(fill='legend')+ theme_clean() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(pdf12)

#### (df13) near repeat and total for precicts apt-hous ####
df13 <- sqldf("select ADDR_PCT_CD, near_rep,count(near_rep) as freq
              from new_df
              where PREM_TYP_DESC='RESIDENCE - APT. HOUSE'
              group by ADDR_PCT_CD, near_rep")
df13 <- reshape(df13, direction = "wide", idvar = "ADDR_PCT_CD", timevar = "near_rep")
df13 <- df13 %>% mutate(total=freq.0+freq.1)
df13 <- df13 %>% mutate(percent = freq.1/total)

df13_mostcrime <- df13 %>% filter(total >=123)
df13_mostnear <- df13 %>% filter(percent >=0.633)

#chaneg column name
colnames(df13_mostcrime) <- c('Precinct','Not Near Repeat','Near Repat','total','percent')
colnames(df13_mostnear) <- c('Precinct','Not Near Repeat','Near Repat','total','percent')

#for saving as png
grid.table(df13_mostcrime)
grid.table(df13_mostnear)


