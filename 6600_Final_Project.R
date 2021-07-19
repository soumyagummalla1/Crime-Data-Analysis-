setwd("Desktop")
data <- read.csv("Police_Department_Incident_Reports__2018_to_Present.csv")

library(dplyr)
library(ggplot2)
# Data Cleaning
# After cleaning the data the dataset has 279,693 rows and 12 columns.
data1<-data%>% 
  select(1,2,4,5,6,8,12,13,15,16,18,21,22,23,24,25)
data1$Incident_Count<-1

# Replace Filed online value
# True is 1
# N/A is 0
data1$Filed.Online<-as.character(data1$Filed.Online)
data1$Filed.Online[data1$Filed.Online=="true"]<-"1"
data1$Filed.Online[data1$Filed.Online== ""]<-"0"
data1$Filed.Online<-as.numeric(data1$Filed.Online)
data1<-na.omit(data1)

# Frequency of Incident Reports with Reference to Day of the Week
day1 = 
  data1%>%
  group_by(Incident.ID,Incident.Day.of.Week)%>%
  count %>%
  ungroup
day1<-table(day1$Incident.Day.of.Week)
day1<-as.data.frame(day1)
day1$Var1 <- factor(day1$Var1, levels= c( "Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday",
                                                    "Saturday"))
ggplot(day1, mapping = aes(x = Var1, y=Freq, group=1) ) + 
  geom_line(color='grey',size=3)+geom_point(color='steel blue')+ggtitle("Reports in Days") +
  xlab("Day") + ylab("Reports")+ labs(color = "Delay Type")+
  scale_color_manual(values=c("#999999", "#E69F00")+
  theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold")))


# Maximum Number of Incidents with Reference to Incident Category
crime<-
  data1%>%
  group_by(Incident.ID,Incident.Category)%>%
  count %>%
  ungroup
crime<-table(crime$Incident.Category)
crime<-as.data.frame(crime)
crime<-crime%>%
  top_n(10)
crime = crime[order(crime$Freq, decreasing= T), ]
crime[order(crime$Freq), ]
ggplot(data=crime, aes(x=reorder(Var1,Freq),y = Freq)) +
  geom_bar(stat="identity", fill = "chocolate1")+
  xlab("Incident Category") + ylab("Number Of Incident Reports") +
  coord_flip()+coord_flip()+
  theme( plot.title = element_text(color = "steelblue", size = 15, face = "bold"),
         axis.text=element_text(size=12),axis.title=element_text(color = "steelblue",size=14,face="bold")) +
  ggtitle("Top 10 Incident Category")

# Incident Reports in All the Months
# line graph
library(tidyr)
library(lubridate)
data1<-data1%>%
  mutate(Month=month(Incident.Date))


data1$Month<- month.abb[data1$Month]

months1<-table(data1$Month) 
months1<-as.data.frame(months1)

months1<- data1 %>%
  select(Incident.Year,Month,Incident_Count)%>%
  group_by(Incident.Year,Month)%>%
  summarise_all(funs(sum))

months1$Month<- factor(months1$Month, levels= c( "Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
months1$Incident.Year<-as.character(months1$Incident.Year)
months_plot<-ggplot(months1, mapping = aes(x = Month, y = Incident_Count, group = Incident.Year,color= Incident.Year) ) + 
  geom_line()+
  geom_point()+
  ggtitle("Incident Reports in All the Months")+
  xlab("Months") + ylab("Number of Incidents")+ labs(color = "Year") + ylim(10000,13500)+
  theme(plot.title = element_text(color = "steelblue", size = 15, face = "bold"),
       axis.text=element_text(size=12),axis.title=element_text(color = "steelblue",size=14,face="bold")) 
months_plot

# Plot number of cases that were reported online and Offline 
online<-table(data1$Filed.Online)
online<-as.data.frame(online)
online$Var1<-as.character(online$Var1)
online$Var1[online$Var1=="1"]<-"Yes"
online$Var1[online$Var1=="0"]<-"No"

library(scales)
onlinepie<-ggplot(online, aes(x="", y= Freq, fill=Var1))+ 
  ggtitle("Filed Online or not")+
  geom_bar(width = 1,stat = "identity")+
  coord_polar("y", statrt = 0)+ 
  theme_void()+ geom_text(state = 'identity', aes(label= paste0(Freq)), position = position_stack(vjust=0.5), color ="white", face ="bold", size = 5)+
  labs(fill = "Filed Online")
onlinepie

x <-  c(222269,50363)
labels <-  c("No", "Yes")
piepercent<- round(100*x/sum(x), 1)

# Plot the chart.
pie(x, labels = piepercent, main = "Filed Online chart",col = rainbow(length(x)))
legend("topright", c("No", "Yes"), cex = 0.8,
       fill = rainbow(length(x)))


# Plot to show which Incident subcategories within the Incident Categories 

subcat<-data1%>%
  select(Incident.Category, Incident.Subcategory,Incident_Count)%>%
  group_by(Incident.Category,Incident.Subcategory)%>%
  summarise_all(funs(sum))
subcat<-subset(subcat, Incident.Category %in% c("Larceny Theft", "Other Miscellaneous", "Non-Criminal", "Malicious Mischief", "Assault", "Burglary","Lost Property","Warrant","Motor Vehicle Theft", "Recovered Vehicle"))

subcat = subcat[order(subcat$Incident_Count, decreasing= T), ]


subcat_plot<-ggplot(subcat,aes(x=reorder(Incident.Category,-Incident_Count), y=Incident_Count,fill=Incident.Subcategory))+
  geom_bar(stat = "identity") +   coord_flip()+
  theme( plot.title = element_text(color = "steelblue", size = 20, face = "bold"),
         axis.text=element_text(size=12),axis.title=element_text(color = "steelblue",size=14,face="bold")) +
  ggtitle("Top 10 Incident Categories and their Subcategories")


# Find out which Police District had the maximum number of reports 
dis =
  data1 %>%
  group_by(Incident.ID,Police.District) %>%
  count %>%
  ungroup
dis<-table(dis$Police.District)
dis<-as.data.frame(dis)
dis<-dis%>%
  top_n(10)
dis = dis[order(dis$Freq, decreasing= T), ]

ggplot(data=dis, aes(x=reorder(Var1,Freq),y=Freq)) +
  geom_bar(stat="identity", fill = "brown2")+
  xlab("Police District") + ylab("Number of incident reports") +
  coord_flip()+
  theme( plot.title = element_text(color = "steelblue", size = 20, face = "bold"),
         axis.text=element_text(size=12),axis.title=element_text(color = "steelblue",size=14,face="bold")) +
  ggtitle("Police Districts with Highest Reports")

# Level of Resolution to Incidents
data1$Incident_Count<-as.numeric(data1$Incident_Count)
year_resol<-data1%>%
  select(Incident.Year, Resolution,Incident_Count)%>%
  group_by(Incident.Year, Resolution)%>%
  summarise(total_reports=sum(Incident_Coun))
plot_year<-ggplot(year_resol, aes(x=reorder(Incident.Year,total_reports), y=total_reports, fill= Resolution)) +
  geom_bar(stat="identity")+ggtitle("Level of Resolution to Incidents") +
  xlab("Year") + ylab("Number of Incidents")+
  theme( plot.title = element_text(color = "steelblue", size = 20, face = "bold"),
         axis.text=element_text(size=12),axis.title=element_text(color = "steelblue",size=14,face="bold"))+
  ggtitle("Level of Resolution")
plot_year


# Difference between Incident Time and Report Time
trial<-data1%>%
select(Incident.Datetime, Report.Datetime)
trial$Incident.Datetime<-as.Date(trial$Incident.Datetime)
trial$Report.Datetime<-as.Date(trial$Report.Datetime)
trial$diff<-trial$Report.Datetime-trial$Incident.Datetime
difff<-table(trial$diff)
difff<-as.data.frame(difff)
difff$Var1<-as.numeric(as.character(difff$Var1))

difff$Delay<-"Instant"
difff$Delay[difff$Var1<=30 & difff$Var1>=1]<-'1Mon'
difff$Delay[difff$Var1<=60 & difff$Var1>=31]<-'2Mon'
difff$Delay[difff$Var1<=90 & difff$Var1>=61]<-'3Mon'
difff$Delay[difff$Var1<=120 & difff$Var1>=91]<-'4Mon'
difff$Delay[difff$Var1<=150 & difff$Var1>=121]<-'5Mon'
difff$Delay[difff$Var1<=180 & difff$Var1>=151]<-'6Mon'
difff$Delay[difff$Var1<=210 & difff$Var1>=181]<-'7Mon'
difff$Delay[difff$Var1<=240 & difff$Var1>=211]<-'8Mon'
difff$Delay[difff$Var1<=270 & difff$Var1>=241]<-'9Mon'
difff$Delay[difff$Var1<=300 & difff$Var1>=271]<-'10Mon'
difff$Delay[difff$Var1<=330 & difff$Var1>=301]<-'11Mon'
difff$Delay[difff$Var1<=360 & difff$Var1>=331]<-'12Mon'
difff$Delay[difff$Var1<=390 & difff$Var1>=361]<-'13Mon'
difff$Delay[difff$Var1<=420 & difff$Var1>=391]<-'14Mon'
difff$Delay[difff$Var1<=450 & difff$Var1>=421]<-'15Mon'
difff$Delay[difff$Var1<=480 & difff$Var1>=451]<-'16Mon'
difff$Delay[difff$Var1<=510 & difff$Var1>=481]<-'17Mon'
difff$Delay[difff$Var1<=540 & difff$Var1>=511]<-'18Mon'
difff$Delay[difff$Var1<=570 & difff$Var1>=541]<-'19Mon'
difff$Delay[difff$Var1<=600 & difff$Var1>=571]<-'20Mon'
difff$Delay[difff$Var1<=630 & difff$Var1>=601]<-'21Mon'
difff$Delay[difff$Var1<=660 & difff$Var1>=631]<-'22Mon'
difff$Delay[difff$Var1<=690 & difff$Var1>=661]<-'23Mon'
difff$Delay[difff$Var1<=720 & difff$Var1>=691]<-'24Mon'
difff <- difff[-c(1), ]
difference<-difff%>%
  select(Delay,Freq)%>%
  group_by(Delay)%>%
  summarise_all(funs(sum))

# 
difference1<-difference[-c(11,16,17,24),]

difference1$Delay<- factor(difference1$Delay, levels= c("4Mon","5Mon","6Mon","7Mon","8Mon","9Mon","10Mon","11Mon","12Mon","13Mon","14Mon","15Mon","16Mon","17Mon","18Mon","19Mon","20Mon","21Mon","22Mon","23Mon","24Mon"))

Difference_plot<-ggplot(difference1, mapping = aes(x = Delay, y = Freq, group=1) ) + geom_line(color='grey',size=1)+geom_point(color='steel blue')+ggtitle("Difference between Incident Date and Report Date") +
  xlab("Number of Months Between Incident Date and Report Date") + ylab("Number of reports")+theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))+ ylim(1,900)
Difference_plot


bostondata <- read.csv("boston_day.csv")
day1$City<-"San Francisco"
bostondata<-bostondata[,-c(1)]
final_day<-rbind(day1,bostondata)

days_plot<-ggplot(final_day, mapping = aes(x = Var1, y = Freq, group = City,color= City) ) + geom_line(size=1.5)+geom_point(size=2)+ggtitle("Incident Reports in Different Days of the Week") +
  xlab("Days") + ylab("Number of Incidents")+ labs(color = "City")+
  scale_color_manual(values=c("#999999", "#E69F00","#56B4E9"))+
  theme( plot.title = element_text(color = "steel blue", size = 20, face = "bold"),axis.text=element_text(size=12),axis.title=element_text(color = "steel blue",size=14,face="bold"))
days_plot


analysis<-table(data$Analysis.Neighborhood)
analysis<-as.data.frame(analysis)
analysis<-analysis[-c(1,25),]
ana<- analysis[order(analysis$Freq, decreasing= T), ]
ana<-ana[c(31:41),]

ggplot(data=ana, aes(x=reorder(Var1,Freq),y=Freq)) +
  geom_bar(stat="identity", fill = "orange")+
  xlab("Neighborhoods") + ylab("Number of incident reports") +
  coord_flip()+
  theme( plot.title = element_text(color = "steelblue", size = 20, face = "bold"),
         axis.text=element_text(size=12),axis.title=element_text(color = "steelblue",size=14,face="bold")) +
  ggtitle("Neighborhoods With Least Number Of Incidents")

reporttype<-table(data$Report.Type.Description)
reporttype<-as.data.frame(reporttype)

ggplot(data=reporttype, aes(x=reorder(Var1,Freq),y=Freq)) +
  geom_bar(stat="identity", fill = "limegreen")+
  xlab("Report Type") + ylab("Number of incident reports") +
  theme( plot.title = element_text(color = "steelblue", size = 20, face = "bold"),
         axis.text=element_text(size=12),axis.title=element_text(color = "steelblue",size=14,face="bold")) +
  ggtitle("Type of Reports")



