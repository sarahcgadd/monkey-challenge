#install.packages("lubridate")
#install.packages("ggplot2")
library(lubridate)
library(ggplot2)
set.seed(1532)

info<-read.csv("full tree info.csv")
#info columns indicate the following
#month: month of the year
#tree: tree id number
#flower: if flowers are present
#fruit: if fruit is present
#leaf: if the monkeys are eating the leaves on this tree at this time (when there aren't many tasty flowers or fruits available)
#tasty.fl: how tasty the flowers are
#tasty.fr: how tasty the fruit is
#leaf.diam: the diameter of the leaves
#leaf.coverage: percentage of 1m2 covered by leaves in the canopy (how well it prevents rainfall)

#sample dates from 2 years on which there are monkey counts
dates<-as.data.frame(sample(seq(as.Date('2016/01/01'),as.Date('2017/12/31'), by="day"), 500))
colnames(dates)<-"Date"

#gen sequence of dates
Date<-seq(as.Date('2016/01/01'),as.Date('2017/12/31'), by="day")
#generate rain variable
rain<-rlnorm(length(Date), 0, 0.6)
rain<-data.frame(Date,rain)

#merge rain values with the monkey count dates
data<-merge(dates, rain, by="Date", all.x=T)

#generate month variable
data$Month<-month(data$Date)
#merge rain data with the tree information
data<-merge(data, info, by="Month")
#use a model to generate monkey count data for each tree
#Monkeys prefer fruit over flowers over leaves
#Monkeys only eat leaves when fruit and flowers are not available
#Monkeys prefer smaller leaves to eat
#Monkeys shelter where thereis larger leaf coverage
data$count<-round(1.5*(2*data$Flower*data$Tasty.Fl+ 
                         3*data$Fruit*data$Tasty.Fr+
                         2*data$Leaf/data$leaf.diam+
                         0.01*data$leaf.coverage*data$rain))+
  rpois(n=nrow(data), 0.5)

#add missing count data
data$count[sample(1:length(data$count), 300)]<-NA
#add some counts that are way too high
samp<-sample(1:length(data$count), 70)
data$count[samp]<-data$count[samp]*5
#add some missing rain data
rain$rain[sample(1:length(rain$rain), 40)]<-NA
#add some rain that is too high
samp<-sample(1:length(rain$rain), 15)
rain$rain[samp]<-rain$rain[samp]*10

#keep only the columns needed for the challenge
datakeep<-data[,c("Date","Tree","count")]
datakeep<-datakeep[order(datakeep$Date, datakeep$Tree),]
#name data columns
colnames(rain)<-c("Date","Rainfall")
#save the data
write.csv(datakeep, "Monkey_count.csv", row.names=F)
write.csv(rain, "Rain.csv", row.names=F)


