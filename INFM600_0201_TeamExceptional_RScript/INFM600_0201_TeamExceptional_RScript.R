# INFM600 Information Environment[Team Exceptional]
# Document: R Script
# Date: 16 November 2016

# Document Structure: We have three research questions. This document contains three separate scripts for each question.

# Research Question:[1] Does the change in temperature have an effect on the crime rate?
# Description: We are analyzing the trend of crime rate with respect to the changes in the temperature. Simple linear regression model is
#	           applied to figure out whether there is a relationship between the crime rate and the temperature.
# Dataset/s: Weather_Crime_Incidents2011-15 
# NOTE: This dataset includes the details of the crimes as well as the weather details of that day. This was created from two separate 
#		datasets in the data-cleaning phase
# Columns/Attributes used: REPORTDATENAME, OFFENSE,START_DATE,MaxTemp,MinTemp,RangeTemp
# Packages: plyr, ggplot2

#Reading the dataset into RStudio
data=read.csv(file="Weather_Crime_Incidents2011-15.csv", header=TRUE, sep=",")

#Load support libraries
# Load rename function
library(plyr)
# Load plot function
library(ggplot2)


#Part 1: Check the relationship between the frequency of all cases and temperature (Max Temp and Min Temp).
#Mean and standard deviation of max temperature
mean(data$MaxTemp)
sd(data$MaxTemp)
#Mean and standard deviation of min temperature
mean(data$MinTemp)
sd(data$MinTemp)
# Draw line chart
plot(table(data$MaxTemp),type="l",main= "The distribution of cases happened between 2011 and 2015", xlab="Temperature in degrees Fahrenheit", ylab="Number of cases",col="red")
lines(table(data$MinTemp),type="l",col="blue",lty=2)
legend('topright',  legend=c("Max Temperature", "Min Temperature"),col=c("red", "blue"), lty=1:2, cex=0.8)


# Classify the data of 2011-2015 according to the type of cases.
#ARSON
arson=subset(data,OFFENSE=="ARSON")
#Count the frequency of the type of cases
nrow(arson)
#ASSUT
assault=subset(data,OFFENSE=="ASSAULT W/DANGEROUS WEAPON")
#Count the frequency of the type of cases
nrow(assault)
#BURGLARY
burglary=subset(data,OFFENSE=="BURGLARY")
#Count the frequency of the type of cases
nrow(burglary)
#HOMICIDE
homicide=subset(data,OFFENSE=="HOMICIDE")
#Count the frequency of the type of cases
nrow(homicide)
#MOTOR VEHICLE THEFT
mvt=subset(data,OFFENSE=="MOTOR VEHICLE THEFT")
#Count the frequency of the type of cases
nrow(mvt)
#ROBBERY
robbery=subset(data,OFFENSE=="ROBBERY")
#Count the frequency of the type of cases
nrow(robbery)
#SEX ABUSE
sa=subset(data,OFFENSE=="SEX ABUSE")
#Count the frequency of the type of cases
nrow(sa)
#THEFT FROM AUTO
thefa=subset(data,OFFENSE=="THEFT F/AUTO")
#Count the frequency of the type of cases
nrow(thefa)
#THEFT FROM OTHER
thefot=subset(data,OFFENSE=="THEFT/OTHER")
#Count the frequency of the type of cases
nrow(thefot)


#Part 2: Compare the impact of the range of temperature on the occurrence rate of all types of cases.
#Independent variable: Range of temperature of each day (interval)
#Dependent varibale: The frequency of each type of cases happened (ratio)
#Find the frequency of each type of cases from the data of 2011-2015 corresponding to each numerical value in "range of temperature" column.

#ARSON
rangeT_arson=table(arson$RangeTemp)
rangeT_arson=as.data.frame(rangeT_arson)
rangeT_arson$Var1=as.numeric(as.character(rangeT_arson$Var1))
rangeT_arson=rename(rangeT_arson, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#ASSAULT
rangeT_assault=table(assault$RangeTemp)
rangeT_assault=as.data.frame(rangeT_assault)
rangeT_assault$Var1=as.numeric(as.character(rangeT_assault$Var1))
rangeT_assault=rename(rangeT_assault, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#BURGLARY
rangeT_burglary=table(burglary$RangeTemp)
rangeT_burglary=as.data.frame(rangeT_burglary)
rangeT_burglary$Var1=as.numeric(as.character(rangeT_burglary$Var1))
rangeT_burglary=rename(rangeT_burglary, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#HOMICIDE
rangeT_homicide=table(homicide$RangeTemp)
rangeT_homicide=as.data.frame(rangeT_homicide)
rangeT_homicide$Var1=as.numeric(as.character(rangeT_homicide$Var1))
rangeT_homicide=rename(rangeT_homicide, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#MOTOR VEHICLE THEFT
rangeT_mvt=table(mvt$RangeTemp)
rangeT_mvt=as.data.frame(rangeT_mvt)
rangeT_mvt$Var1=as.numeric(as.character(rangeT_mvt$Var1))
rangeT_mvt=rename(rangeT_mvt, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#ROBBERY
rangeT_robbery=table(robbery$RangeTemp)
rangeT_robbery=as.data.frame(rangeT_robbery)
rangeT_robbery$Var1=as.numeric(as.character(rangeT_robbery$Var1))
rangeT_robbery=rename(rangeT_robbery, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#SEX ABUSE
rangeT_sa=table(sa$RangeTemp)
rangeT_sa=as.data.frame(rangeT_sa)
rangeT_sa$Var1=as.numeric(as.character(rangeT_sa$Var1))
rangeT_sa=rename(rangeT_sa, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#THEFT FROM AUTO
rangeT_thefa=table(thefa$RangeTemp)
rangeT_thefa=as.data.frame(rangeT_thefa)
rangeT_thefa$Var1=as.numeric(as.character(rangeT_thefa$Var1))
rangeT_thefa=rename(rangeT_thefa, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))
#THEFT FROM OTHER
rangeT_thefot=table(thefot$RangeTemp)
rangeT_thefot=as.data.frame(rangeT_thefot)
rangeT_thefot$Var1=as.numeric(as.character(rangeT_thefot$Var1))
rangeT_thefot=rename(rangeT_thefot, c("Var1"="Range.of.Temp", "Freq"="NumberofCases"))

#Draw histogram

#ARSON
hist(rangeT_arson$Range.of.Temp,main= "The distribution of 'ARSON' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#ASSAULT
hist(rangeT_assault$Range.of.Temp,main= "The distribution of 'ASSAULT' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#BURGLARY
hist(rangeT_burglary$Range.of.Temp,main= "The distribution of 'BURGLARY' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#HOMICIDE
hist(rangeT_homicide$Range.of.Temp,main= "The distribution of 'HOMICIDE' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#MOTOR VEHICLE THEFT
hist(rangeT_mvt$Range.of.Temp,main= "The distribution of 'MOTOR VEHICLE THEFT' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#ROBBERY
hist(rangeT_robbery$Range.of.Temp,main= "The distribution of 'ROBBERY' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#SEX ABUSE
hist(rangeT_sa$Range.of.Temp,main= "The distribution of 'SEX ABUSE' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#THEFT FROM AUTO
hist(rangeT_thefa$Range.of.Temp,main= "The distribution of 'THEFT FROM AUTO' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 
#THEFT FROM OTHER
hist(rangeT_thefot$Range.of.Temp,main= "The distribution of 'THEFT FROM OTHER' cases happened between 2011 and 2015", xlab="Range of temperature in degrees Fahrenheit", ylab="Number of cases") 


#Part 3: Compare the impact of max temperature on the occurrence rate of all types of cases.
#Find the frequency of each type of cases from the data of 2011-2015 corresponding to each numerical value in "max temperature" column.
#Independent varibale: Max Temprature of each day from 2011 to 2015 (interval)
#Dependent varibale: The frequency of each type of cases happened (ratio)

#ARSON
#Find out the max temperature on the day when this type of cases happened, and count the frequency of the same type of cases corresponding to each temperature.
maxT_arson=table(arson$MaxTemp)
maxT_arson=as.data.frame(maxT_arson)
#Save the value of temperature and  frequency of the type of cases in a new data frame
maxT_arson$Var1=as.numeric(as.character(maxT_arson$Var1))
#Rename columns' names
maxT_arson=rename(maxT_arson, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#ASSAULT
maxT_assault=table(assault$MaxTemp)
maxT_assault=as.data.frame(maxT_assault)
maxT_assault$Var1=as.numeric(as.character(maxT_assault$Var1))
maxT_assault=rename(maxT_assault, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#BURGLARY
maxT_burglary=table(burglary$MaxTemp)
maxT_burglary=as.data.frame(maxT_burglary)
maxT_burglary$Var1=as.numeric(as.character(maxT_burglary$Var1))
maxT_burglary=rename(maxT_burglary, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#HOMICIDE
maxT_homicide=table(homicide$MaxTemp)
maxT_homicide=as.data.frame(maxT_homicide)
maxT_homicide$Var1=as.numeric(as.character(maxT_homicide$Var1))
maxT_homicide=rename(maxT_homicide, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#MOTOR VEHICLE THEFT
maxT_mvt=table(mvt$MaxTemp)
maxT_mvt=as.data.frame(maxT_mvt)
maxT_mvt$Var1=as.numeric(as.character(maxT_mvt$Var1))
maxT_mvt=rename(maxT_mvt, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#ROBBERY
maxT_robbery=table(robbery$MaxTemp)
maxT_robbery=as.data.frame(maxT_robbery)
maxT_robbery$Var1=as.numeric(as.character(maxT_robbery$Var1))
maxT_robbery=rename(maxT_robbery, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#SEX ABUSE
maxT_sa=table(sa$MaxTemp)
maxT_sa=as.data.frame(maxT_sa)
maxT_sa$Var1=as.numeric(as.character(maxT_sa$Var1))
maxT_sa=rename(maxT_sa, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#THEFT FROM AUTO
maxT_thefa=table(thefa$MaxTemp)
maxT_thefa=as.data.frame(maxT_thefa)
maxT_thefa$Var1=as.numeric(as.character(maxT_thefa$Var1))
maxT_thefa=rename(maxT_thefa, c("Var1"="MaxTemp", "Freq"="NumberofCases"))
#THEFT FROM OTHER
maxT_thefot=table(thefot$MaxTemp)
maxT_thefot=as.data.frame(maxT_thefot)
maxT_thefot$Var1=as.numeric(as.character(maxT_thefot$Var1))
maxT_thefot=rename(maxT_thefot, c("Var1"="MaxTemp", "Freq"="NumberofCases"))

#Use "simple linear regression model" to analyze the relationship between IV and DV,because IV is interval,and DV is ratio.

#ARSON
summary(lm(NumberofCases~MaxTemp,data=maxT_arson))
#ASSAULT
summary(lm(NumberofCases~MaxTemp,data=maxT_assault))
#BURGLARY
summary(lm(NumberofCases~MaxTemp,data=maxT_burglary))
#HOMICIDE
summary(lm(NumberofCases~MaxTemp,data=maxT_homicide))
#MOTOR VEHICLE THEFT
summary(lm(NumberofCases~MaxTemp,data=maxT_mvt))
#ROBBERY
summary(lm(NumberofCases~MaxTemp,data=maxT_robbery))
#SEX ABUSE
summary(lm(NumberofCases~MaxTemp,data=maxT_sa))
#THEFT FROM AUTO
summary(lm(NumberofCases~MaxTemp,data=maxT_thefa))
#THEFT FROM OTHER
summary(lm(NumberofCases~MaxTemp,data=maxT_thefot))

# Draw scatterplots

#ARSON
ggplot(maxT_arson,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'ARSON' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#ASSAULT
ggplot(maxT_assault,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'ASSAULT' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#BURGLARY
ggplot(maxT_burglary,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'BURGLARY' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#HOMICIDE
ggplot(maxT_homicide,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'HOMICIDE' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#MOTOR VEHICLE THEFT
ggplot(maxT_mvt,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'MOTOR VEHICLE THEFT' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#ROBBERY
ggplot(maxT_robbery,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'ROBBERY' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#SEX ABUSE
ggplot(maxT_sa,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'SEX ABUSE' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#THEFT FROM AUTO
ggplot(maxT_thefa,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between number of 'THEFT FROM AUTO' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")
#THEFT FROM OTHER
ggplot(maxT_thefot,aes(MaxTemp,NumberofCases))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(title = "The relationship between  number of 'THEFT FROM OTHER' cases happened and average temperature from 2011 to 2015", x="Temperature in degrees Fahrenheit", y="Number of cases")

# End of R Script for Research Question [1]



# Research Question:[2]	Is there a relation between the number of nightclubs with the crime rate in a particular geographic unit (cluster)?
# Description: We are analyzing the crime rate(number of crimes) for each cluster with respect to the number of nightclubs in each cluster. Moreover, we will be looking into 
# 			   different types of crimes (Homicide, Burglary, Sexual Assault, etc.) We will be running the correlation test for each type of crime to figure out if there is a 
#			   relationship between the number of nightclubs and occurence of crime
# Datasets: (1) Crime_Incidents_year(2011-2015)- all details about crimes for each year
#			(2) NightClub_Cluster- list of nightclubs classified by cluster id
# NOTE: The above mentioned datasets are the ones we created after data-cleaning phase.
# Columns/Attributes used: NEIGHBORHOODCLUSTER, OFFENCE in dataset(1), NEIGHBORHOODCLUSTER, NUMBER_CLUBS in dataset(2)
# Packages: No extra packages are used

# [Step 0.0] Opening each dataset in RStudio

Crime_Incidents__2011 <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/Data/Crime_Incidents__2011.csv")
View(Crime_Incidents__2011)
Crime_Incidents__2012 <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/Data/Crime_Incidents__2012.csv")
View(Crime_Incidents__2012)
Crime_Incidents__2013 <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/Data/Crime_Incidents__2013.csv")
View(Crime_Incidents__2013)
Crime_Incidents__2014 <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/Data/Crime_Incidents__2014.csv")
View(Crime_Incidents__2014)
Crime_Incidents__2015 <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/Data/Crime_Incidents__2015.csv")
View(Crime_Incidents__2015)
club.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/Data/NightClub_Cluster.csv")
View(club.clusterwise)

# [Step 0.1] Merging the crime data for all years 2011-2015 into one dataset

merged.dataset=rbind(Crime_Incidents__2011,Crime_Incidents__2012)
merged.dataset=rbind(merged.dataset,Crime_Incidents__2013)
merged.dataset=rbind(merged.dataset,Crime_Incidents__2014)
merged.dataset=rbind(merged.dataset,Crime_Incidents__2015)

# Crime Type: ALL
# [Step 1] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
total.clusterwise=table(merged.dataset$NEIGHBORHOODCLUSTER)
write.csv(total.clusterwise,file="total.clusterwise.csv")

# [Step 2] Reading in the file created in above step and renaming the columns for better understandability 
total.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/total.clusterwise.csv", row.names=1)
View(total.clusterwise)
names(total.clusterwise)=c("Cluster","Number.Crimes")

# [Step 3.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
total.cluster.mean=mean(total.clusterwise$Number.Crimes)

# [Step 3.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
total.cluster.range=range(total.clusterwise$Number.Crimes)

# [Step 3.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
total.cluster.sd=sd(total.clusterwise$Number.Crimes)


# Crime Type: ARSON
# [Step 1] Creating a subset of those rows which describe crimes of type:ARSON 
arson=subset(merged.dataset,merged.dataset$OFFENSE=="ARSON")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
arson.clusterwise=table(arson$NEIGHBORHOODCLUSTER)
write.csv(arson.clusterwise,file="arson.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
arson.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/arson.clusterwise.csv", row.names=1)
View(arson.clusterwise)
names(arson.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
arson.cluster.mean=mean(arson.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
arson.cluster.range=range(arson.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
arson.cluster.sd=sd(arson.clusterwise$Number.Crimes)


# Crime Type: ASSAULT W/DANGEROUS WEAPON
# [Step 1] Creating a subset of those rows which describe crimes of type:ASSAULT W/DANGEROUS WEAPON 
assault=subset(merged.dataset,merged.dataset$OFFENSE=="ASSAULT W/DANGEROUS WEAPON")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
assault.clusterwise=table(assault$NEIGHBORHOODCLUSTER)
write.csv(assault.clusterwise,file="assault.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
assault.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/assault.clusterwise.csv", row.names=1)
View(assault.clusterwise)
names(assault.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
assault.cluster.mean=mean(assault.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
assault.cluster.range=range(assault.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
assault.cluster.sd=sd(assault.clusterwise$Number.Crimes)


# Crime Type: BURGLARY
# [Step 1] Creating a subset of those rows which describe crimes of type:BURGLARY 
burglary=subset(merged.dataset,merged.dataset$OFFENSE=="BURGLARY")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
burglary.clusterwise=table(burglary$NEIGHBORHOODCLUSTER)
write.csv(burglary.clusterwise,file="burglary.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
burglary.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/burglary.clusterwise.csv", row.names=1)
View(burglary.clusterwise)
names(burglary.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
burglary.cluster.mean=mean(burglary.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
burglary.cluster.range=range(burglary.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
burglary.cluster.sd=sd(burglary.clusterwise$Number.Crimes)


# Crime Type: HOMICIDE
# [Step 1] Creating a subset of those rows which describe crimes of type:HOMICIDE 
homicide=subset(merged.dataset,merged.dataset$OFFENSE=="HOMICIDE")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
homicide.clusterwise=table(homicide$NEIGHBORHOODCLUSTER)
write.csv(homicide.clusterwise,file="homicide.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
homicide.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/homicide.clusterwise.csv", row.names=1)
View(homicide.clusterwise)
names(homicide.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
homicide.cluster.mean=mean(homicide.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
homicide.cluster.range=range(homicide.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
homicide.cluster.sd=sd(homicide.clusterwise$Number.Crimes)


# Crime Type: MOTOR VEHICLE THEFT
# [Step 1] Creating a subset of those rows which describe crimes of type:MOTOR VEHICLE THEFT 
motor.theft=subset(merged.dataset,merged.dataset$OFFENSE=="MOTOR VEHICLE THEFT")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
motor.theft.clusterwise=table(motor.theft$NEIGHBORHOODCLUSTER)
write.csv(motor.theft.clusterwise,file="motor.theft.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
motor.theft.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/motor.theft.clusterwise.csv", row.names=1)
View(motor.theft.clusterwise)
names(motor.theft.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
motor.theft.cluster.mean=mean(motor.theft.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
motor.theft.cluster.range=range(motor.theft.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
motor.theft.cluster.sd=sd(motor.theft.clusterwise$Number.Crimes)


# Crime Type: ROBBERY
# [Step 1] Creating a subset of those rows which describe crimes of type:ROBBERY 
robbery=subset(merged.dataset,merged.dataset$OFFENSE=="ROBBERY")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
robbery.clusterwise=table(robbery$NEIGHBORHOODCLUSTER)
write.csv(robbery.clusterwise,file="robbery.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
robbery.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/robbery.clusterwise.csv", row.names=1)
View(robbery.clusterwise)
names(robbery.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
robbery.cluster.mean=mean(robbery.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
robbery.cluster.range=range(robbery.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
robbery.cluster.sd=sd(robbery.clusterwise$Number.Crimes)


# Crime Type: SEX ABUSE
# [Step 1] Creating a subset of those rows which describe crimes of type:SEX ABUSE 
sex.abuse=subset(merged.dataset,merged.dataset$OFFENSE=="SEX ABUSE")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
sex.abuse.clusterwise=table(sex.abuse$NEIGHBORHOODCLUSTER)
write.csv(sex.abuse.clusterwise,file="sex.abuse.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
sex.abuse.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/sex.abuse.clusterwise.csv", row.names=1)
View(sex.abuse.clusterwise)
names(sex.abuse.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
sex.abuse.cluster.mean=mean(sex.abuse.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
sex.abuse.cluster.range=range(sex.abuse.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
sex.abuse.cluster.sd=sd(sex.abuse.clusterwise$Number.Crimes)


# Crime Type: THEFT F/AUTO
# [Step 1] Creating a subset of those rows which describe crimes of type:THEFT F/AUTO 
theft.auto=subset(merged.dataset,merged.dataset$OFFENSE=="THEFT F/AUTO")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
theft.auto.clusterwise=table(theft.auto$NEIGHBORHOODCLUSTER)
write.csv(theft.auto.clusterwise,file="theft.auto.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
theft.auto.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/theft.auto.clusterwise.csv", row.names=1)
View(theft.auto.clusterwise)
names(theft.auto.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
theft.auto.cluster.mean=mean(theft.auto.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
theft.auto.cluster.range=range(theft.auto.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
theft.auto.cluster.sd=sd(theft.auto.clusterwise$Number.Crimes)


# Crime Type: THEFT/OTHER
# [Step 1] Creating a subset of those rows which describe crimes of type:THEFT/OTHER 
theft.other=subset(merged.dataset,merged.dataset$OFFENSE=="THEFT/OTHER")

# [Step 2] Creating a frequency table to get the number of crimes for each cluster and writing it to a csv file for recordkeeping
theft.other.clusterwise=table(theft.other$NEIGHBORHOODCLUSTER)
write.csv(theft.other.clusterwise,file="theft.other.clusterwise.csv")

# [Step 3] Reading in the file created in above step and renaming the columns for better understandability 
theft.other.clusterwise <- read.csv("C:/Users/PAL/Desktop/MIM/INFM600/Project/3.RScript/AnalysisScript/theft.other.clusterwise.csv", row.names=1)
View(theft.other.clusterwise)
names(theft.other.clusterwise)=c("Cluster","Number.Crimes")

# [Step 4.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
theft.other.cluster.mean=mean(theft.other.clusterwise$Number.Crimes)

# [Step 4.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
theft.other.cluster.range=range(theft.other.clusterwise$Number.Crimes)

# [Step 4.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
theft.other.cluster.sd=sd(theft.other.clusterwise$Number.Crimes)


#Clubs

# [Step 0] The relevant dataset is already open (see Step 0.0 at the beginning of the script). Here we rename the columns for better understandability
names(club.clusterwise)=c("Cluster","Number.Clubs")

# [Step 1.0] Calculating the average number of crimes occuring in a cluster to be able to understand our data better
club.cluster.mean=mean(club.clusterwise$Number.Clubs)

# [Step 1.1] Calculating the maximum and minimum number of crimes occuring in a cluster to figure out the cluster with the highest and lowest number of crimes
club.cluster.range=range(club.clusterwise$Number.Clubs)

# [Step 1.2] Calculating the standard deviation of crimes occuring in a cluster to get an idea about the variance seen in our data
club.cluster.sd=sd(club.clusterwise$Number.Clubs)

# Correlation tests: For figuring out if there is a relationship between the number of nightclubs and number of crimes

# Crime Type: ALL

# [Step 1] Merging the crime dataset with nightclubs dataset
club.total.cluster=merge(club.clusterwise,total.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.total.cluster$Number.Clubs,club.total.cluster$Number.Crimes)


# Crime Type: ARSON

# [Step 1] Merging the crime dataset with nightclubs dataset
club.arson.cluster=merge(club.clusterwise,arson.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.arson.cluster$Number.Clubs,club.arson.cluster$Number.Crimes)


# Crime Type: ASSAULT W/DANGEROUS WEAPON

# [Step 1] Merging the crime dataset with nightclubs dataset
club.assault.cluster=merge(club.clusterwise,assault.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.assault.cluster$Number.Clubs,club.assault.cluster$Number.Crimes)


# Crime Type: BURGLARY

# [Step 1] Merging the crime dataset with nightclubs dataset
club.burglary.cluster=merge(club.clusterwise,burglary.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.burglary.cluster$Number.Clubs,club.burglary.cluster$Number.Crimes)


# Crime Type: HOMICIDE

# [Step 1] Merging the crime dataset with nightclubs dataset
club.homicide.cluster=merge(club.clusterwise,homicide.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.homicide.cluster$Number.Clubs,club.homicide.cluster$Number.Crimes)


# Crime Type: MOTOR VEHICLE THEFT

# [Step 1] Merging the crime dataset with nightclubs dataset
club.motor.theft.cluster=merge(club.clusterwise,motor.theft.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.motor.theft.cluster$Number.Clubs,club.motor.theft.cluster$Number.Crimes)


# Crime Type: ROBBERY

# [Step 1] Merging the crime dataset with nightclubs dataset
club.robbery.cluster=merge(club.clusterwise,robbery.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.robbery.cluster$Number.Clubs,club.robbery.cluster$Number.Crimes)


# Crime Type: SEX ABUSE

# [Step 1] Merging the crime dataset with nightclubs dataset
club.sex.abuse.cluster=merge(club.clusterwise,sex.abuse.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.sex.abuse.cluster$Number.Clubs,club.sex.abuse.cluster$Number.Crimes)


# Crime Type: THEFT F/AUTO

# [Step 1] Merging the crime dataset with nightclubs dataset
club.theft.auto.cluster=merge(club.clusterwise,theft.auto.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.theft.auto.cluster$Number.Clubs,club.theft.auto.cluster$Number.Crimes)


# Crime Type: THEFT/OTHER

# [Step 1] Merging the crime dataset with nightclubs dataset
club.theft.other.cluster=merge(club.clusterwise,theft.other.clusterwise,by="Cluster")

# [Step 2] Running the correlation test (linear regression model) between the number of clubs and number of crimes in a cluster
cor.test(club.theft.other.cluster$Number.Clubs,club.theft.other.cluster$Number.Crimes)

# End of RScript for Research Question [2]



# Research Question:[3]	Is there a spike in the crime rate during major public holidays?
# Description: We are analyzing the crime rate during the public holidays to check if there is a spike. We are further drilling down to specific types of crimes (eg: burgary, theft, etc) 
#			   to figure out trends. Simple t-tests are carried out for basic analysis.
# Datasets: (1) Crime_Incidents_year(2011-2015)- all details about crimes for each year
#			(2) Holidays- list of public holidays for United States of  America for the years (2011-2015	)
# NOTE: The above mentioned datasets are the ones we created after data-cleaning phase.
# Columns/Attributes used: REPORTDATETIME, START_DATE, END_DATE in dataset (1), Date in dataset(2)
# Packages: dplyr, tidyr

#loading all datasets
Crime_Data_2011 <- read.csv("~/INFM600/Crime_Data_2011.csv")
Crime_Data_2012 <- read.csv("~/INFM600/Crime_Data_2012.csv")
Crime_Data_2013 <- read.csv("~/INFM600/Crime_Data_2013.csv")
Crime_Data_2014 <- read.csv("~/INFM600/Crime_Data_2014.csv")
Crime_Data_2015 <- read.csv("~/INFM600/Crime_Data_2015.csv")

#Appending all datasets into one
FinalDataSet <- rbind.data.frame(Crime_Data_2011, Crime_Data_2012, Crime_Data_2013, Crime_Data_2014, Crime_Data_2015)
View(FinalDataSet)

#Segregating the REPORTDATETIME column into Date and Time columns by using the library tidyr
library(tidyr)
FinalDataSet <- separate(FinalDataSet, REPORTDATETIME, c("Date", "Time"), sep = "T")
View(FinalDataSet)

#Converting Date column to proper format
class(FinalDataSet$Date)
FinalDataSet$Date <- as.Date(FinalDataSet$Date)
class(FinalDataSet$Date)

#Segregating the START_DATE AND END_DATE column, having timestamp into StartDate and EndDate columns without Timestamp
FinalDataSet <- separate(FinalDataSet, START_DATE , c("StartDate", "Time1"), sep = "T")
FinalDataSet <- separate(FinalDataSet, END_DATE, c("EndDate", "Time2"), sep = "T")
View(FinalDataSet)

#Converting the StartDate and EndDate into Date format
FinalDataSet$StartDate <- as.Date(FinalDataSet$StartDate)
FinalDataSet$EndDate <- as.Date(FinalDataSet$EndDate)

#Calculating the duration of crime
FinalDataSet$DurationInDays <- FinalDataSet$EndDate - FinalDataSet$StartDate
View(FinalDataSet)
class(FinalDataSet$DurationInDays)
FinalDataSet$DurationNumeric <- as.numeric(FinalDataSet$DurationInDays)
View(FinalDataSet)
FinalDataSet$DurationInDays <- NULL
View(FinalDataSet)
hist(FinalDataSet$DurationNumeric)

#writing the FinalDataSet as a csv file
write.csv(FinalDataSet, file = "HolidayCrimeData.csv")

#removing all the unnecessary columns from the dataset
FinalDataSet$X <- NULL
FinalDataSet$Y <- NULL
FinalDataSet$OBJECTID <- NULL
FinalDataSet$CCN <- NULL
FinalDataSet$BLOCKXCOORD <- NULL
FinalDataSet$BLOCKYCOORD <- NULL
FinalDataSet$ANC <- NULL
FinalDataSet$DISTRICT <- NULL
FinalDataSet$PSA <- NULL
FinalDataSet$BUSINESSIMPROVEMENTDISTRICT <- NULL
FinalDataSet$BLOCK_GROUP <-NULL
FinalDataSet$CENSUS_TRACT <- NULL
FinalDataSet$VOTING_PRECINCT <- NULL
View(FinalDataSet)

#sorting the StartDate of crime in ascending order
FinalDataSet <- FinalDataSet[order(FinalDataSet$StartDate),]
View(FinalDataSet)
min(FinalDataSet$StartDate, na.rm = TRUE)
FinalDataSet <- FinalDataSet[order(FinalDataSet$DurationNumeric),]
View(FinalDataSet)

#Getting a summary of basic descriptive statistics 
summary(FinalDataSet$DurationNumeric)
#the value of duration of crimes range from -1659 days to 328700 days with dates from the year 1800 to 2911
#indicating that there are a lot of incorrect data

#Removing the outliers and keeping only the relevant values
#Keeping only those crime details for which duration was less than 7 days
FinalDataSet <- FinalDataSet[FinalDataSet$DurationNumeric <= 7 & FinalDataSet$DurationNumeric >= 0 & FinalDataSet$EndDate > as.Date("2011-01-01"),]
View(FinalDataSet)
write.csv(FinalDataSet, file = "FinalDataSet.csv", row.names = FALSE)

#removing the missing values
FinalDataSet <- na.omit(FinalDataSet)
dim(FinalDataSet)
table(FinalDataSet$OFFENSE)

#grouping the days into weeks
FinalDataSet$WeekNo <- format(FinalDataSet$EndDate, "%U")
FinalDataSet$WeekNo <- as.numeric(FinalDataSet$WeekNo)
class(FinalDataSet$WeekNo)

#importing the holidays dataset
Holidays <- read.csv(file.choose())
class(Holidays$Date)
Holidays$Date <- as.Date(Holidays$Date)
Holidays$HolidayWeek <- format(Holidays$Date, "%U")
class(Holidays$HolidayWeek)

#converting HolidaysWeek to numeric
Holidays$HolidayWeek <- as.numeric(Holidays$HolidayWeek)


##Creating a function for conducting t-tests and using the library dplyr
HolidayVsNonHoliday <- function(x, y = Holidays){
  library(dplyr)
  w <- semi_join(x, y, by = "WeekNo" )
  w <- as.data.frame(table(w$WeekNo))
  z <- anti_join(x, y, by = "WeekNo")
  z <- as.data.frame(table(z$WeekNo))
  t.test(w$Freq, z$Freq)
}

#subsetting different categories of crime
table(FinalDataSet$OFFENSE)
ArsonSet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "ARSON")
AssaultSet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "ASSAULT W/DANGEROUS WEAPON")
BurglarySet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "BURGLARY")
HomicideSet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "HOMICIDE")
MotorTheftSet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "MOTOR VEHICLE THEFT")
RobberySet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "ROBBERY")
SexAbuseSet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "SEX ABUSE")
TheftSet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "THEFT/OTHER")
TheftAutoSet <- subset(FinalDataSet, FinalDataSet$OFFENSE == "THEFT F/AUTO")

#conducting t-tests on subsets
HolidayVsNonHoliday(ArsonSet)
HolidayVsNonHoliday(FinalDataSet)
HolidayVsNonHoliday(AssaultSet)
HolidayVsNonHoliday(BurglarySet)
HolidayVsNonHoliday(HomicideSet)
HolidayVsNonHoliday(MotorTheftSet)
HolidayVsNonHoliday(RobberySet)
HolidayVsNonHoliday(SexAbuseSet)
HolidayVsNonHoliday(TheftSet)
HolidayVsNonHoliday(TheftAutoSet)

#function for plotting different types of crime
plotcrime <- function(x){
  x <- table(x$WeekNo)
  plot(x, type = "l", col = "blue", lwd = 2)
  abline(v = Holidays$WeekNo, col = "red", lwd = 2)
}

#making plots
plotcrime(FinalDataSet)
plotcrime(ArsonSet)
plotcrime(AssaultSet)
plotcrime(SexAbuseSet)
plotcrime(RobberySet)
plotcrime(BurglarySet)
plotcrime(TheftAutoSet)
plotcrime(TheftSet)

# End of RScript for Research Question [3]

# INFM600 Information Environment[Team Exceptional]
# Date Updated: 16 November 2016. 10:42:56AM