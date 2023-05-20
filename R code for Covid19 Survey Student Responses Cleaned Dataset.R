#Module 6 - Data Analysis Assignment 
#Devi Somalinga Bhuvanesh

install.packages("plyr")  #to install new package 
library(plyr)             #to import the new package in the library

install.packages("FSA")
library(FSA)

install.packages("FSAdata")
library(FSAdata)

install.packages("magrittr")
library(magrittr)

install.packages("dplyr")
library(dplyr)

install.packages("plotrix")
library(plotrix)

install.packages("ggplot2")
library(ggplot2)

install.packages("moments")
library(moments)

install.packages("outliers")
library(outliers)

install.packages("lattice")
library(lattice)

CovidImpactdataset <- read.csv("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 6/Covid19 Survey Student Responses Cleaned Dataset.csv", sep = ",")
CovidImpactdataset

#Descriptive Analysis of overall dataset
summary(CovidImpactdataset)
str(CovidImpactdataset)

#Descriptive statistics for Age of Subject
range(CovidImpactdataset$Age.of.Subject)
var(CovidImpactdataset$Age.of.Subject)
kurtosis(CovidImpactdataset$Age.of.Subject)
skewness(CovidImpactdataset$Age.of.Subject)
outlier(CovidImpactdataset$Age.of.Subject)

#Descriptive for statistics for Time spent on online class
range(CovidImpactdataset$Time.spent.on.Online.Class)
var(CovidImpactdataset$Time.spent.on.Online.Class)
kurtosis(CovidImpactdataset$Time.spent.on.Online.Class)
skewness(CovidImpactdataset$Time.spent.on.Online.Class)
outlier(CovidImpactdataset$Time.spent.on.Online.Class)

#Descriptive for statistics for Time spent on self study class
range(CovidImpactdataset$Time.spent.on.self.study)
var(CovidImpactdataset$Time.spent.on.self.study)
kurtosis(CovidImpactdataset$Time.spent.on.self.study)
skewness(CovidImpactdataset$Time.spent.on.self.study)
outlier(CovidImpactdataset$Time.spent.on.self.study)

#Descriptive for statistics for Time spent on fitness 
range(CovidImpactdataset$Time.spent.on.fitness)
var(CovidImpactdataset$Time.spent.on.fitness)
kurtosis(CovidImpactdataset$Time.spent.on.fitness)
skewness(CovidImpactdataset$Time.spent.on.fitness)
outlier(CovidImpactdataset$Time.spent.on.fitness)

#Descriptive for statistics for Time spent on Sleep
range(CovidImpactdataset$Time.spent.on.sleep)
var(CovidImpactdataset$Time.spent.on.sleep)
kurtosis(CovidImpactdataset$Time.spent.on.sleep)
skewness(CovidImpactdataset$Time.spent.on.sleep)
outlier(CovidImpactdataset$Time.spent.on.sleep)

#Descriptive for statistics for Time spent on Social Media
range(CovidImpactdataset$Time.spent.on.social.media)
var(CovidImpactdataset$Time.spent.on.social.media)
kurtosis(CovidImpactdataset$Time.spent.on.social.media)
skewness(CovidImpactdataset$Time.spent.on.social.media)
outlier(CovidImpactdataset$Time.spent.on.social.media)

#Descriptive for statistics for Number of meals per day
range(CovidImpactdataset$Number.of.meals.per.day)
var(CovidImpactdataset$Number.of.meals.per.day)
kurtosis(CovidImpactdataset$Number.of.meals.per.day)
skewness(CovidImpactdataset$Number.of.meals.per.day)
outlier(CovidImpactdataset$Number.of.meals.per.day)


#Plot1 - Region wise
Regionwise <- table(CovidImpactdataset[2])
Regionwise <- as.data.frame(Regionwise)
Regionwise
pct <- round(100*Regionwise$Freq/sum(Regionwise$Freq))
pct
pie(Regionwise$Freq, 
    labels = paste(Regionwise$Region.of.residence, sep = " ", pct, "%"),
    cex=1, hjust=0.5,
    radius = 0.9,
    col = c("darkblue","lightblue"),
    main = "Region of Residence", 
    col.main = "navyblue", cex.main = 2)


#Plot2 - Age
hist(CovidImpactdataset$Age.of.Subject, 
     col = "dodgerblue3", 
     main = "Age Distribution of Students", 
     xlim = c(0,60), ylim = c(0,600),
     col.main = "navyblue",
     xlab="Age (in years)",
     ylab = "Number of Students",
     cex.axis = 0.9, 
     cex.main = 1.5,
     labels=TRUE, 
     freq = TRUE)
abline(v=mean(CovidImpactdataset$Age.of.Subject),
       col="red",
       lty=2,
       lwd=2)
legend(x = "topright", lwd=2, lty=2, cex = 0.9,
       col= c("red"),text.col = "black", 
       legend="Mean")


plot(density(CovidImpactdataset$Age.of.Subject), xlab="Age (in years)", main ="Density of Students Age", cex.main=1.5, col.main="navyblue")
polygon(density(CovidImpactdataset$Age.of.Subject), col="pink", border="black")
abline(v=mean(CovidImpactdataset$Age.of.Subject),
       col="red",
       lty=2,
       lwd=2)
legend(x = "topright", lwd=2, lty=2, cex = 0.9,
       col= c("red"),text.col = "black", 
       legend="Mean")

#Plot3 - Type of Population
typeofpopulation <- table(CovidImpactdataset$Type.of.Population)
typeofpopulation <- as.data.frame(typeofpopulation)
typeofpopulation <- typeofpopulation[order(typeofpopulation$Freq), ]     #to order the frequency in descending order
typeofpopulation         
barplot(typeofpopulation$Freq,  
        names.arg = typeofpopulation$Var1,
        main = "Type of Population",
        col = "pink",  
        col.main="navyblue",
        las = 1,
        cex.names = 0.8,
        cex.main = 2,
        cex.axis=1,
        horiz = TRUE,
        xlim=c(0,1000),
        xlab = "Number of Students")

#Pareto Chart
par(cex.axis = 0.8)
Populationtype <- c("Child","Adolescent", "Adult")
Numberofstudents <- c(44,258,880)
populationtype <- data.frame(Populationtype,Numberofstudents)
populationtype
library(qcc)
names(Numberofstudents) <- c("Child","Adolescent", "Adult")
pareto.chart(Numberofstudents, 
             main="Type of Population", 
             col=heat.colors(length(populationtype$Numberofstudents)),
             ylab="Number of Students",
             cumperc = seq(0, 100, by = 20),
             las=1)


#Plot4 - Time Spent on Online class Vs Rating

library(lattice)     #to split and plot the data based on each species
dotplot(CovidImpactdataset$Rating.of.Online.Class.experience~CovidImpactdataset$Time.spent.on.Online.Class | CovidImpactdataset$Type.of.Population,
        main="Time Spent on Online Class Vs Rating", 
        col.main="navyblue",
        xlab="Time spent on online class", ylab="Online Class Rating")

par(ask=TRUE)
Ratingofonlineclass <- as.factor(CovidImpactdataset$Rating.of.Online.Class.experience, level=c("Excellent", "Good", "Average", "Poor", "Very poor", "NA"))
Ratingofonlineclass
Rangeooftimespentononlineclass <- as.factor(CovidImpactdataset$Range...Online.Study)
Rangeooftimespentononlineclass
typeofpopulation <- as.factor(CovidImpactdataset$Type.of.Population, level=c("Child", "Adolescents", "Adult"))
typeofpopulation
ggplot(data = CovidImpactdataset, aes(x = factor(CovidImpactdataset$Rating.of.Online.Class.experience, level=c("Excellent", "Good", "Average", "Poor", "Very poor", "NA")), 
                                      fill = typeofpopulation))+
  geom_bar(position='stack', color="black") +
  scale_fill_manual(values = c("#DADAEB", "#9E9AC8", "#6A51A3")) +
  ggtitle("Online Class Experience") +
  labs(x="Online Class Rating",y="Number of Students") +
  theme(plot.title=element_text(hjust=0.5, face="bold",
                                size="14", color="navyblue"),
        axis.title=element_text(face="bold.italic",
                                size=10, color="navyblue"),
        axis.text=element_text(face="bold", size=9,
                               color="black"),
        panel.background=element_rect(fill="white",
                                      color="black"),
        panel.grid.minor.x=element_blank(),
        legend.position="top")

#Plot5 - Time spent on various activites

boxplot(CovidImpactdataset$Time.spent.on.sleep, 
        CovidImpactdataset$Time.spent.on.social.media,
        CovidImpactdataset$Time.spent.on.fitness,
        CovidImpactdataset$Time.spent.on.self.study,
        CovidImpactdataset$Time.spent.on.Online.Class,
        names = c("Sleep", "Social Media", "Fitness", "Self Study", "Online Class"),
        xlab="Time spent in a day", 
        col = rainbow(ncol(CovidImpactdataset)), 
        las = 1,
        main = "Time spent on different activities during Covid",
        col.main ="navyblue",
        cex.main=1,
        lty = 1,
        cex.axis = 0.6,
        horizontal = TRUE)

#Plot5 - Time spent on various activites - Remove outliers
Outliers <- read.csv("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 6/Outlier.csv")
Outliers
boxplot(Outliers$Time.spent.on.sleep,
        Outliers$Time.spent.on.social.media,
        Outliers$Time.spent.on.fitness,
        Outliers$Time.spent.on.self.study,
        Outliers$Time.spent.on.Online.Class,
        names = c("Sleep", "Social Media", "Fitness", "Self Study", "Online Class"),
        xlab="Time spent in a day", 
        col = rainbow(ncol(CovidImpactdataset)), 
        las = 1,
        main = "Time spent on different activities during Covid",
        col.main ="navyblue",
        cex.main=1,
        lty = 1,
        cex.axis = 0.6,
        horizontal = TRUE)

summary(Outliers)

Onlineclass <- read.csv("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 6/Onlineclass.csv")
Onlineclass
range(Onlineclass$Time.spent.on.Online.Class)
skewness(Onlineclass$Time.spent.on.Online.Class)
kurtosis(Onlineclass$Time.spent.on.Online.Class)

Selfstudy <- read.csv("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 6/SelfStudy.csv")
Selfstudy
range(Selfstudy$Time.spent.on.self.study)
skewness(Selfstudy$Time.spent.on.self.study)
kurtosis(Selfstudy$Time.spent.on.self.study)


Fitness <- read.csv("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 6/Fitness.csv")
Fitness
range(Fitness$Time.spent.on.fitness)
skewness(Fitness$Time.spent.on.fitness)
kurtosis(Fitness$Time.spent.on.fitness)

Socialmedia <- read.csv("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 6/Socialmedia.csv")
Socialmedia
range(Socialmedia$Time.spent.on.social.media)
skewness(Socialmedia$Time.spent.on.social.media)
kurtosis(Socialmedia$Time.spent.on.social.media)

Sleep <- read.csv("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 6/Sleep.csv")
Sleep
range(Sleep$Time.spent.on.sleep)
skewness(Sleep$Time.spent.on.sleep)
kurtosis(Sleep$Time.spent.on.sleep)

#Plot 6 - Time Spent on Online Class Vs Self Study

plot(CovidImpactdataset$Time.spent.on.Online.Class, CovidImpactdataset$Time.spent.on.self.study)

library(lattice)    
dotplot(CovidImpactdataset$Time.spent.on.self.study~CovidImpactdataset$Range...Online.Study|CovidImpactdataset$Type.of.Population,
        cex.axis = 0.1,
        main="Type of Species = Fish length Vs Fish Scale", 
        xlab="Total Length", ylab="Scale")

#Plot 7 - Medium of Online Class 
par(mar=c(11,4,4,4))
barchart(CovidImpactdataset$Medium.for.online.class, 
         main="Medium used for online class",
         col = "brown", 
         col.lab="navyblue",
         xlab="Number of students", 
         cex.axis=0.9, 
         cex.name=2, las=2)
 
#Plot 8 - Time of Population vs Preferred social media platform
Preferredsocialmediaplatform <-  as.factor(CovidImpactdataset$Prefered.social.media.platform)
Preferredsocialmediaplatform

Typeofpopulation <- as.factor(CovidImpactdataset$Type.of.Population)
Typeofpopulation

ggplot(data = CovidImpactdataset, aes(x = Preferredsocialmediaplatform, fill = Typeofpopulation))+
  geom_bar(position='stack', color="black") + scale_fill_brewer()+
  ggtitle("Preferred Social Media Platform") +
  labs(x="Social Media Platforms",y="Number of Students") +
  theme(plot.title=element_text(hjust=0.5,face="bold",
                                size="12", color="navyblue"),
        axis.title=element_text(face="bold",
                                size=10, color="navyblue"),
        axis.text=element_text(face="bold", size=9,
                               color="black"),
        panel.background=element_rect(fill="white",
                                      color="black"),
        panel.grid.minor.x=element_blank(),
        legend.position="top")+
  coord_flip()


#Plot 9 - Meals per day - Type of Population

Numberofmeals <- c(1,2,3,4,5,6,7,8)
Numberofmeals

Adolescents <- c(18,64,123,50,2,0,1,0)
Adolescents

Adult <- c(20,212,471,165,6,2,3,1)
Adult

Child <-  c(3,11,16,14,0,0,0,0)
Child

barplot(cbind(Child,Adolescents,Adult) ~ Numberofmeals,
        beside = TRUE,
        xlab = "Meals consumed per day", 
        ylim = c(0,500), 
        legend.text = c("Child", "Adolescent", "Adult"),
        col=c("pink","lightblue","navyblue"), 
        ylab="Number of Students",
        main="Number of Meals taken on a daily basis", col.main="navyblue",
        cex.axis=1, las=1)


#Plot 9.1 

Numberofmeals <- c(1,2,3,4,5,6,7,8)
Numberofmeals
Decreasedweight <- c(15,70,93,28,1,1,1,0)
Decreasedweight
Increasedweight <- c(14,91,223,101,4,1,3,1)
Increasedweight
Remainconstantweight <- c(12,126,294,100,3,0,0,0)
Remainconstantweight
plot(Decreasedweight, type = "o", pch=20, col = "red", ylim = c(0,300),
     xlab = "Number of Meals consumed", ylab = "Number of Students",
     main = "Change in Weight Vs Meals consumed")
lines(Increasedweight, type = "o",pch=13, col = "darkgreen")
lines(Remainconstantweight, type = "o", pch=15,col = "blue")
legend(x = "topright", pch=c(20,13,15), cex = 0.9,
       col= c("red","darkgreen","blue"),text.col = "black", 
       legend=c("Decrease in weight", "Increase in weight", "Remain Constant"))

#Plot 10 - Health issues vs Type of Population
Healthissueduringlockdown <- c("Yes", "No")
AdultHealth <- c(148,732)
AdolescentHealth <- c(12,246)
Childhealth <- c(1,43)

Typeofpopulationforhealth <- c("Child", "Adolescent", "Adult")
PresenceofHealthissue <- c(1,12,148)
Healthissuenotpresent <- c(43,246,732)
barplot(cbind(PresenceofHealthissue,Healthissuenotpresent)~Typeofpopulationforhealth,
        ylab= "Number of Students", 
        ylim=c(0,1000), 
        col=c("brown","grey"),
        legend.text = c("Health Issue Present", "No Health Issue"), las=1, 
        main="Health issue during lockdown")
library(ggplot2)
install.packages("webr")
library(webr)
library(dplyr)
Healthissueduringlockdown <- c("Health issue Present", "No Health issue")
Healthissue <- c(161,1021)
df <- data.frame(Healthissueduringlockdown,Healthissue)
df
PieDonut(df,aes(Healthissue,Healthissueduringlockdown), title="Health issue during lockdown")

install.packages("lessR")
library(lessR)
df <- rd(df)
PieChart(Healthissue, data = df,
         color = "black",
         lwd = 2,
         lty = 1,
         main = NULL)


library(tidyverse)
library(scales)

Healthissueduringlockdown <- c("Health issue Present", "No Health issue")
Healthissue <- c(161,1021)
df <- data.frame(Healthissueduringlockdown,Healthissue)
df
PieChart(Healthissueduringlockdown,Healthissue,
         main="Health issues faced by students during Lockdown",
         col.main="navyblue",
         cex.axis = 1)

#Plot 11 - Stress Busters 

par(mar=c(11,4,4,4))

cnt <- plyr::count(CovidImpactdataset$Stress.busters)
Stressbuster <- factor(CovidImpactdataset$Stress.busters, levels = cnt$x[order(cnt$freq, decreasing = FALSE)])
Stressbuster

Typeofpopulation <- as.factor(CovidImpactdataset$Type.of.Population)
Typeofpopulation

ggplot(data = CovidImpactdataset, aes(x = Stressbuster, fill = Typeofpopulation))+
  geom_bar(position='stack') + 
  scale_fill_brewer(palette="Reds") +
  ggtitle("Type of Stress Buster Activities") +
  labs(x="Activities", y="Number of Students") +
  theme(plot.title=element_text(hjust=0.5, face="bold",
                                size="12", color="navyblue"),
        axis.title=element_text(face="bold",
                                size=10, color="navyblue"),
        axis.text=element_text(face="italic", size=9,
                               color="black"),
        panel.background=element_rect(fill="white",
                                      color="black"),
        panel.grid.minor.x=element_blank(),
        legend.position="top") + coord_flip()

#Plot 12 - What you missed the most

missingmost <- as.factor(CovidImpactdataset$What.you.miss.the.most)
missingmost
plot(missingmost)

Typeofpopulation <- as.factor(CovidImpactdataset$Type.of.Population)
Typeofpopulation
par(mar=c(15,11,15,11))

ggplot(data = CovidImpactdataset, aes(x = missingmost, fill = Typeofpopulation))+
  geom_bar(position='stack') + 
  scale_fill_brewer(palette="Greens") +
  ggtitle("Activities missed during Covid") +
  labs(x="Activities",y="Number of Students") +
  theme(plot.title=element_text(hjust=0.5, face="bold",
                                size="12", color="navyblue"),
        axis.title=element_text(face="bold",
                                size=10, color="navyblue"),
        axis.text=element_text(face="italic", size=9,
                               color="black"),
        panel.background=element_rect(fill="white",
                                      color="black"),
        panel.grid.minor.x=element_blank(),
        legend.position="top") + coord_flip()

#Statistical Data - Child, Adult, Adolescent
#To extract Child data
Childdata <- CovidImpactdataset[CovidImpactdataset$Type.of.Population == "Child", ] 
Childdata       #to view the extracted data in the assigned variable
summary(Childdata)
str(Childdata)
range(Childdata$Age.of.Subject)
var(Childdata$Age.of.Subject)
kurtosis(Childdata$Age.of.Subject)
skewness(Childdata$Age.of.Subject)
outlier(Childdata$Age.of.Subject)

#To extract Adult data
as.factor(CovidImpactdataset$Type.of.Population)
Adultdata <- CovidImpactdataset[CovidImpactdataset$Type.of.Population != "Child", ]
Adultdata
Adultdata <- Adultdata[Adultdata$Type.of.Population != "Adolescents", ]
Adultdata      #to view the extracted data in the assigned variable
summary(Adultdata)
str(Adultdata)
range(Adultdata$Age.of.Subject)
var(Adultdata$Age.of.Subject)
kurtosis(Adultdata$Age.of.Subject)
skewness(Adultdata$Age.of.Subject)
outlier(Adultdata$Age.of.Subject)

#To extract Adolescents data
Adolescentsdata <- CovidImpactdataset[CovidImpactdataset$Type.of.Population == "Adolescents", ] 
Adolescentsdata      #to view the extracted data in the assigned variable
summary(Adolescentsdata)
str(Adolescentsdata)
range(Adolescentsdata$Age.of.Subject)
var(Adolescentsdata$Age.of.Subject)
kurtosis(Adolescentsdata$Age.of.Subject)
skewness(Adolescentsdata$Age.of.Subject)
outlier(Adolescentsdata$Age.of.Subject)


#Age for Child, Adults, Adolescents

#Age for Children
par()
par(mar = c(7, 6, 7, 6))
plot(density(Childdata$Age.of.Subject), xlab="Age(in years)", main="Age(yrs) of Children")
polygon(density(Childdata$Age.of.Subject), col="lightyellow", border="black")
abline(v=mean(Childdata$Age.of.Subject),col="red", lwd = 3)
abline(v=median(Childdata$Age.of.Subject),col="blue", lwd=2)
legend(x = "topleft", lwd=c(3,2), cex = 0.9,
       col= c("red","blue"),text.col = "black", 
       legend=c("Mean", "Median"))

#Age for Adult
par()
par(mar = c(7, 6, 7, 6))
plot(density(Adultdata$Age.of.Subject), xlab="Age(in years)", main="Age(yrs) of Adults")
polygon(density(Adultdata$Age.of.Subject), col="pink", border="black")
abline(v=mean(Adultdata$Age.of.Subject),col="red", lwd = 3)
abline(v=median(Adultdata$Age.of.Subject),col="blue", lwd=2)
legend(x = "topright", lwd=c(3,2), cex = 0.9,
       col= c("red","blue"),text.col = "black", 
       legend=c("Mean", "Median"))


#Age for Adolescents
par()
par(mar = c(7, 6, 7, 6))
plot(density(Adolescentsdata$Age.of.Subject), xlab="Age(in years)", main="Age(yrs) of Adolescents")
polygon(density(Adolescentsdata$Age.of.Subject), col="lightblue", border="black")
abline(v=mean(Adolescentsdata$Age.of.Subject),col="red", lwd = 3)
abline(v=median(Adolescentsdata$Age.of.Subject),col="blue", lwd=2)
legend(x = "topleft", lwd=c(3,2), cex = 0.9,
       col= c("red","blue"),text.col = "black", 
       legend=c("Mean", "Median"))

