#########################################
#### MSIS 2506- Fall 2019- Project 2 ####
## Submission by : ##                   
### Lahari Kuchibhotla ##               
### Stuti Sanghavi ###                
### Suchita Negi ###          
### Tanmayi Varanasi ###
##########################################
library(dplyr)
library(ggplot2)
#install.packages('ggthemes') 
###remove the comment above and install ggthemes package if the not already installed
library(ggthemes)

Deaths <- read.csv("C://Users//abc//Desktop//R//Accidental_Drug_Related_Deaths_2012-2018.csv", stringsAsFactors = FALSE)

###Interesting Finding 1###

##Part A - Increase over the Years Plot##

#Cleaning Date and Extracting years from date#

Year_of_death <- as.numeric(substring(trimws(Deaths$Date), 7, 10))

#Add the newly cleaned column to the dataset Deaths

Deaths <- cbind(Deaths, Year_of_death)

#Manipulation of Data

Summarized_data <- Deaths %>%
                      filter(Year_of_death != "") %>%
                      filter(Sex != "") %>%
                        group_by(Year_of_death, Sex) %>%
                         summarize(No_of_deaths = n())

#Plotting Data

ggplot(Summarized_data, aes(x = Year_of_death, y = No_of_deaths, fill = Sex)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Increase in number of deaths over years across Gender') + 
  theme(plot.title = element_text(size = 12.5, face = 'bold',margin = margin(10,0,10,0),color = 'darkblue'),axis.title.x = element_text(color = 'darkblue', vjust = -0.35),axis.title.y = element_text(color = 'darkblue',vjust = 0.35)) +
  theme_few()

#### Part B - Increase of drugs over the months####

#Cleaning Date and Extracting month from date

Month_of_death <- substring(trimws(Deaths$Date), 1, 2)

#Add the newly cleaned column to the dataset Deaths

Deaths <- cbind(Deaths, Month_of_death)

#Data Manipulation

Summarized_month_data <- Deaths %>%
  filter(Month_of_death != "") %>%
  filter(Sex != "") %>%
  group_by(Month_of_death, Sex) %>%
  summarize(No_of_deaths = n())

#Plotting Data

ggplot(Summarized_month_data, aes(x = Month_of_death, y = No_of_deaths, fill = Sex)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Number of deaths over months across Gender') + 
  theme(plot.title = element_text(size = 12.5, face = 'bold',margin = margin(10,0,10,0),color = 'darkblue'),axis.title.x = element_text(color = 'darkblue', vjust = -0.35),axis.title.y = element_text(color = 'darkblue',vjust = 0.35)) +
  theme_few()



## Fact 2 - Deaths by looking at Demographics ## 

tb1 <- table( Deaths$Race, Deaths$Sex, Deaths$Cocaine)
names(dimnames(tb1)) <- c("", "Deaths caused by Cocaine")
tb2 <- table(Deaths$Race, Deaths$Sex, Deaths$Heroin)
names(dimnames(tb2)) <- c("", "Deaths caused by Heroin")


hist(Deaths[Deaths$Race =="White" & Deaths$Sex == "Male" & Deaths$Cocaine == "Y",]$Age, main = "White Men Overdosing on Cocaine", xlab = "Age", col = "green")
hist(Deaths[Deaths$Race =="White" & Deaths$Sex == "Female" & Deaths$Cocaine == "Y",]$Age, main = "White Women Overdosing on Cocaine", xlab = "Age", col = "purple")

hist(Deaths[Deaths$Race =="White" & Deaths$Sex == "Male" & Deaths$Heroin== "Y",]$Age, main = "White Men Overdosing on Heroin", xlab = "Age", col = "green")
hist(Deaths[Deaths$Race =="White" & Deaths$Sex == "Female" & Deaths$Cocaine == "Y",]$Age, main = "White Women Overdosing on Heroin", xlab = "Age", col ="purple")


## Fact 3 - Deaths due to drugs##

###Tanmayi add ur code here##
##Part A - Displaying victims of various age groups that are affected by drugs##
install.packages("formattable")
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(gridExtra)
library(grid)

#cocaine victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,22)]
newDataCocaine = my_data[my_data$Cocaine == 'Y',] #selecting rows with value 'Y' for column Cocaine
ages1 <- as.vector(newDataCocaine$Age)
numberOfVictimsUnder20 = 0
numberOfVictimsBetween20_35 = 0
numberOfVictimsBetween35_60 = 0
numberOfVictimsAbove60 = 0

#Count victims under 50
for (i in ages1){
  if(!is.na(i) && i < 20){
    numberOfVictimsUnder20 <- numberOfVictimsUnder20 + 1
  }else if( !is.na(i) && i >= 20 && i<35){
    numberOfVictimsBetween20_35 <- numberOfVictimsBetween20_35 + 1
  }else if(!is.na(i) && i >= 35 && i<60){
    numberOfVictimsBetween35_60 <- numberOfVictimsBetween35_60 + 1
  }else if(!is.na(i) && i >= 60){
    numberOfVictimsAbove60 <- numberOfVictimsAbove60 + 1
  }
}
#_____________________________________________________________________________________________________________________
#Heroin victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,21)]
newDataHeroin = my_data[my_data$Heroin == 'Y',]
ages2 <- as.vector(newDataHeroin$Age)
numberOfHeroinVictimsUnder20 = 0
numberOfHeroinVictimsBetween20_35 = 0
numberOfHeroinVictimsBetween35_60 = 0
numberOfHeroinVictimsAbove60 = 0

#Count victims under 50
for (j in ages2){
  if(!is.na(j) && j < 20){
    numberOfHeroinVictimsUnder20 <- numberOfHeroinVictimsUnder20 + 1
  }else if( !is.na(j) && j >= 20 && j<35){
    numberOfHeroinVictimsBetween20_35 <- numberOfHeroinVictimsBetween20_35 + 1
  }else if(!is.na(j) && j >= 35 && j<60){
    numberOfHeroinVictimsBetween35_60 <- numberOfHeroinVictimsBetween35_60 + 1
  }else if(!is.na(j) && j >= 60){
    numberOfHeroinVictimsAbove60 <- numberOfHeroinVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Fentanyl victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,23)]
newDataFentanyl = my_data[my_data$Fentanyl == 'Y',]
ages3 <- as.vector(newDataFentanyl$Age)
numberOfFentanylVictimsUnder20 = 0
numberOfFentanylVictimsBetween20_35 = 0
numberOfFentanylVictimsBetween35_60 = 0
numberOfFentanylVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfFentanylVictimsUnder20 <- numberOfFentanylVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfFentanylVictimsBetween20_35 <- numberOfFentanylVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfFentanylVictimsBetween35_60 <- numberOfFentanylVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfFentanylVictimsAbove60 <- numberOfFentanylVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#FentanylAnalogue victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,24)]
newDataFentanylAnalogue = my_data[my_data$FentanylAnalogue == 'Y',]
ages3 <- as.vector(newDataFentanylAnalogue$Age)
numberOfFentanylAnalogueVictimsUnder20 = 0
numberOfFentanylAnalogueVictimsBetween20_35 = 0
numberOfFentanylAnalogueVictimsBetween35_60 = 0
numberOfFentanylAnalogueVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfFentanylAnalogueVictimsUnder20 <- numberOfFentanylAnalogueVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfFentanylAnalogueVictimsBetween20_35 <- numberOfFentanylAnalogueVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfFentanylAnalogueVictimsBetween35_60 <- numberOfFentanylAnalogueVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfFentanylAnalogueVictimsAbove60 <- numberOfFentanylAnalogueVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Oxycodone victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,25)]
newDataOxycodone = my_data[my_data$Oxycodone == 'Y',]
ages3 <- as.vector(newDataOxycodone$Age)
numberOfOxycodoneVictimsUnder20 = 0
numberOfOxycodoneVictimsBetween20_35 = 0
numberOfOxycodoneVictimsBetween35_60 = 0
numberOfOxycodoneVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfOxycodoneVictimsUnder20 <- numberOfOxycodoneVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfOxycodoneVictimsBetween20_35 <- numberOfOxycodoneVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfOxycodoneVictimsBetween35_60 <- numberOfOxycodoneVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfOxycodoneVictimsAbove60 <- numberOfOxycodoneVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Oxymorphone victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,26)]
newDataOxymorphone = my_data[my_data$Oxymorphone == 'Y',]
ages3 <- as.vector(newDataOxymorphone$Age)
numberOfOxymorphoneVictimsUnder20 = 0
numberOfOxymorphoneVictimsBetween20_35 = 0
numberOfOxymorphoneVictimsBetween35_60 = 0
numberOfOxymorphoneVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfOxymorphoneVictimsUnder20 <- numberOfOxymorphoneVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfOxymorphoneVictimsBetween20_35 <- numberOfOxymorphoneVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfOxymorphoneVictimsBetween35_60 <- numberOfOxymorphoneVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfOxymorphoneVictimsAbove60 <- numberOfOxymorphoneVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Ethanol victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,27)]
newDataEthanol = my_data[my_data$Ethanol == 'Y',]
ages3 <- as.vector(newDataEthanol$Age)
numberOfEthanolVictimsUnder20 = 0
numberOfEthanolVictimsBetween20_35 = 0
numberOfEthanolVictimsBetween35_60 = 0
numberOfEthanolVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfEthanolVictimsUnder20 <- numberOfEthanolVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfEthanolVictimsBetween20_35 <- numberOfEthanolVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfEthanolVictimsBetween35_60 <- numberOfEthanolVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfEthanolVictimsAbove60 <- numberOfEthanolVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Hydrocodone victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,28)]
newDataHydrocodone = my_data[my_data$Hydrocodone == 'Y',]
ages3 <- as.vector(newDataHydrocodone$Age)
numberOfHydrocodoneVictimsUnder20 = 0
numberOfHydrocodoneVictimsBetween20_35 = 0
numberOfHydrocodoneVictimsBetween35_60 = 0
numberOfHydrocodoneVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfHydrocodoneVictimsUnder20 <- numberOfHydrocodoneVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfHydrocodoneVictimsBetween20_35 <- numberOfHydrocodoneVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfHydrocodoneVictimsBetween35_60 <- numberOfHydrocodoneVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfHydrocodoneVictimsAbove60 <- numberOfHydrocodoneVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Benzodiazepine victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,29)]
newDataBenzodiazepine = my_data[my_data$Benzodiazepine == 'Y',]
ages3 <- as.vector(newDataBenzodiazepine$Age)
numberOfBenzodiazepineVictimsUnder20 = 0
numberOfBenzodiazepineVictimsBetween20_35 = 0
numberOfBenzodiazepineVictimsBetween35_60 = 0
numberOfBenzodiazepineVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfBenzodiazepineVictimsUnder20 <- numberOfBenzodiazepineVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfBenzodiazepineVictimsBetween20_35 <- numberOfBenzodiazepineVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfBenzodiazepineVictimsBetween35_60 <- numberOfBenzodiazepineVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfBenzodiazepineVictimsAbove60 <- numberOfBenzodiazepineVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Methadone victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,30)]
newDataMethadone = my_data[my_data$Methadone == 'Y',]
ages3 <- as.vector(newDataMethadone$Age)
numberOfMethadoneVictimsUnder20 = 0
numberOfMethadoneVictimsBetween20_35 = 0
numberOfMethadoneVictimsBetween35_60 = 0
numberOfMethadoneVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfMethadoneVictimsUnder20 <- numberOfMethadoneVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfMethadoneVictimsBetween20_35 <- numberOfMethadoneVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfMethadoneVictimsBetween35_60 <- numberOfMethadoneVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfMethadoneVictimsAbove60 <- numberOfMethadoneVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Amphet victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,31)]
newDataAmphet = my_data[my_data$Amphet == 'Y',]
ages3 <- as.vector(newDataAmphet$Age)
numberOfAmphetVictimsUnder20 = 0
numberOfAmphetVictimsBetween20_35 = 0
numberOfAmphetVictimsBetween35_60 = 0
numberOfAmphetVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfAmphetVictimsUnder20 <- numberOfAmphetVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfAmphetVictimsBetween20_35 <- numberOfAmphetVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfAmphetVictimsBetween35_60 <- numberOfAmphetVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfAmphetVictimsAbove60 <- numberOfAmphetVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Tramad victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,32)]
newDataTramad = my_data[my_data$Tramad == 'Y',]
ages3 <- as.vector(newDataTramad$Age)
numberOfTramadVictimsUnder20 = 0
numberOfTramadVictimsBetween20_35 = 0
numberOfTramadVictimsBetween35_60 = 0
numberOfTramadVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfTramadVictimsUnder20 <- numberOfTramadVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfTramadVictimsBetween20_35 <- numberOfTramadVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfTramadVictimsBetween35_60 <- numberOfTramadVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfTramadVictimsAbove60 <- numberOfTramadVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Morphine_NotHeroin victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,33)]
newDataMorphine_NotHeroin = my_data[my_data$Morphine_NotHeroin == 'Y',]
ages3 <- as.vector(newDataMorphine_NotHeroin$Age)
numberOfMorphine_NotHeroinVictimsUnder20 = 0
numberOfMorphine_NotHeroinVictimsBetween20_35 = 0
numberOfMorphine_NotHeroinVictimsBetween35_60 = 0
numberOfMorphine_NotHeroinVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfMorphine_NotHeroinVictimsUnder20 <- numberOfMorphine_NotHeroinVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfMorphine_NotHeroinVictimsBetween20_35 <- numberOfMorphine_NotHeroinVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfMorphine_NotHeroinVictimsBetween35_60 <- numberOfMorphine_NotHeroinVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfMorphine_NotHeroinVictimsAbove60 <- numberOfMorphine_NotHeroinVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#Hydromorphone victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,34)]
newDataHydromorphone = my_data[my_data$Hydromorphone == 'Y',]
ages3 <- as.vector(newDataHydromorphone$Age)
numberOfHydromorphoneVictimsUnder20 = 0
numberOfHydromorphoneVictimsBetween20_35 = 0
numberOfHydromorphoneVictimsBetween35_60 = 0
numberOfHydromorphoneVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfHydromorphoneVictimsUnder20 <- numberOfHydromorphoneVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfHydromorphoneVictimsBetween20_35 <- numberOfHydromorphoneVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfHydromorphoneVictimsBetween35_60 <- numberOfHydromorphoneVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfHydromorphoneVictimsAbove60 <- numberOfHydromorphoneVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#OpiateNOS victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,36)]
newDataOpiateNOS = my_data[my_data$OpiateNOS == 'Y',]
ages3 <- as.vector(newDataOpiateNOS$Age)
numberOfOpiateNOSVictimsUnder20 = 0
numberOfOpiateNOSVictimsBetween20_35 = 0
numberOfOpiateNOSVictimsBetween35_60 = 0
numberOfOpiateNOSVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfOpiateNOSVictimsUnder20 <- numberOfOpiateNOSVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfOpiateNOSVictimsBetween20_35 <- numberOfOpiateNOSVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfOpiateNOSVictimsBetween35_60 <- numberOfOpiateNOSVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfOpiateNOSVictimsAbove60 <- numberOfOpiateNOSVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
#AnyOpiod victims
my_data = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")[ ,c(4,37)]
newDataAnyOpioid = my_data[my_data$AnyOpioid == 'Y',]
ages3 <- as.vector(newDataAnyOpioid$Age)
numberOfAnyOpioidVictimsUnder20 = 0
numberOfAnyOpioidVictimsBetween20_35 = 0
numberOfAnyOpioidVictimsBetween35_60 = 0
numberOfAnyOpioidVictimsAbove60 = 0

#Count victims under 50
for (k in ages3){
  if(!is.na(k) && k< 20){
    numberOfAnyOpioidVictimsUnder20 <- numberOfAnyOpioidVictimsUnder20 + 1
  }else if( !is.na(k) && k>= 20 && k<35){
    numberOfAnyOpioidVictimsBetween20_35 <- numberOfAnyOpioidVictimsBetween20_35 + 1
  }else if(!is.na(k) && k>= 35 && k<60){
    numberOfAnyOpioidVictimsBetween35_60 <- numberOfAnyOpioidVictimsBetween35_60 + 1
  }else if(!is.na(k) && k>= 60){
    numberOfAnyOpioidVictimsAbove60 <- numberOfAnyOpioidVictimsAbove60 + 1
  }
}
#------------------------------------------------------------------------------------------------------------------------------------
# Creating a Table to store the above data
drugDeathDataTable <- matrix(c(numberOfVictimsUnder20,numberOfVictimsBetween20_35,numberOfVictimsBetween35_60, numberOfVictimsAbove60, numberOfHeroinVictimsUnder20, numberOfHeroinVictimsBetween20_35, numberOfHeroinVictimsBetween35_60, numberOfHeroinVictimsAbove60, numberOfFentanylVictimsUnder20, numberOfFentanylVictimsBetween20_35, numberOfFentanylVictimsBetween35_60, numberOfFentanylVictimsAbove60,numberOfFentanylAnalogueVictimsUnder20, numberOfFentanylAnalogueVictimsBetween20_35, numberOfFentanylAnalogueVictimsBetween35_60, numberOfFentanylAnalogueVictimsAbove60,numberOfOxycodoneVictimsUnder20, numberOfOxycodoneVictimsBetween20_35, numberOfOxycodoneVictimsBetween35_60, numberOfOxycodoneVictimsAbove60,numberOfOxymorphoneVictimsUnder20, numberOfOxymorphoneVictimsBetween20_35, numberOfOxymorphoneVictimsBetween35_60, numberOfOxymorphoneVictimsAbove60,numberOfEthanolVictimsUnder20, numberOfEthanolVictimsBetween20_35, numberOfEthanolVictimsBetween35_60, numberOfEthanolVictimsAbove60,numberOfHydrocodoneVictimsUnder20, numberOfHydrocodoneVictimsBetween20_35, numberOfHydrocodoneVictimsBetween35_60, numberOfHydrocodoneVictimsAbove60,numberOfBenzodiazepineVictimsUnder20, numberOfBenzodiazepineVictimsBetween20_35, numberOfBenzodiazepineVictimsBetween35_60, numberOfBenzodiazepineVictimsAbove60,numberOfMethadoneVictimsUnder20, numberOfMethadoneVictimsBetween20_35, numberOfMethadoneVictimsBetween35_60, numberOfMethadoneVictimsAbove60,numberOfAmphetVictimsUnder20, numberOfAmphetVictimsBetween20_35, numberOfAmphetVictimsBetween35_60, numberOfAmphetVictimsAbove60,numberOfTramadVictimsUnder20, numberOfTramadVictimsBetween20_35, numberOfTramadVictimsBetween35_60, numberOfTramadVictimsAbove60,numberOfMorphine_NotHeroinVictimsUnder20, numberOfMorphine_NotHeroinVictimsBetween20_35, numberOfMorphine_NotHeroinVictimsBetween35_60, numberOfMorphine_NotHeroinVictimsAbove60,numberOfHydromorphoneVictimsUnder20, numberOfHydromorphoneVictimsBetween20_35, numberOfHydromorphoneVictimsBetween35_60, numberOfHydromorphoneVictimsAbove60,numberOfOpiateNOSVictimsUnder20, numberOfOpiateNOSVictimsBetween20_35, numberOfOpiateNOSVictimsBetween35_60, numberOfOpiateNOSVictimsAbove60, numberOfAnyOpioidVictimsUnder20, numberOfAnyOpioidVictimsBetween20_35, numberOfAnyOpioidVictimsBetween35_60, numberOfAnyOpioidVictimsAbove60), ncol=4, byrow=TRUE)
colnames(drugDeathDataTable) <- c('Teenagers', 'Young Adults', 'Adults', 'Senior citizens')
rownames(drugDeathDataTable) <- c('Cocaine','Heroin','Fentanyl','FentanylAnalogue','Oxycodone','Oxymorphone','Ethanol','Hydrocodone','Benzodiazepine','Methadone','Amphet','Tramad','Morphine_NotHeroin','Hydromorphone','OpiateNOS','AnyOpioid')
drugDeathDataTable.table <- as.table(drugDeathDataTable)
finalTable <- drugDeathDataTable.table
finalTable

#styling the table
grid.table(finalTable)

#dotchart to represent the table
dotchart(finalTable, color=c("red","blue","limegreen","forestgreen","orange","darkgrey","deeppink3","black","purple","darkblue","tomato","slategray4","red4","plum3","navy","darkgoldenrod4"),
         main="Accidental drug related deaths sorted by age group",xlab = "Number of deaths",cex=0.6, pch = 19,lcolor="black")


##Part B - Drug related deaths over the years ##
#Defining each drug for identifying a pattern
Fentanyl <- Deaths %>%
  filter(Year_of_death != "") %>%
  filter(Fentanyl != "") %>%
  group_by(Year_of_death) %>%
  summarize(Drug_deaths = n()) %>%
  mutate(Drug = "Fentanyl")

Heroin <- Deaths %>%
  filter(Year_of_death != "") %>%
  filter(Heroin != "") %>%
  group_by(Year_of_death) %>%
  summarize(Drug_deaths = n()) %>%
  mutate(Drug = "Heroin")


Cocaine <- Deaths %>%
  filter(Year_of_death != "") %>%
  filter(Cocaine != "") %>%
  group_by(Year_of_death) %>%
  summarize(Drug_deaths = n()) %>%
  mutate(Drug = "Cocaine")


Drug_related_death <- rbind(Fentanyl, Heroin, Cocaine)

#Plotting Data
ggplot(Drug_related_death, aes(x = Year_of_death, y = Drug_deaths, color = Drug)) + 
  geom_line() + 
  ggtitle('Number of Drug Deaths over the Years by Drug Type') + 
  theme(plot.title = element_text(size = 12.5, face = 'bold',margin = margin(10,0,10,0),color = 'darkblue'),axis.title.x = element_text(color = 'darkblue', vjust = -0.35),axis.title.y = element_text(color = 'darkblue',vjust = 0.5)) +
  theme_few()


