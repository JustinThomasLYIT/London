# Q1 Read in dataset called london_crime

london_crime <- read.csv("london-crime-data.csv", na="")

# show structure
str(london_crime)

# create new Date variable using Paste
london_crime$Date <- paste("1",london_crime$month, london_crime$year, sep="/")
datetemp <- as.Date(london_crime$Date, "%d/%m/%Y")
datetemp
class(datetemp)
london_crime$Date <- datetemp
str(london_crime)


# Q2 Keep only some variable names and rename them
include_list <- names(london_crime) %in% c("borough", "major_category", "minor_category", "value", "Date")
include_list

london_crime <- london_crime[(include_list)]
london_crime
str(london_crime)

# Add column names to the london_crime dataframe

column_names <- c("Borough", 
                  "MajorCategory", 
                  "SubCategory", 
                  "Value", 
                  "CrimeDate")
colnames(london_crime) <- column_names
str(london_crime)

#Q3 Convert crimedate so its variable of type Date

str(london_crime$CrimeDate)

# already did this in Q1. Crimedate is in date format


# Q4 Plot a chart to show summary of the borough info so we can see where most
# crimes occur. Answer 2 questions based on this.

boroughfactor <- factor(london_crime$Borough)

plot(boroughfactor, main="Borough Crimes", xlab = "Borough", ylab = "Crime Count")

summary(boroughfactor)

# Croydon has the highest level of crime with 5226 counts

# City of London has the lowest level of crime with 86 counts


# Q5 Display MajorCategory data in a pie chart and answer 2 questions

categoryfactor <- factor(london_crime$MajorCategory)

pie(summary(categoryfactor), main="Pie Chart of Crime Categories")
summary(categoryfactor)

# Theft and Handling had the highest level of crimes at  33759

# Sexual Offences had the lowest level of offences at 917




# Q6  Categorise each borough in dataset into general area it lies in London

str(london_crime)

london_crime$Region[london_crime$Borough == "Barking and Dagenham"] <- "East"
london_crime$Region[london_crime$Borough == "Barnet"] <- "North"
london_crime$Region[london_crime$Borough == "Bexley"] <- "East"
london_crime$Region[london_crime$Borough == "Brent"] <- "West"
london_crime$Region[london_crime$Borough == "Bromley"] <- "South"
london_crime$Region[london_crime$Borough == "Camden"] <- "North"
london_crime$Region[london_crime$Borough == "Croydon"] <- "South"
london_crime$Region[london_crime$Borough == "Ealing"] <- "West"
london_crime$Region[london_crime$Borough == "Enfield"] <- "North"
london_crime$Region[london_crime$Borough == "Greenwich"] <- "East"
london_crime$Region[london_crime$Borough == "Hackney"] <- "North"
london_crime$Region[london_crime$Borough == "Hammersmith and Fulham"] <- "West"
london_crime$Region[london_crime$Borough == "Haringey"] <- "North"
london_crime$Region[london_crime$Borough == "Harrow"] <- "West"
london_crime$Region[london_crime$Borough == "Havering"] <- "East"
london_crime$Region[london_crime$Borough == "Hillingdon"] <- "West"
london_crime$Region[london_crime$Borough == "Hounslow"] <- "West"
london_crime$Region[london_crime$Borough == "Islington"] <- "Central"
london_crime$Region[london_crime$Borough == "Kensington and Chelsea"] <- "Central"
london_crime$Region[london_crime$Borough == "Kingston upon Thames"] <- "East"
london_crime$Region[london_crime$Borough == "Lambeth"] <- "Central"
london_crime$Region[london_crime$Borough == "Lewisham"] <- "Central"
london_crime$Region[london_crime$Borough == "Merton"] <- "South"
london_crime$Region[london_crime$Borough == "Newham"] <- "East"
london_crime$Region[london_crime$Borough == "Redbridge"] <- "East"
london_crime$Region[london_crime$Borough == "Richmond upon Thames"] <- "West"
london_crime$Region[london_crime$Borough == "Southwark"] <- "Central"
london_crime$Region[london_crime$Borough == "Sutton"] <- "South"
london_crime$Region[london_crime$Borough == "Tower Hamlets"] <- "Central"
london_crime$Region[london_crime$Borough == "Waltham Foest"] <- "Central"
london_crime$Region[london_crime$Borough == "Wandsworth"] <- "East"
london_crime$Region[london_crime$Borough == "Westminster"] <- "Central"

missing_data <- london_crime[is.na(london_crime$Region),]
missing_data

#mistyped walham forrest

london_crime$Region[london_crime$Borough == "Waltham Forest"] <- "Central"

missing_data <- london_crime[is.na(london_crime$Region),]
missing_data

# city of london is missing, assigned to central

london_crime$Region[london_crime$Borough == "City of London"] <- "Central"

missing_data <- london_crime[is.na(london_crime$Region),]
missing_data


# Q7 Display with region has highest crime rate

regionfactor <- factor(london_crime$Region)

plot(regionfactor, main="Region Crimes", xlab = "Region", ylab = "Crime Count")

summary(regionfactor)


# Central has most number of crimes at 28591

# South has lowest number of crimes at 15487



# Q8 Extract highest and lowest subsets of data

HighSubsetLondonCrime <- subset(london_crime, london_crime$Region == "Central")

LowSubsetLondonCrime <- subset(london_crime, london_crime$Region == "South")

summary(factor(HighSubsetLondonCrime$MajorCategory))
# Theft and Handling is the highest and sexual offences are the lowest in Regioin Central


summary(factor(LowSubsetLondonCrime$MajorCategory))
# Theft and Handling is the highest and sexual offences are the lowest in Regioin south also


# Q9 plot content of both data frames side by side

CentralInfo <- (factor(HighSubsetLondonCrime$MajorCategory))
SouthInfo <- (factor(LowSubsetLondonCrime$MajorCategory))

# save par defaults in case you need to revert
opar <- par()

# 2 columns
par(mfrow=c(1,2))
#plot the graphs, with labels, same y axis and vertical x labels

plot(CentralInfo,main="Central Crimes", xlab = "Categories", ylab = "Crime Count", ylim = c(0,9000), las=2)
plot(SouthInfo,main="South Crimes", xlab = "Categories", ylab = "Crime Count", ylim = c(0,9000), las=2)


# Q10 save files

write.csv(london_crime, file = "london-crime-modified.csv") 



